import logging
import os
import glob
import json
from typing import List, Dict, Any
from sqlalchemy.orm import Session
from app.core.ai_client import AIClient
from app.core.config import settings
from app.repositories.migration_repository import MigrationRepository
from app.services.agent_registry import AgentRegistry
from app.agents.base import AgentContext, AgentStatus

import time

logger = logging.getLogger(__name__)

class MigrationOrchestrator:
    def __init__(self, db: Session):
        self.db = db
        self.repo = MigrationRepository(db)
        self.ai_client = AIClient()

    def start_migration_sync(self, source_folder: str, run_id: int):
        import asyncio
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(self.start_migration(source_folder, run_id))
        finally:
            loop.close()

    async def start_migration(self, source_folder: str = "data/cobol_source", run_id: int = None):
        logger.info(f"=== Starting agentic migration flow for {source_folder} ===")
        
        if not run_id:
            run = self.repo.create_run(cobol_source_path=source_folder)
            run_id = run.id
        else:
            run = self.repo.get_run(run_id)
        
        logger.info(f"Run ID: {run_id}")
        
        files = self._scan_files(source_folder)
        logger.info(f"Scanned files: {files}")
        if not files:
            logger.warning("No files found in source folder")
            self.repo.complete_run(run_id, "Completed", "No files found")
            return run

        cobol_files_map = {}
        project_root = os.getcwd()
        scan_base_path = os.path.join(project_root, source_folder)

        for file_path in files:
            try:
                with open(file_path, "r", encoding="utf-8") as f:
                    content = f.read()
            except Exception as e:
                logger.error(f"Failed to read {file_path}: {e}")
                continue
            
            relative_path = os.path.relpath(file_path, start=scan_base_path)
            cobol_files_map[relative_path] = content

        logger.info(f"Loaded {len(cobol_files_map)} files. Resolving agent execution order...")

        all_agents = AgentRegistry.get_all_agents()
        auto_run_agents = AgentRegistry.get_auto_run_agents()
        non_auto_run_agents = [a for a in all_agents if a not in auto_run_agents]
        logger.info(f"All registered agents: {all_agents}")
        logger.info(f"Auto-run agents: {auto_run_agents}")
        logger.info(f"Non-auto-run agents: {non_auto_run_agents}")
        
        execution_order = AgentRegistry.resolve_execution_order_for_auto_run()
        logger.info(f"Agent Execution Order (auto_run only): {execution_order}")
        
        for agent_name in execution_order:
            self.repo.create_agent_result(run_id, agent_name, "PENDING")
        
        for agent_name in non_auto_run_agents:
            logger.info(f"Creating COMPLETED result for non-auto-run agent: {agent_name}")
            self.repo.create_agent_result(run_id, agent_name, "COMPLETED")
            self.repo.update_agent_result(run_id, agent_name, "COMPLETED", data={"skipped": True, "reason": "Not auto-run agent"}, progress=100, current_action="Skipped - on-demand only")
        
        agent_results_cache = {}
        execution_levels = self._group_by_execution_level(execution_order)
        logger.info(f"Execution levels: {execution_levels}")

        for level_idx, level_agents in enumerate(execution_levels):
            import asyncio
            logger.info(f"=== Starting Level {level_idx} with {len(level_agents)} agents in PARALLEL: {level_agents} ===")
            tasks = []
            
            for agent_name in level_agents:
                task = self._execute_agent(
                    agent_name=agent_name,
                    run_id=run_id,
                    cobol_files_map=cobol_files_map,
                    agent_results_cache=agent_results_cache
                )
                tasks.append(task)
            
            logger.info(f"Executing {len(tasks)} tasks concurrently with asyncio.gather...")
            results = await asyncio.gather(*tasks, return_exceptions=True)
            logger.info(f"Level {level_idx} completed. Processing results...")
            
            for agent_name, result in zip(level_agents, results):
                if isinstance(result, Exception):
                    logger.error(f"Agent {agent_name} failed with exception: {result}")
                else:
                    agent_results_cache[agent_name] = result


        logger.info(f"=== Migration complete. Processed {len(cobol_files_map)} files ===")
        self.repo.complete_run(run_id, "Completed", f"Processed {len(cobol_files_map)} files using agents.")
        logger.info(f"Run {run_id} marked as completed")
        return run

    def rerun_migration_sync(self, run_id: int, agents_to_rerun: List[str], additional_context: Dict[str, Any] = None):
        import asyncio
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            loop.run_until_complete(self.rerun_migration(run_id, agents_to_rerun, additional_context))
        finally:
            loop.close()

    async def rerun_migration(self, run_id: int, agents_to_rerun: List[str], additional_context: Dict[str, Any] = None):
        logger.info(f"=== Rerunning agents for run {run_id}: {agents_to_rerun} ===")
        
        run = self.repo.get_run(run_id)
        if not run:
            raise ValueError(f"Run {run_id} not found")
        
        dependent_agents = AgentRegistry.get_dependent_agents(agents_to_rerun)
        all_agents_to_run = set(agents_to_rerun) | dependent_agents
        logger.info(f"Agents to rerun (including dependents): {all_agents_to_run}")
        
        persistent_agents = AgentRegistry.get_persistent_agents()
        self.repo.reset_agent_results(run_id, list(all_agents_to_run), exclude_agents=persistent_agents)
        
        cobol_files_map = {}
        if run.cobol_source_path:
            files = self._scan_files(run.cobol_source_path)
            project_root = os.getcwd()
            scan_base_path = os.path.join(project_root, run.cobol_source_path)
            for file_path in files:
                try:
                    with open(file_path, "r", encoding="utf-8") as f:
                        content = f.read()
                    relative_path = os.path.relpath(file_path, start=scan_base_path)
                    cobol_files_map[relative_path] = content
                except Exception as e:
                    logger.error(f"Failed to read {file_path}: {e}")
        
        execution_order = AgentRegistry.resolve_execution_order_for_agents(list(all_agents_to_run))
        logger.info(f"Rerun Execution Order: {execution_order}")
        
        agent_results_cache = self.repo.get_agent_results_data(run_id)
        execution_levels = self._group_by_execution_level(execution_order)
        
        for level_idx, level_agents in enumerate(execution_levels):
            import asyncio
            logger.info(f"=== Rerun Level {level_idx}: {level_agents} ===")
            tasks = []
            
            for agent_name in level_agents:
                task = self._execute_agent(
                    agent_name=agent_name,
                    run_id=run_id,
                    cobol_files_map=cobol_files_map,
                    agent_results_cache=agent_results_cache,
                    additional_context=additional_context or {}
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            for agent_name, result in zip(level_agents, results):
                if isinstance(result, Exception):
                    logger.error(f"Agent {agent_name} failed: {result}")
                else:
                    agent_results_cache[agent_name] = result
        
        logger.info(f"=== Rerun complete for run {run_id} ===")
        return {"status": "completed", "agents_executed": list(all_agents_to_run)}

    async def _execute_agent(self, agent_name: str, run_id: int, cobol_files_map: dict, agent_results_cache: dict, additional_context: dict = None):
        logger.info(f"Running Agent: {agent_name}")
        time.sleep(1)
        self.repo.update_agent_result(run_id, agent_name, "RUNNING", progress=0, current_action="Starting...")
        
        agent = AgentRegistry.get_agent(agent_name, self.ai_client)

        async def update_progress_callback(progress: int, current_action: str):
            self.repo.update_agent_result(run_id, agent_name, "RUNNING", progress=progress, current_action=current_action)

        context = AgentContext(
            run_id=run_id,
            db_session=self.db,
            files=cobol_files_map,
            previous_results=agent_results_cache,
            update_progress_callback=update_progress_callback,
            additional_call_context=additional_context or {}
        )
        agent.context = context
        
        logger.info(f"Agent {agent_name} processing {len(cobol_files_map)} files")
        result = await agent.run(context)
        logger.info(f"Agent {agent_name} result status: {result.status}")
        
        if result.status == AgentStatus.COMPLETED:
            logger.info(f"Saving result for {agent_name}")
            await agent.save_result(result, context)
            self.repo.update_agent_result(run_id, agent_name, "COMPLETED", data=result.data, progress=100, current_action="Completed")
            return result.data
        elif result.status == AgentStatus.FAILED:
            logger.error(f"Agent {agent_name} failed: {result.error}")
            self.repo.update_agent_result(run_id, agent_name, "FAILED", error=result.error, progress=0, current_action=f"Failed: {result.error}")
            return None
        else:
            logger.warning(f"Agent {agent_name} status: {result.status}")
            self.repo.update_agent_result(run_id, agent_name, result.status.value, progress=50)
            return None
    
    def _group_by_execution_level(self, execution_order: list) -> list:
        graph = {}
        for agent_name in execution_order:
            agent = AgentRegistry.get_agent(agent_name, None)
            graph[agent_name] = set(agent.dependencies)
            logger.info(f"Agent '{agent_name}' dependencies: {agent.dependencies}")
        
        levels = []
        processed = set()
        
        while len(processed) < len(execution_order):
            current_level = []
            for agent_name in execution_order:
                if agent_name not in processed:
                    deps = graph[agent_name]
                    # Only wait for dependencies that are actually part of this execution set
                    # Dependencies not in 'execution_order' are assumed to be already completed (external dependencies)
                    if all(dep in processed for dep in deps if dep in graph):
                        current_level.append(agent_name)
            
            if not current_level:
                logger.error(f"Circular dependency detected! Processed: {processed}, Remaining: {set(execution_order) - processed}")
                break
            
            logger.info(f"Level {len(levels)}: {current_level}")
            levels.append(current_level)
            processed.update(current_level)
        
        return levels

    def _scan_files(self, folder: str) -> list[str]:


        project_root = os.getcwd() 
        full_path = os.path.join(project_root, folder)
        cbl_files = glob.glob(os.path.join(full_path, "**", "*.cbl"), recursive=True)
        cpy_files = glob.glob(os.path.join(full_path, "**", "*.cpy"), recursive=True)
        return cbl_files + cpy_files
