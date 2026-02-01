import logging
import json
import os
from typing import List
from app.core.config import settings
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency


logger = logging.getLogger(__name__)


class CobolParserAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "cobol_parser_agent"

    @property
    def dependencies(self) -> List[str]:
        return []
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return []

    def __init__(self, ai_client):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("cobol_analysis_system.txt")
        self.user_prompt_template = self.load_prompt("cobol_analysis_user.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            results = {}
            cobol_files = {fp: content for fp, content in context.files.items() if fp.lower().endswith(".cbl")}
            
            if not cobol_files:
                logger.warning("No COBOL files found in context")
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={},
                    progress=100
                )
            
            total_files = len(cobol_files)
            processed = 0
            import asyncio
            semaphore = asyncio.Semaphore(5)

            async def process_file(file_path: str, content: str):
                nonlocal processed
                async with semaphore:
                    prompt = self.user_prompt_template.format(file_content=content)
                    response = await self.ai_client.call(
                        prompt=prompt,
                        system_prompt=self.system_prompt
                    )
                    processed += 1
                    current_loop_progress = int((processed / total_files * 100))
                    await self.update_progress(current_loop_progress, f"Parsing {os.path.basename(file_path)}...")
                    logger.info(f"Parsed {processed}/{total_files} files")
                    return file_path, response

            tasks = [process_file(file_path, content) for file_path, content in cobol_files.items()]
            results_list = await asyncio.gather(*tasks)
            
            for file_path, response in results_list:
                results[file_path] = {
                    "analysis": response,
                    "code": cobol_files[file_path]
                }
            
            progress = int((processed / total_files * 100)) if total_files > 0 else 100
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=results,
                progress=progress
            )
            
        except Exception as e:
            logger.error(f"Error in CobolParserAgent: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        from app.models.migration import AgentResult as AgentResultModel
        
        try:
            db = context.db_session
            
            agent_result = AgentResultModel(
                run_id=context.run_id,
                agent_name=self.name,
                status=result.status.value,
                file_path=None,
                data={"parsed_files": list(result.data.keys()) if result.data else []},
                error=result.error,
                execution_time=int(result.execution_time),
                progress=result.progress
            )
            
            db.add(agent_result)
            db.commit()
            logger.info(f"Saved agent result for {self.name}")
        except Exception as e:
            logger.error(f"Failed to save agent result for {self.name}: {e}")
            raise
