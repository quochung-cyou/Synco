import logging
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency
from app.repositories.migration_repository import MigrationRepository

logger = logging.getLogger(__name__)

class ContextExtractorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "context_extractor_agent"

    @property
    def auto_run(self) -> bool:
        return False

    @property
    def dependencies(self) -> List[str]:
        return []

    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return []

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            await self.update_progress(10, "Fetching completed agent results...")
            
            repo = MigrationRepository(context.db_session)
            agent_data = repo.get_agent_results_data(context.run_id)
            
            await self.update_progress(30, "Extracting relevant context...")
            
            extracted_context = {
                "run_id": context.run_id,
                "summary": {},
                "details": {}
            }
            
            if "dashboard_summary_agent" in agent_data:
                extracted_context["summary"] = agent_data["dashboard_summary_agent"]
            
            if "complexity_analyzer_agent" in agent_data:
                extracted_context["details"]["complexity"] = agent_data["complexity_analyzer_agent"]
            
            if "dependency_aggregator_agent" in agent_data:
                deps = agent_data["dependency_aggregator_agent"]
                dep_summary = {}
                for file_path, file_deps in deps.items():
                    dep_summary[file_path] = len(file_deps) if isinstance(file_deps, list) else 0
                extracted_context["details"]["dependencies"] = dep_summary
            
            if "structure_insights_agent" in agent_data:
                extracted_context["details"]["insights"] = agent_data["structure_insights_agent"]
            
            if "mermaid_diagram_agent" in agent_data:
                extracted_context["details"]["diagram"] = agent_data["mermaid_diagram_agent"]
            
            await self.update_progress(80, "Building context summary...")
            
            chat_history_data = agent_data.get("chat_history_agent", {})
            chat_history = chat_history_data.get("history", []) if chat_history_data else []
            extracted_context["chat_history"] = chat_history
            
            await self.update_progress(100, "Context extraction complete")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=extracted_context,
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Context extraction failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        pass
