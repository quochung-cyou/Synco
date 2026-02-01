import logging
from typing import List
from app.core.config import settings
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class StructureInsightsAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "structure_insights_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["copybook_usage_analyzer_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="copybook_usage_analyzer_agent",
                description="Requires copybook usage analysis",
                required=True
            )
        ]

    def __init__(self, ai_client):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("structure_analysis_system.txt")
        self.user_prompt_template = self.load_prompt("structure_analysis_user.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            usage_data = context.previous_results.get("copybook_usage_analyzer_agent", {})
            
            file_structure_summary = usage_data.get("file_structure_summary", "")
            usage_summary = usage_data.get("usage_summary", "")
            
            analysis_prompt = self.user_prompt_template.format(
                file_structure=file_structure_summary,
                copybook_usage=usage_summary
            )

            structure_response = await self.ai_client.call(
                prompt=analysis_prompt,
                system_prompt=self.system_prompt
            )
            
            logger.info("Generated structure insights")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "insights": structure_response.strip()
                },
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Error in StructureInsightsAgent: {e}")
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
                data=result.data,
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
