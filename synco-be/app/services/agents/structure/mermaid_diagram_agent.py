import logging
from typing import List
from app.core.config import settings
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class MermaidDiagramAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "mermaid_diagram_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["copybook_usage_analyzer_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="copybook_usage_analyzer_agent",
                description="Requires processed dependency data",
                required=True
            )
        ]

    def __init__(self, ai_client):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("structure_analysis_system.txt")
        self.mermaid_user_prompt_template = self.load_prompt("structure_analysis_mermaid.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            usage_data = context.previous_results.get("copybook_usage_analyzer_agent", {})
            
            # Get flattened dependencies passed through by CopybookUsageAnalyzer
            all_dependencies = usage_data.get("all_dependencies", [])
            total_progs = usage_data.get("total_programs", 0)
            total_cpys = usage_data.get("total_copybooks", 0)
            
            relationships = []
            for dep in all_dependencies:
                source = dep.get("source")
                target = dep.get("target")
                dep_type = dep.get("type")
                if source and target and dep_type:
                    relationships.append(f"{source} -> {target} ({dep_type})")
            
            relationships_str = "\n".join(relationships)

            mermaid_prompt = self.mermaid_user_prompt_template.format(
                relationships=relationships_str,
                total_programs=total_progs,
                total_copybooks=total_cpys
            )

            mermaid_response = await self.ai_client.call(
                prompt=mermaid_prompt,
                system_prompt=self.system_prompt
            )
            
            clean_mermaid = mermaid_response.strip()
            if clean_mermaid.startswith("```mermaid"):
                clean_mermaid = clean_mermaid[10:]
            elif clean_mermaid.startswith("```"):
                clean_mermaid = clean_mermaid[3:]
            if clean_mermaid.endswith("```"):
                clean_mermaid = clean_mermaid[:-3]
            
            logger.info("Generated Mermaid diagram")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "mermaid_diagram": clean_mermaid.strip()
                },
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Error in MermaidDiagramAgent: {e}")
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
