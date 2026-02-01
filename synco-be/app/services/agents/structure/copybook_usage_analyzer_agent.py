import logging
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class CopybookUsageAnalyzerAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "copybook_usage_analyzer_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["dependency_aggregator_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="dependency_aggregator_agent",
                description="Requires aggregated dependencies",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            import os
            
            # Get aggregated dependencies directly
            aggregator_data = context.previous_results.get("dependency_aggregator_agent", {})
            
            # Flatten dependencies logic (formerly in DataCollectorAgent)
            all_dependencies = []
            file_map = {}
            
            for file_path, deps in aggregator_data.items():
                file_name = os.path.basename(file_path)
                file_map[file_name] = "analyzed"
                
                for dep in deps:
                    all_dependencies.append({
                        "source": file_name,
                        "target": dep.get("target"),
                        "type": dep.get("type"),
                        "line": dep.get("line")
                    })
            
            # Analyze copybook usage
            copybook_usage = {}
            
            for dep in all_dependencies:
                if dep.get("type") == "COPY":
                    source = dep.get("source")
                    target = dep.get("target")
                    if source and target:
                        if source not in copybook_usage:
                            copybook_usage[source] = []
                        copybook_usage[source].append(target)
            
            file_structure_summary = "\n".join([f"{k}: {v}" for k, v in file_map.items()])
            usage_summary = "\n".join([f"{k} uses {v}" for k, v in copybook_usage.items()])
            
            total_progs = len([f for f in file_map.keys() if f.lower().endswith('.cbl')])
            total_cpys = len([f for f in file_map.keys() if f.lower().endswith('.cpy')])
            
            logger.info(f"Analyzed copybook usage: {len(copybook_usage)} files use copybooks")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "copybook_usage": copybook_usage,
                    # Pass through flattened data for other agents
                    "all_dependencies": all_dependencies,
                    "file_map": file_map,
                    "file_structure_summary": file_structure_summary,
                    "usage_summary": usage_summary,
                    "total_programs": total_progs,
                    "total_copybooks": total_cpys
                },
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Error in CopybookUsageAnalyzerAgent: {e}")
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
