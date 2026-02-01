import logging
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class DependencyAggregatorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "dependency_aggregator_agent"

    @property
    def dependencies(self) -> List[str]:
        return [
            "copy_dependency_extractor_agent",
            "call_dependency_extractor_agent",
            "perform_dependency_extractor_agent",
            "file_op_dependency_extractor_agent"
        ]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="copy_dependency_extractor_agent",
                description="COPY statement dependencies",
                required=True
            ),
            AgentDependency(
                agent_name="call_dependency_extractor_agent",
                description="CALL statement dependencies",
                required=True
            ),
            AgentDependency(
                agent_name="perform_dependency_extractor_agent",
                description="PERFORM statement dependencies",
                required=True
            ),
            AgentDependency(
                agent_name="file_op_dependency_extractor_agent",
                description="File operation dependencies",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            copy_deps = context.previous_results.get("copy_dependency_extractor_agent", {})
            call_deps = context.previous_results.get("call_dependency_extractor_agent", {})
            perform_deps = context.previous_results.get("perform_dependency_extractor_agent", {})
            file_op_deps = context.previous_results.get("file_op_dependency_extractor_agent", {})
            
            aggregated_results = {}
            all_file_paths = set()
            all_file_paths.update(copy_deps.keys())
            all_file_paths.update(call_deps.keys())
            all_file_paths.update(perform_deps.keys())
            all_file_paths.update(file_op_deps.keys())
            
            for file_path in all_file_paths:
                all_dependencies = []
                
                if file_path in copy_deps:
                    all_dependencies.extend(copy_deps[file_path])
                if file_path in call_deps:
                    all_dependencies.extend(call_deps[file_path])
                if file_path in perform_deps:
                    all_dependencies.extend(perform_deps[file_path])
                if file_path in file_op_deps:
                    all_dependencies.extend(file_op_deps[file_path])
                
                aggregated_results[file_path] = all_dependencies
            
            logger.info(f"Aggregated dependencies for {len(aggregated_results)} files")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=aggregated_results,
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Error in DependencyAggregatorAgent: {e}")
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
            logger.info(f"Saved agent result for {self.name} (processed {len(context.files)} files)")
        except Exception as e:
            logger.error(f"Failed to save agent result for {self.name}: {e}")
            raise
