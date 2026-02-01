import logging
import re
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class PerformDependencyExtractorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "perform_dependency_extractor_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["json_cleaner_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="json_cleaner_agent",
                description="Requires COBOL analysis to be complete",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            results = {}
            total_files = len(context.files)
            processed = 0
            
            for file_path, content in context.files.items():
                dependencies = self._extract_perform_dependencies(content)
                results[file_path] = dependencies
                processed += 1
            
            progress = int((processed / total_files * 100)) if total_files > 0 else 100
            logger.info(f"Extracted PERFORM dependencies from {processed}/{total_files} files")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=results,
                progress=progress
            )
            
        except Exception as e:
            logger.error(f"Error in PerformDependencyExtractorAgent: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    def _extract_perform_dependencies(self, file_content: str) -> List[dict]:
        dependencies = []
        perform_pattern = re.compile(r'PERFORM\s+([A-Z0-9\-_]+)', re.IGNORECASE)
        
        for idx, line in enumerate(file_content.split('\n')):
            match = perform_pattern.search(line)
            if match:
                target = match.group(1).upper()
                dependencies.append({
                    "target": target,
                    "type": "PERFORM",
                    "line": idx + 1,
                    "context": line.strip()
                })
        
        return dependencies

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
