import logging
import re
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class ComplexityAnalyzerAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "complexity_analyzer_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["json_cleaner_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="json_cleaner_agent",
                description="Requires cleaned COBOL code to analyze complexity",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            cleaned_data = context.previous_results.get("json_cleaner_agent", {})
            complexity_scores = {}
            total_complexity = 0
            
            complexity_keywords = [
                r'\bIF\b', r'\bELSE\b', r'\bEVALUATE\b', r'\bWHEN\b', 
                r'\bPERFORM\b', r'\bSEARCH\b', r'\bVARYING\b', r'\bUNTIL\b',
                r'\bLOOP\b', r'\bCALL\b', r'\bEXEC\b', r'\bCICS\b', r'\bSQL\b'
            ]
            
            for file_path, data in cleaned_data.items():
                code = data.get("code", "")
                if not code:
                    complexity_scores[file_path] = 1
                    continue
                
                score = 1
                for keyword in complexity_keywords:
                    matches = re.findall(keyword, code, re.IGNORECASE)
                    score += len(matches)
                
                if score > 10:
                    score = 10
                
                complexity_scores[file_path] = score
                total_complexity += score
                
            avg_complexity = total_complexity / len(cleaned_data) if cleaned_data else 0
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "complexity_scores": complexity_scores,
                    "average_complexity": avg_complexity
                },
                progress=100
            )
            
        except Exception as e:
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
                data=result.data,
                error=result.error,
                execution_time=int(result.execution_time),
                progress=result.progress
            )
            db.add(agent_result)
            db.commit()
        except Exception:
            raise
