import logging
import json
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class JSONCleanerAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "json_cleaner_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["cobol_parser_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="cobol_parser_agent",
                description="Requires raw AI responses to clean",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            raw_responses = context.previous_results.get("cobol_parser_agent", {})
            
            if not raw_responses:
                logger.warning("No parsed responses found from cobol_parser_agent")
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={},
                    progress=100
                )
            
            cleaned_results = {}
            total_files = len(raw_responses)
            processed = 0
            
            for file_path, data in raw_responses.items():
                if isinstance(data, dict):
                    response = data.get("analysis", "")
                    code_content = data.get("code", "")
                else:
                    # Fallback for old format
                    response = data
                    code_content = ""

                clean_response = response.strip()
                
                if clean_response.startswith("```json"):
                    clean_response = clean_response[7:]
                if clean_response.startswith("```"):
                    clean_response = clean_response[3:]
                if clean_response.endswith("```"):
                    clean_response = clean_response[:-3]
                
                try:
                    file_data = json.loads(clean_response)
                    # Structure: {path: {analysis: object, code: string}}
                    cleaned_results[file_path] = {
                        "analysis": file_data,
                        "code": code_content
                    }
                except json.JSONDecodeError:
                    logger.error(f"Failed to parse JSON response for {file_path}")
                    cleaned_results[file_path] = {
                        "analysis": {"error": "Failed to parse JSON", "raw": response},
                        "code": code_content
                    }
                
                processed += 1
            
            progress = int((processed / total_files * 100)) if total_files > 0 else 100
            logger.info(f"Cleaned {processed}/{total_files} JSON responses")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=cleaned_results,
                progress=progress
            )
            
        except Exception as e:
            logger.error(f"Error in JSONCleanerAgent: {e}")
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
            logger.info(f"Saved agent result for {self.name} (processed {len(result.data) if result.data else 0} files)")
        except Exception as e:
            logger.error(f"Failed to save agent result for {self.name}: {e}")
            raise
