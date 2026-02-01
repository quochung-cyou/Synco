import logging
import io
import sys
import asyncio
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class PythonExecutorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "python_executor_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["python_logic_generator_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="python_logic_generator_agent",
                description="Requires converted Python code to execute",
                required=True
            )
        ]

    @property
    def auto_run(self) -> bool:
        return False

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            from app.repositories.migration_repository import MigrationRepository
            repo = MigrationRepository(context.db_session)
            
            run = repo.get_run_with_details(context.run_id)
            generator_data = {}
            if run:
                for res in run.agent_results:
                    if res.agent_name == "python_logic_generator_agent":
                        generator_data = res.data or {}
                        break
            
            converted_files = generator_data.get("converted_files", {})
            
            if not converted_files:
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={"execution_results": {}, "message": "No converted files to execute"},
                    progress=100
                )

            execution_results = {}
            total_files = len(converted_files)
            processed = 0

            for file_path, python_code in converted_files.items():
                await self.update_progress(
                    int((processed / total_files) * 100),
                    f"Executing {file_path}"
                )
                
                result = await self._execute_python_code(python_code, file_path)
                execution_results[file_path] = result
                processed += 1

            await self.update_progress(100, "Completed all Python executions")

            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={"execution_results": execution_results},
                progress=100
            )

        except Exception as e:
            logger.exception(f"Python executor agent failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    async def _execute_python_code(self, code: str, file_path: str) -> dict:
        def run_code():
            old_stdout = sys.stdout
            old_stderr = sys.stderr
            captured_stdout = io.StringIO()
            captured_stderr = io.StringIO()
            
            try:
                sys.stdout = captured_stdout
                sys.stderr = captured_stderr
                
                exec_globals = {"__name__": "__main__"}
                exec(code, exec_globals)
                
                return {
                    "success": True,
                    "stdout": captured_stdout.getvalue(),
                    "stderr": captured_stderr.getvalue(),
                    "error": None
                }
            except Exception as e:
                return {
                    "success": False,
                    "stdout": captured_stdout.getvalue(),
                    "stderr": captured_stderr.getvalue(),
                    "error": str(e)
                }
            finally:
                sys.stdout = old_stdout
                sys.stderr = old_stderr

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, run_code)

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        from app.models.migration import AgentResult as AgentResultModel
        try:
            db = context.db_session
            execution_results = result.data.get("execution_results", {}) if result.data else {}
            
            agent_result = AgentResultModel(
                run_id=context.run_id,
                agent_name=self.name,
                status=result.status.value,
                data={
                    "execution_count": len(execution_results),
                    "results": execution_results
                },
                error=result.error,
                execution_time=int(result.execution_time),
                progress=result.progress
            )
            db.add(agent_result)
            db.commit()
        except Exception:
            raise
