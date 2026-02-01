import logging
import asyncio
import subprocess
import os
import tempfile
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class CobolExecutorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "cobol_executor_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["json_cleaner_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="json_cleaner_agent",
                description="Requires COBOL file information",
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
            cleaner_data = context.previous_results.get("json_cleaner_agent", {})
            
            if not cleaner_data:
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={"execution_results": {}, "message": "No COBOL files to execute"},
                    progress=100
                )

            execution_results = {}
            file_paths = list(cleaner_data.keys())
            total_files = len(file_paths)
            processed = 0

            for file_path in file_paths:
                if not file_path.lower().endswith('.cbl'):
                    continue
                    
                await self.update_progress(
                    int((processed / total_files) * 100),
                    f"Executing {os.path.basename(file_path)}"
                )
                
                full_path = self._resolve_cobol_path(file_path, context)
                if full_path and os.path.exists(full_path):
                    result = await self._execute_cobol(full_path)
                    execution_results[file_path] = result
                else:
                    execution_results[file_path] = {
                        "success": False,
                        "stdout": "",
                        "stderr": "",
                        "error": f"COBOL file not found: {file_path}"
                    }
                    
                processed += 1

            await self.update_progress(100, "Completed all COBOL executions")

            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={"execution_results": execution_results},
                progress=100
            )

        except Exception as e:
            logger.exception(f"COBOL executor agent failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    def _resolve_cobol_path(self, file_path: str, context: AgentContext) -> str:
        if os.path.isabs(file_path) and os.path.exists(file_path):
            return file_path
        
        base_dir = os.path.join(os.path.dirname(__file__), "..", "..", "..", "..", "data", "cobol_source")
        base_dir = os.path.normpath(base_dir)
        
        full_path = os.path.join(base_dir, os.path.basename(file_path))
        if os.path.exists(full_path):
            return full_path
            
        if file_path in context.files:
            return file_path
            
        return None

    async def _execute_cobol(self, cobol_file_path: str) -> dict:
        def run_cobc():
            work_dir = os.path.dirname(cobol_file_path)
            file_name = os.path.basename(cobol_file_path)
            
            try:
                result = subprocess.run(
                    ["cobc", "-x", "-j", file_name],
                    cwd=work_dir,
                    capture_output=True,
                    text=True,
                    timeout=60
                )
                
                return {
                    "success": result.returncode == 0,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                    "error": None if result.returncode == 0 else f"Exit code: {result.returncode}"
                }
            except subprocess.TimeoutExpired:
                logger.error(f"COBOL code execution timed out: {cobol_file_path}")
                return {
                    "success": False,
                    "stdout": "",
                    "stderr": "",
                    "error": "Execution timed out after 60 seconds"
                }
            except FileNotFoundError:
                logger.error(f"COBOL code execution failed: {cobol_file_path}, GnuCOBOL (cobc) not found. Please install GnuCOBOL.")
                return {
                    "success": False,
                    "stdout": "",
                    "stderr": "",
                    "error": "GnuCOBOL (cobc) not found. Please install GnuCOBOL."
                }
            except Exception as e:
                logger.error(f"COBOL code execution failed: {cobol_file_path}, {e}")
                return {
                    "success": False,
                    "stdout": "",
                    "stderr": "",
                    "error": str(e)
                }

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, run_cobc)

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
