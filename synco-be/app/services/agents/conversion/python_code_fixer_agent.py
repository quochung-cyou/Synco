import logging
import asyncio
from typing import List, Dict, Any
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency
from app.repositories.migration_repository import MigrationRepository

logger = logging.getLogger(__name__)

class PythonCodeFixerAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "python_code_fixer_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["python_logic_generator_agent"]
    
    @property
    def auto_run(self) -> bool:
        return False
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="python_logic_generator_agent",
                description="Requires existing generated Python code to fix",
                required=True
            )
        ]

    def __init__(self, ai_client):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("python_code_fixer_system.txt")
        self.user_prompt_template = self.load_prompt("python_code_fixer_user.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            additional_context = context.additional_call_context or {}
            fix_requests = additional_context.get("fix_requests", [])
            
            if not fix_requests:
                return AgentResult(self.name, AgentStatus.COMPLETED, data={"message": "No fix_requests provided"}, progress=100)
            
            generator_data = context.previous_results.get("python_logic_generator_agent", {})
            converted_files = generator_data.get("converted_files", {})
            
            repo = MigrationRepository(context.db_session)
            results = []
            
            total_items = len(fix_requests)
            processed = 0
            
            for req in fix_requests:
                file_name = req.get("file_name")
                error_description = req.get("error") or req.get("assertion_error") or ""
                user_feedback = req.get("user_feedback", "")
                
                if not file_name:
                    logger.warning("Skipping fix request without file_name")
                    continue
                    
                current_code = converted_files.get(file_name)
                if not current_code and not file_name.endswith('.cbl'):
                    file_name_with_ext = f"{file_name}.cbl"
                    current_code = converted_files.get(file_name_with_ext)
                    if current_code:
                        file_name = file_name_with_ext

                if not current_code:
                    logger.error(f"Code for {file_name} not found in previous results")
                    results.append({"file_name": file_name, "status": "failed", "error": "Code not found"})
                    continue

                cobol_code = context.files.get(file_name, "COBOL source not found.")

                processed += 1
                await self.update_progress(int((processed / total_items) * 90), f"Fixing {file_name}")

                prompt = self.user_prompt_template.format(
                    file_path=file_name,
                    cobol_code=cobol_code,
                    current_code=current_code,
                    error_description=error_description,
                    user_feedback=user_feedback
                )
                
                response = await self.ai_client.call(
                    prompt=prompt,
                    system_prompt=self.system_prompt
                )
                
                fixed_code = self._clean_code(response)
                logger.info(f"Fixed code for {file_name}: {fixed_code}")
                
                updated = repo.update_agent_converted_file(context.run_id, "python_logic_generator_agent", file_name, fixed_code)
                
                if updated:
                    logger.info(f"Successfully updated code for {file_name} in DB")
                    
                    converted_files[file_name] = fixed_code
                    results.append({
                        "file_name": file_name,
                        "status": "fixed",
                        "fixed_code_snippet": fixed_code[:100] + "..."
                    })
                else:
                    logger.error(f"Failed to update code for {file_name} in DB")
                    results.append({
                        "file_name": file_name,
                        "status": "failed_db_update"
                    })

            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "results": results
                },
                progress=100
            )

        except Exception as e:
            logger.exception("Failed to fix python code batch")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=100
            )

    def _clean_code(self, response: str) -> str:
        fixed_code = response.strip()
        if fixed_code.startswith("```python"):
            fixed_code = fixed_code[9:]
        elif fixed_code.startswith("```"):
            fixed_code = fixed_code[3:]
        if fixed_code.endswith("```"):
            fixed_code = fixed_code[:-3]
        return fixed_code.strip()

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        repo = MigrationRepository(context.db_session)
        repo.update_agent_result(
            run_id=context.run_id,
            agent_name=self.name,
            status=result.status.value,
            data=result.data,
            error=result.error,
            progress=result.progress
        )
