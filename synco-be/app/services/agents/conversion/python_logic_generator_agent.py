import logging
import os
import asyncio
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class PythonLogicGeneratorAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "python_logic_generator_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["python_conversion_strategy_agent", "json_cleaner_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="python_conversion_strategy_agent",
                description="Requires detailed conversion strategy",
                required=True
            ),
            AgentDependency(
                agent_name="json_cleaner_agent",
                description="Requires cleaned COBOL code",
                required=True
            )
        ]

    def __init__(self, ai_client):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("python_conversion_system.txt")
        self.user_prompt_template = self.load_prompt("python_conversion_user.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            plan_data = context.previous_results.get("python_conversion_strategy_agent", {})
            conversion_plan = plan_data.get("conversion_plan", [])
            
            cleaner_data = context.previous_results.get("json_cleaner_agent", {})
            
            if not conversion_plan:
                return AgentResult(self.name, AgentStatus.COMPLETED, {}, 100)

            converted_files = {}
            total_items = len(conversion_plan)
            processed = 0
            
            semaphore = asyncio.Semaphore(3)

            async def convert_item(item):
                nonlocal processed
                file_path = item.get("file_path")
                strategy = item.get("strategy", "No strategy provided.")
                
                file_data = cleaner_data.get(file_path, {})
                code_content = file_data.get("code", "")
                
                if not code_content:
                    code_content = context.files.get(file_path, "")

                if not code_content:
                    return file_path, "# Error: No content found"

                async with semaphore:
                    prompt = self.user_prompt_template.format(
                        strategy=strategy,
                        code_content=code_content
                    )
                    
                    response = await self.ai_client.call(
                        prompt=prompt,
                        system_prompt=self.system_prompt
                    )
                    
                    processed += 1
                    progress = int((processed / total_items) * 100)
                    await self.update_progress(progress, f"Converting {os.path.basename(file_path)}")
                    
                    clean_code = response.strip()
                    if clean_code.startswith("```python"):
                        clean_code = clean_code[9:]
                    elif clean_code.startswith("```"):
                        clean_code = clean_code[3:]
                    if clean_code.endswith("```"):
                        clean_code = clean_code[:-3]
                        
                    return file_path, clean_code.strip()

            tasks = [convert_item(item) for item in conversion_plan]
            results_list = await asyncio.gather(*tasks)
            
            for file_path, python_code in results_list:
                converted_files[file_path] = python_code
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "converted_files": converted_files
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
