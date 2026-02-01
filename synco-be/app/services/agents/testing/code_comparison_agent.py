import logging
import re
from typing import List, Dict
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)


class CodeComparisonAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "code_comparison_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["python_executor_agent", "cobol_executor_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="python_executor_agent",
                description="Requires Python execution results",
                required=True
            ),
            AgentDependency(
                agent_name="cobol_executor_agent",
                description="Requires COBOL execution results",
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
            python_data = context.previous_results.get("python_executor_agent", {})
            cobol_data = context.previous_results.get("cobol_executor_agent", {})
            
            python_results = python_data.get("execution_results", {})
            cobol_results = cobol_data.get("execution_results", {})
            
            if not python_results and not cobol_results:
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={"comparison_results": {}, "summary": {"total": 0, "passed": 0, "failed": 0, "pass_rate": 0}},
                    progress=100
                )

            await self.update_progress(10, "Matching Python and COBOL outputs")
            
            comparison_results = {}
            total_tests = 0
            passed_tests = 0
            failed_tests = 0
            
            all_files = set()
            for file_path in python_results.keys():
                base_name = self._get_base_name(file_path)
                all_files.add(base_name)
            for file_path in cobol_results.keys():
                base_name = self._get_base_name(file_path)
                all_files.add(base_name)
            
            total_files = len(all_files)
            processed = 0
            
            for base_name in all_files:
                await self.update_progress(
                    10 + int((processed / max(total_files, 1)) * 80),
                    f"Comparing {base_name}"
                )
                
                python_output = self._find_output(python_results, base_name)
                cobol_output = self._find_output(cobol_results, base_name)
                
                comparison = self._compare_outputs(python_output, cobol_output, base_name)
                comparison_results[base_name] = comparison
                
                total_tests += 1
                if comparison["passed"]:
                    passed_tests += 1
                else:
                    failed_tests += 1
                    
                processed += 1

            await self.update_progress(100, "Comparison complete")

            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "comparison_results": comparison_results,
                    "summary": {
                        "total": total_tests,
                        "passed": passed_tests,
                        "failed": failed_tests,
                        "pass_rate": round((passed_tests / total_tests * 100), 2) if total_tests > 0 else 0
                    }
                },
                progress=100
            )

        except Exception as e:
            logger.exception(f"Code comparison agent failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    def _get_base_name(self, file_path: str) -> str:
        import os
        base = os.path.basename(file_path)
        name_without_ext = os.path.splitext(base)[0]
        return name_without_ext.upper()

    def _find_output(self, results: Dict, base_name: str) -> dict:
        for file_path, result in results.items():
            if self._get_base_name(file_path) == base_name:
                return result
        return None

    def _normalize_output(self, output: str) -> List[str]:
        if not output:
            return []
        
        lines = output.strip().split('\n')
        normalized = []
        for line in lines:
            line = re.sub(r'\s+', ' ', line.strip())
            line = re.sub(r'(\d+)\.0+\b', r'\1', line)
            line = re.sub(r'\b0+(\d+)\b', r'\1', line)
            if line:
                normalized.append(line.lower())
        return normalized

    def _clean_display_output(self, output: str) -> str:
        if not output:
            return ""
        lines = output.split('\n')
        cleaned = [line.rstrip() for line in lines]
        return '\n'.join(cleaned)

    def _compare_outputs(self, python_result: dict, cobol_result: dict, file_name: str) -> dict:
        if python_result is None and cobol_result is None:
            return {
                "passed": False,
                "file_name": file_name,
                "reason": "Both Python and COBOL outputs are missing",
                "python_available": False,
                "cobol_available": False,
                "differences": []
            }
        
        if python_result is None:
            return {
                "passed": False,
                "file_name": file_name,
                "reason": "Python output is missing",
                "python_available": False,
                "cobol_available": True,
                "cobol_output": self._clean_display_output(cobol_result.get("stdout", "")),
                "differences": []
            }
        
        if cobol_result is None:
            return {
                "passed": False,
                "file_name": file_name,
                "reason": "COBOL output is missing",
                "python_available": True,
                "cobol_available": False,
                "python_output": self._clean_display_output(python_result.get("stdout", "")),
                "differences": []
            }
        
        if not python_result.get("success"):
            return {
                "passed": False,
                "file_name": file_name,
                "reason": f"Python execution failed: {python_result.get('error', 'Unknown error')}",
                "python_available": True,
                "cobol_available": True,
                "python_error": python_result.get("error"),
                "differences": []
            }
        
        if not cobol_result.get("success"):
            return {
                "passed": False,
                "file_name": file_name,
                "reason": f"COBOL execution failed: {cobol_result.get('error', 'Unknown error')}",
                "python_available": True,
                "cobol_available": True,
                "cobol_error": cobol_result.get("error"),
                "differences": []
            }
        
        python_lines = self._normalize_output(python_result.get("stdout", ""))
        cobol_lines = self._normalize_output(cobol_result.get("stdout", ""))
        
        differences = []
        max_lines = max(len(python_lines), len(cobol_lines))
        
        for i in range(max_lines):
            py_line = python_lines[i] if i < len(python_lines) else "<missing>"
            cb_line = cobol_lines[i] if i < len(cobol_lines) else "<missing>"
            
            if py_line != cb_line:
                differences.append({
                    "line": i + 1,
                    "python": py_line,
                    "cobol": cb_line
                })
        
        passed = len(differences) == 0
        
        return {
            "passed": passed,
            "file_name": file_name,
            "reason": "Outputs match" if passed else f"Found {len(differences)} difference(s)",
            "python_available": True,
            "cobol_available": True,
            "python_output": self._clean_display_output(python_result.get("stdout", "")),
            "cobol_output": self._clean_display_output(cobol_result.get("stdout", "")),
            "python_lines": len(python_lines),
            "cobol_lines": len(cobol_lines),
            "differences": differences
        }

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        from app.models.migration import AgentResult as AgentResultModel
        try:
            db = context.db_session
            data = result.data if result.data else {}
            
            agent_result = AgentResultModel(
                run_id=context.run_id,
                agent_name=self.name,
                status=result.status.value,
                data=data,
                error=result.error,
                execution_time=int(result.execution_time),
                progress=result.progress
            )
            db.add(agent_result)
            db.commit()
        except Exception:
            raise
