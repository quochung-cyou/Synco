import logging
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class DashboardSummaryAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "dashboard_summary_agent"

    @property
    def dependencies(self) -> List[str]:
        return [
            "complexity_analyzer_agent",
            "copybook_usage_analyzer_agent",
            "dependency_aggregator_agent"
        ]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(agent_name="complexity_analyzer_agent", description="Complexity metrics", required=True),
            AgentDependency(agent_name="copybook_usage_analyzer_agent", description="File counts", required=True),
            AgentDependency(agent_name="dependency_aggregator_agent", description="Dependency counts", required=True)
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            complexity_data = context.previous_results.get("complexity_analyzer_agent", {})
            avg_complexity = complexity_data.get("average_complexity", 0)
            
            usage_data = context.previous_results.get("copybook_usage_analyzer_agent", {})
            total_programs = usage_data.get("total_programs", 0)
            total_copybooks = usage_data.get("total_copybooks", 0)
            total_files = total_programs + total_copybooks
            
            aggregator_data = context.previous_results.get("dependency_aggregator_agent", {})
            
            cics_count = 0
            sql_count = 0
            file_io_count = 0
            
            for file_path, deps in aggregator_data.items():
                has_cics = False
                has_sql = False
                has_file_io = False
                
                for dep in deps:
                    dtype = str(dep.get("type", "")).upper()
                    target = str(dep.get("target", "")).upper()
                    
                    if "CICS" in dtype or "CICS" in target:
                        has_cics = True
                    if "SQL" in dtype or "SQL" in target or "DB2" in dtype:
                        has_sql = True
                    if "FILE" in dtype or "READ" in dtype or "WRITE" in dtype:
                        has_file_io = True
                        
                if has_cics:
                    cics_count += 1
                if has_sql:
                    sql_count += 1
                if has_file_io:
                    file_io_count += 1
            
            base_count = total_programs if total_programs > 0 else (total_files if total_files > 0 else 1)
            
            cics_pct = int((cics_count / base_count) * 100)
            sql_pct = int((sql_count / base_count) * 100)
            file_io_pct = int((file_io_count / base_count) * 100)
            
            dashboard_data = {
                "Analysis Summary": {
                    "Total Programs": total_programs,
                    "Total Copybooks": total_copybooks,
                    "Avg Complexity": round(avg_complexity, 1)
                },
                "Migration Challenges": {
                    "CICS Dependencies": f"{cics_pct}% of programs",
                    "SQL Dependencies": f"{sql_pct}% of programs",
                    "File I/O Patterns": f"{file_io_pct}% of programs"
                }
            }
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=dashboard_data,
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
