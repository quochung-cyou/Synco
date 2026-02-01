import logging
import networkx as nx
import asyncio
import os
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class PythonConversionStrategyAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "python_conversion_strategy_agent"

    @property
    def dependencies(self) -> List[str]:
        return ["dependency_aggregator_agent", "complexity_analyzer_agent"]
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(
                agent_name="dependency_aggregator_agent",
                description="Requires dependency graph for topo sort & connectivity analysis",
                required=True
            ),
            AgentDependency(
                agent_name="complexity_analyzer_agent",
                description="Requires complexity scores for risk assessment",
                required=True
            )
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)
        self.system_prompt = self.load_prompt("python_strategy_system.txt")
        self.user_prompt_template = self.load_prompt("python_strategy_user.txt")

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            aggregator_data = context.previous_results.get("dependency_aggregator_agent", {})
            complexity_data = context.previous_results.get("complexity_analyzer_agent", {}).get("complexity_scores", {})
            
            G = nx.DiGraph()
            all_files = set(aggregator_data.keys())
            G.add_nodes_from(all_files)
            
            for file_path, deps in aggregator_data.items():
                for dep in deps:
                    target = dep.get("target")
                    if not target: 
                        continue
                    
                    target_path = next((f for f in all_files if target in f), None)
                    if target_path:
                        G.add_edge(file_path, target_path)

            try:
                conversion_order = list(reversed(list(nx.topological_sort(G))))
            except nx.NetworkXUnfeasible:
                conversion_order = list(all_files)

            incoming_map = {}
            for node in G.nodes():
                incoming_map[node] = list(G.predecessors(node))
            
            outgoing_map = {}
            for node in G.nodes():
                outgoing_map[node] = list(G.successors(node))

            plan = []
            total_items = len(conversion_order)
            processed = 0
            
            semaphore = asyncio.Semaphore(5)

            async def create_strategy(file_path):
                nonlocal processed
                score = complexity_data.get(file_path, 1)
                
                outgoing_deps = outgoing_map.get(file_path, [])
                incoming_deps = incoming_map.get(file_path, [])
                
                outgoing_str = "\n".join([os.path.basename(f) for f in outgoing_deps]) or "None"
                incoming_str = "\n".join([os.path.basename(f) for f in incoming_deps]) or "None"
                
                async with semaphore:
                    prompt = self.user_prompt_template.format(
                        file_path=file_path,
                        complexity=score,
                        outgoing_dependencies=outgoing_str,
                        incoming_dependencies=incoming_str
                    )
                    
                    strategy_text = await self.ai_client.call(
                        prompt=prompt,
                        system_prompt=self.system_prompt
                    )
                    
                    processed += 1
                    progress = int((processed / total_items) * 100)
                    await self.update_progress(progress, f"Planning {os.path.basename(file_path)}")
                    
                    return {
                        "file_path": file_path,
                        "complexity": score,
                        "status": "pending",
                        "strategy": strategy_text,
                        "dependencies": outgoing_deps
                    }

            tasks = [create_strategy(fp) for fp in conversion_order]
            plan = await asyncio.gather(*tasks)
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={
                    "conversion_plan": plan
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
