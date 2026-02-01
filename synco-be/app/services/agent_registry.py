from typing import Dict, List, Type, Set
import logging
from app.agents.base import BaseAgent

logger = logging.getLogger(__name__)

class AgentRegistry:
    _agents: Dict[str, Type[BaseAgent]] = {}

    @classmethod
    def register(cls, agent_cls: Type[BaseAgent]):
        agent_instance = agent_cls(None) 
        cls._agents[agent_instance.name] = agent_cls
        return agent_cls

    @classmethod
    def get_agent(cls, name: str, ai_client) -> BaseAgent:
        if name not in cls._agents:
            raise ValueError(f"Agent {name} not found")
        return cls._agents[name](ai_client)

    @classmethod
    def get_all_agents(cls) -> List[str]:
        return list(cls._agents.keys())

    @classmethod
    def get_auto_run_agents(cls) -> List[str]:
        return [name for name, agent_cls in cls._agents.items() 
                if agent_cls(None).auto_run]

    @classmethod
    def get_dependent_agents(cls, agent_names: List[str]) -> Set[str]:
        all_dependents = set()
        to_process = set(agent_names)
        
        while to_process:
            current = to_process.pop()
            for name, agent_cls in cls._agents.items():
                if name not in all_dependents and name not in agent_names:
                    temp_agent = agent_cls(None)
                    if current in temp_agent.dependencies:
                        all_dependents.add(name)
                        to_process.add(name)
        
        return all_dependents

    @classmethod
    def resolve_execution_order(cls) -> List[str]:
        return cls._resolve_order_for_agents(list(cls._agents.keys()))

    @classmethod
    def resolve_execution_order_for_auto_run(cls) -> List[str]:
        auto_run_agents = cls.get_auto_run_agents()
        return cls._resolve_order_for_agents(auto_run_agents)

    @classmethod
    def resolve_execution_order_for_agents(cls, agent_names: List[str]) -> List[str]:
        return cls._resolve_order_for_agents(agent_names)

    @classmethod
    def _resolve_order_for_agents(cls, agent_names: List[str]) -> List[str]:
        agent_set = set(agent_names)
        graph = {name: set() for name in agent_names}
        
        for name in agent_names:
            if name in cls._agents:
                temp_agent = cls._agents[name](None)
                for dep in temp_agent.dependencies:
                    if dep in agent_set:
                        graph[name].add(dep)
        
        visited = set()
        temp_mark = set()
        order = []

        def visit(node):
            if node in temp_mark:
                raise ValueError(f"Cyclic dependency detected involving {node}")
            if node not in visited:
                temp_mark.add(node)
                for neighbor in graph[node]: 
                   visit(neighbor) 
                temp_mark.remove(node)
                visited.add(node)
                order.append(node)
        
        for node in agent_names:
            if node not in visited:
                visit(node)
        
        return order

    @classmethod
    def get_persistent_agents(cls) -> List[str]:
        return [name for name, agent_cls in cls._agents.items() 
                if agent_cls(None).persistent]

