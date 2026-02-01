from fastapi import APIRouter, HTTPException
from typing import List, Dict, Any
from app.services.agent_registry import AgentRegistry
from app.agents.base import AgentDependency

router = APIRouter()

@router.get("/agents")
async def get_all_agents() -> Dict[str, Any]:
    """
    Get all registered agents with their metadata.
    """
    try:
        agent_names = AgentRegistry.get_all_agents()
        agents_info = []
        
        for agent_name in agent_names:
            temp_agent = AgentRegistry.get_agent(agent_name, None)
            agents_info.append({
                "name": temp_agent.name,
                "type": temp_agent.agent_type,
                "dependencies": temp_agent.dependencies,
                "dependency_metadata": [
                    {
                        "agent_name": dep.agent_name,
                        "description": dep.description,
                        "required": dep.required
                    }
                    for dep in temp_agent.dependency_metadata
                ]
            })
        
        return {
            "total": len(agents_info),
            "agents": agents_info
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/agents/execution-order")
async def get_execution_order() -> Dict[str, Any]:
    """
    Get the resolved execution order of agents based on dependencies.
    """
    try:
        execution_order = AgentRegistry.resolve_execution_order()
        return {
            "execution_order": execution_order,
            "total_agents": len(execution_order)
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/agents/dependency-graph")
async def get_dependency_graph() -> Dict[str, Any]:
    """
    Get the dependency graph for visualization.
    Returns nodes and edges for graph visualization.
    """
    try:
        agent_names = AgentRegistry.get_all_agents()
        nodes = []
        edges = []
        
        for agent_name in agent_names:
            temp_agent = AgentRegistry.get_agent(agent_name, None)
            nodes.append({
                "id": temp_agent.name,
                "label": temp_agent.name,
                "type": temp_agent.agent_type
            })
            
            for dep_meta in temp_agent.dependency_metadata:
                edges.append({
                    "from": dep_meta.agent_name,
                    "to": temp_agent.name,
                    "label": dep_meta.description or "",
                    "required": dep_meta.required
                })
        
        return {
            "nodes": nodes,
            "edges": edges
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
