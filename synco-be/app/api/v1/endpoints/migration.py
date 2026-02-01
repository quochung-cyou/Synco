from fastapi import APIRouter, Depends, HTTPException, BackgroundTasks
from sqlalchemy.orm import Session
from typing import Dict, Any
from app.api.deps import get_db
from app.schemas.migration import MigrationRunDetail
from app.schemas.rerun import RerunRequest
from app.services.migration_orchestrator import MigrationOrchestrator
from app.repositories.migration_repository import MigrationRepository
from app.services.agent_registry import AgentRegistry

router = APIRouter()

@router.post("/runs")
async def start_migration(
    source_folder: str = "data/cobol_source",
    background_tasks: BackgroundTasks = None,
    db: Session = Depends(get_db)
):
    repo = MigrationRepository(db)
    run = repo.create_run(cobol_source_path=source_folder)
    
    orchestrator = MigrationOrchestrator(db)
    background_tasks.add_task(orchestrator.start_migration_sync, source_folder, run.id)
    
    return {"run_id": run.id, "status": run.status}

@router.get("/runs/{run_id}")
def get_migration_run(run_id: int, db: Session = Depends(get_db)) -> Dict[str, Any]:
    repo = MigrationRepository(db)
    run = repo.get_run_with_details(run_id)
    
    if not run:
        raise HTTPException(status_code=404, detail="Migration run not found")
    
    agent_names = AgentRegistry.get_all_agents()
    agent_metadata = {}
    
    for agent_name in agent_names:
        try:
            temp_agent = AgentRegistry.get_agent(agent_name, None)
            agent_metadata[agent_name] = {
                "type": temp_agent.agent_type,
                "dependencies": temp_agent.dependencies,
                "auto_run": temp_agent.auto_run,
                "dependency_details": [
                    {
                        "agent_name": dep.agent_name,
                        "description": dep.description,
                        "required": dep.required
                    }
                    for dep in temp_agent.dependency_metadata
                ]
            }
        except Exception:
            continue
    
    from app.schemas.migration import MigrationRunDetail
    run_dict = MigrationRunDetail.model_validate(run).model_dump()
    run_dict["agent_metadata"] = agent_metadata
    run_dict["execution_order"] = AgentRegistry.resolve_execution_order_for_auto_run()
    
    return run_dict

@router.post("/runs/{run_id}/rerun")
async def rerun_migration(
    run_id: int,
    request: RerunRequest,
    background_tasks: BackgroundTasks,
    db: Session = Depends(get_db)
):
    repo = MigrationRepository(db)
    run = repo.get_run(run_id)
    
    if not run:
        raise HTTPException(status_code=404, detail="Migration run not found")
    
    auto_run_agents = AgentRegistry.get_auto_run_agents()
    if not repo.check_auto_run_agents_completed(run_id, auto_run_agents):
        raise HTTPException(status_code=400, detail="Cannot rerun: not all auto-run agents have completed")
    
    for agent_name in request.agents_to_rerun:
        if agent_name not in AgentRegistry.get_all_agents():
            raise HTTPException(status_code=400, detail=f"Agent '{agent_name}' not found")
    
    orchestrator = MigrationOrchestrator(db)
    background_tasks.add_task(
        orchestrator.rerun_migration_sync,
        run_id,
        request.agents_to_rerun,
        request.additional_context
    )
    
    dependent_agents = AgentRegistry.get_dependent_agents(request.agents_to_rerun)
    
    return {
        "run_id": run_id,
        "status": "rerunning",
        "agents_to_rerun": request.agents_to_rerun,
        "dependent_agents": list(dependent_agents)
    }
