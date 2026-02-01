from fastapi import APIRouter
from app.api.v1.endpoints import system, migration, agents

api_router = APIRouter()
api_router.include_router(system.router, prefix="/system", tags=["system"])
api_router.include_router(migration.router, prefix="/migration", tags=["migration"])
api_router.include_router(agents.router, prefix="/agents", tags=["agents"])
