from fastapi import APIRouter, Depends
from app.schemas.system import SystemStatus
from app.services.system_service import SystemService

router = APIRouter()

def get_service():
    return SystemService()

@router.get("/health", response_model=SystemStatus)
def health_check(service: SystemService = Depends(get_service)):
    return service.check_health()
