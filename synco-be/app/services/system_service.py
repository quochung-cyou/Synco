from app.repositories.system_repository import SystemRepository
from app.schemas.system import SystemStatus

class SystemService:
    def __init__(self):
        self.repository = SystemRepository()

    def check_health(self) -> SystemStatus:
        data = self.repository.get_system_info()
        return SystemStatus(**data)
