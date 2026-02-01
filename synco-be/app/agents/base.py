from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, Dict, List, Optional
from pydantic import BaseModel, Field

class AgentStatus(str, Enum):
    PENDING = "PENDING"
    RUNNING = "RUNNING"
    COMPLETED = "COMPLETED"
    FAILED = "FAILED"
    SKIPPED = "SKIPPED"


class AgentResult(BaseModel):
    agent_name: str
    status: AgentStatus
    data: Optional[Any] = None
    error: Optional[str] = None
    execution_time: float = 0.0
    progress: int = 0  # 0-100 scale
    current_action: Optional[str] = None

class AgentContext(BaseModel):
    run_id: int
    db_session: Any = None
    files: Dict[str, str] = {}
    previous_results: Dict[str, Any] = {}
    config: Dict[str, Any] = {}
    update_progress_callback: Optional[Any] = None
    additional_call_context: Dict[str, Any] = {}
    
    class Config:
        arbitrary_types_allowed = True

class AgentDependency(BaseModel):
    agent_name: str
    description: Optional[str] = None
    required: bool = True

class BaseAgent(ABC):
    def __init__(self, ai_client: Any = None):
        self.ai_client = ai_client
        self.context = None

    async def update_progress(self, progress: int, current_action: str) -> None:
        if self.context and self.context.update_progress_callback:
            await self.context.update_progress_callback(progress, current_action)

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @property
    def dependencies(self) -> List[str]:
        return []
    
    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [AgentDependency(agent_name=dep) for dep in self.dependencies]
    
    @property
    def agent_type(self) -> str:
        return "ai" if self.ai_client is not None else "logic"

    @property
    def auto_run(self) -> bool:
        return True

    @abstractmethod
    async def run(self, context: AgentContext) -> Any:
        pass

    @abstractmethod
    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        pass

    def load_prompt(self, prompt_name: str) -> str:
        import os
        base_path = os.path.join(os.path.dirname(__file__), "..", "prompts")
        file_path = os.path.join(base_path, prompt_name)
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                return f.read().strip()
        except FileNotFoundError:
            import logging
            logger = logging.getLogger(__name__)
            logger.error(f"Prompt file not found: {file_path}")
            return ""

    @property
    def persistent(self) -> bool:
        return False


