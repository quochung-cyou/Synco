import logging
from datetime import datetime
from typing import List, Any
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency
from app.repositories.migration_repository import MigrationRepository

logger = logging.getLogger(__name__)

class ChatHistoryAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "chat_history_agent"

    @property
    def auto_run(self) -> bool:
        return False

    @property
    def persistent(self) -> bool:
        return True

    @property
    def dependencies(self) -> List[str]:
        return ["chatbot_agent"]

    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(agent_name="chatbot_agent", description="Chat responses to store", required=True)
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            await self.update_progress(10, "Loading existing chat history...")
            
            repo = MigrationRepository(context.db_session)
            existing_result = repo.get_agent_results_data(context.run_id, [self.name])
            existing_data = existing_result.get(self.name, {})
            
            history = []
            if existing_data and isinstance(existing_data, dict):
                history = existing_data.get("history", [])
            
            logger.info(f"Existing history has {len(history)} messages")
            
            await self.update_progress(50, "Appending new chat message...")
            
            chatbot_result = context.previous_results.get("chatbot_agent", {})
            if chatbot_result:
                user_message = chatbot_result.get("user_message", "")
                assistant_response = chatbot_result.get("assistant_response", "")
                
                if user_message or assistant_response:
                    history.append({
                        "user_message": user_message,
                        "assistant_response": assistant_response,
                        "timestamp": datetime.utcnow().isoformat()
                    })
                    logger.info(f"Appended new message. Total messages: {len(history)}")
            
            await self.update_progress(100, "Chat history updated")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data={"history": history},
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Chat history agent failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        pass
