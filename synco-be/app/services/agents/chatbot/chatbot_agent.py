import logging
import json
from typing import List
from app.agents.base import BaseAgent, AgentContext, AgentResult, AgentStatus, AgentDependency

logger = logging.getLogger(__name__)

class ChatbotAgent(BaseAgent):
    @property
    def name(self) -> str:
        return "chatbot_agent"

    @property
    def auto_run(self) -> bool:
        return False

    @property
    def dependencies(self) -> List[str]:
        return ["context_extractor_agent"]

    @property
    def dependency_metadata(self) -> List[AgentDependency]:
        return [
            AgentDependency(agent_name="context_extractor_agent", description="Migration context", required=True)
        ]

    def __init__(self, ai_client=None):
        super().__init__(ai_client)

    async def run(self, context: AgentContext) -> AgentResult:
        try:
            user_message = context.additional_call_context.get("user_message", "")
            if not user_message:
                return AgentResult(
                    agent_name=self.name,
                    status=AgentStatus.COMPLETED,
                    data={"user_message": "", "assistant_response": "No message provided"},
                    progress=100
                )
            
            await self.update_progress(10, "Preparing context for AI...")
            
            extracted_context = context.previous_results.get("context_extractor_agent", {})
            
            chat_history = extracted_context.get("chat_history", [])
            history_text = ""
            for msg in chat_history[-5:]:
                history_text += f"User: {msg.get('user_message', '')}\n"
                history_text += f"Assistant: {msg.get('assistant_response', '')}\n\n"
            
            summary = extracted_context.get("summary", {})
            details = extracted_context.get("details", {})
            
            context_text = f"""
Migration Analysis Context:
{json.dumps(summary, indent=2)}

Details:
- Complexity: {json.dumps(details.get('complexity', {}), indent=2)}
- Insights: {json.dumps(details.get('insights', {}), indent=2)}
"""
            
            system_prompt = self.load_prompt("chatbot_system.txt")
            if not system_prompt:
                system_prompt = """You are an AI assistant helping with COBOL to Python migration analysis.
You have access to the analysis results from the migration tool.
Answer questions based on the provided context about the COBOL codebase analysis.
Be concise and helpful. If the information is not available in the context, say so."""
            
            await self.update_progress(30, "Calling AI...")
            
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"Context:\n{context_text}\n\nPrevious conversation:\n{history_text}\n\nUser question: {user_message}"}
            ]
            
            response = await self.ai_client.call(messages)
            
            await self.update_progress(90, "Processing response...")
            
            assistant_response = response if isinstance(response, str) else str(response)
            
            result_data = {
                "user_message": user_message,
                "assistant_response": assistant_response
            }
            
            await self.update_progress(100, "Chat complete")
            
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.COMPLETED,
                data=result_data,
                progress=100
            )
            
        except Exception as e:
            logger.error(f"Chatbot agent failed: {e}")
            return AgentResult(
                agent_name=self.name,
                status=AgentStatus.FAILED,
                error=str(e),
                progress=0
            )

    async def save_result(self, result: AgentResult, context: AgentContext) -> None:
        pass
