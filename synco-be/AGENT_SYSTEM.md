# Agent System Architecture

The Synco agent system is designed to be modular, dependency-aware, and explicitly registered. This document outlines how agents are structured, registered, and executed.

## 1. Agent Structure

All agents must inherit from `BaseAgent` (in `app.agents.base`).

### Key Properties

- `name`: Unique string identifier for the agent.
- `dependencies`: List of agent names that this agent depends on.
- `auto_run`: Boolean (default `True`). Set to `False` for agents that should only run when explicitly triggered via the `/rerun` API.
- `dependency_metadata`: (Optional) Detailed metadata about dependencies for visualization.

### Key Methods

- `run(context: AgentContext) -> AgentResult`: Core logic of the agent.
- `save_result(result: AgentResult, context: AgentContext)`: Persist results to the database.

## 2. AgentContext

The `AgentContext` passed to agents contains:
- `run_id`: The migration run ID.
- `db_session`: Database session for queries.
- `files`: Dict of file paths to content.
- `previous_results`: Dict of agent name to result data from completed agents.
- `additional_call_context`: Dict of extra parameters passed via `/rerun` API (e.g., user messages for chatbot).

## 3. Agent Registration

Agents must be explicitly registered in `app/services/agent_loader.py`.

```python
from app.services.agents.my_new_agent import MyNewAgent

def register_all_agents():
    agents = [
        # ... other agents
        MyNewAgent
    ]
    for agent_cls in agents:
        AgentRegistry.register(agent_cls)
```

## 4. Execution Flow

### Automatic Execution (Migration Start)
Only agents with `auto_run=True` execute during normal migration.

### Manual Execution (Rerun API)
`POST /api/v1/migration/runs/{run_id}/rerun` allows triggering any registered agents:


```json
{
  "agents_to_rerun": ["context_extractor_agent", "chatbot_agent"],
  "additional_context": {"user_message": "What is the complexity?"}
}
```

Example for Fixing Code:
```json
{
  "agents_to_rerun": ["python_code_fixer_agent", "python_executor_agent", "code_comparison_agent"],
  "additional_context": {
    "fix_requests": [
        {
            "file_name": "BUBBLE-SORT.cbl",
            "error": "AssertionError: Expected 5 but got 3", 
            "user_feedback": "Fix the sorting logic"
        },
        {
            "file_name": "ANOTHER-FILE.cbl",
            "error": "SyntaxError...", 
            "user_feedback": "Fix indentation"
        }
    ]
  }
}
```

The system automatically:
1. Validates all auto-run agents completed
2. Finds dependent agents and resets them
3. Executes specified agents in dependency order

## 5. Best Practices

- **Explicit Dependencies**: Always list dependencies in the `dependencies` property.
- **Use `auto_run=False`**: For agents that should only run on demand (e.g., chatbot agents).
- **Access Additional Context**: Use `context.additional_call_context` to get parameters from `/rerun` API.
- **Prompt Management**: Use `self.load_prompt("filename.txt")` to load prompts from `app/prompts`.
- **Async calls**: Use `asyncio.Semaphore` and `asyncio.gather` for parallel file processing.
- **Progress Updates**: Use `await self.update_progress(percent, "message")` to report status.
