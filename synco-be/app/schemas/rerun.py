from pydantic import BaseModel
from typing import List, Dict, Any, Optional

class RerunRequest(BaseModel):
    agents_to_rerun: List[str]
    additional_context: Optional[Dict[str, Any]] = {}
