from typing import List, Optional, Dict, Any
from datetime import datetime
from pydantic import BaseModel

class DependencyBase(BaseModel):
    source_file: str
    target_file: str
    dependency_type: str
    line_number: Optional[int] = None
    context: Optional[str] = None

class DependencyDTO(DependencyBase):
    id: int
    class Config:
        from_attributes = True

class UserStoryDTO(BaseModel):
    story_id: Optional[str]
    title: Optional[str]
    role: Optional[str]
    action: Optional[str]
    benefit: Optional[str]
    acceptance_criteria_json: Optional[List[str]] = None
    class Config:
        from_attributes = True

class FeatureDTO(BaseModel):
    feature_id: Optional[str]
    name: Optional[str]
    description: Optional[str]
    inputs_json: Optional[List[str]] = None
    outputs_json: Optional[List[str]] = None
    processing_steps_json: Optional[List[str]] = None
    class Config:
        from_attributes = True

class BusinessRuleDTO(BaseModel):
    rule_id: Optional[str]
    description: Optional[str]
    condition: Optional[str]
    source_location: Optional[str]
    class Config:
        from_attributes = True

class BusinessLogicDTO(BaseModel):
    id: int
    business_purpose: Optional[str]
    raw_analysis: Optional[str]
    user_stories: List[UserStoryDTO] = []
    features: List[FeatureDTO] = []
    business_rules: List[BusinessRuleDTO] = []
    class Config:
        from_attributes = True

class AnalysisDTO(BaseModel):
    id: int
    program_description: Optional[str]
    data_divisions_json: Optional[Dict[str, Any]] = None
    procedure_divisions_json: Optional[Dict[str, Any]] = None
    variables_json: Optional[List[Dict[str, Any]]] = None
    class Config:
        from_attributes = True

class CobolFileDTO(BaseModel):
    id: int
    file_name: str
    file_path: str
    is_copybook: bool
    
    analysis: Optional[AnalysisDTO] = None
    business_logic: Optional[BusinessLogicDTO] = None
    
    agent_results: Optional[Dict[str, Any]] = None

    class Config:
        from_attributes = True

class AgentResultDTO(BaseModel):
    agent_name: str
    status: str
    file_path: Optional[str] = None
    data: Optional[Any] = None
    error: Optional[str] = None
    execution_time: int = 0
    progress: int = 0
    current_action: Optional[str] = None
    created_at: datetime
    
    class Config:
        from_attributes = True

class MigrationRunDetail(BaseModel):
    id: int
    started_at: datetime
    completed_at: Optional[datetime]
    status: str
    cobol_source_path: Optional[str]
    notes: Optional[str]
    
    agent_results: List[AgentResultDTO] = []

    class Config:
        from_attributes = True
