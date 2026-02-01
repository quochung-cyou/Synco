from sqlalchemy import Column, Integer, String, Text, DateTime, ForeignKey, Boolean, JSON
from sqlalchemy.orm import relationship, declarative_base
from datetime import datetime

Base = declarative_base()

class AgentResult(Base):
    __tablename__ = "agent_results"

    id = Column(Integer, primary_key=True, index=True)
    run_id = Column(Integer, ForeignKey("runs.id"), nullable=False)
    agent_name = Column(String(100), nullable=False)
    status = Column(String(50), nullable=False)
    file_path = Column(Text, nullable=True)
    data = Column(JSON, nullable=True)
    error = Column(Text, nullable=True)
    execution_time = Column(Integer, default=0)
    progress = Column(Integer, default=0)
    current_action = Column(Text, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)

    run = relationship("MigrationRun", back_populates="agent_results")

class MigrationRun(Base):
    __tablename__ = "runs"

    id = Column(Integer, primary_key=True, index=True)
    started_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    completed_at = Column(DateTime, nullable=True)
    status = Column(String(50), nullable=False)
    cobol_source_path = Column(Text, nullable=True)
    target_output_path = Column(Text, nullable=True)
    notes = Column(Text, nullable=True)

    agent_results = relationship("AgentResult", back_populates="run", cascade="all, delete-orphan")
    cobol_files = relationship("CobolFile", back_populates="run", cascade="all, delete-orphan")
    dependencies = relationship("Dependency", back_populates="run", cascade="all, delete-orphan")
    metrics = relationship("Metric", back_populates="run", uselist=False, cascade="all, delete-orphan")


class CobolFile(Base):
    __tablename__ = "cobol_files"

    id = Column(Integer, primary_key=True, index=True)
    run_id = Column(Integer, ForeignKey("runs.id"), nullable=False)
    file_name = Column(String(255), nullable=False)
    file_path = Column(Text, nullable=False)
    is_copybook = Column(Boolean, default=False, nullable=False)
    content = Column(Text, nullable=True)

    run = relationship("MigrationRun", back_populates="cobol_files")
    analysis = relationship("Analysis", back_populates="cobol_file", uselist=False, cascade="all, delete-orphan")


class Analysis(Base):
    __tablename__ = "analyses"

    id = Column(Integer, primary_key=True, index=True)
    cobol_file_id = Column(Integer, ForeignKey("cobol_files.id"), nullable=False)
    
    program_description = Column(Text, nullable=True)
    raw_analysis = Column(Text, nullable=True)
    
    data_divisions_json = Column(JSON, nullable=True)
    procedure_divisions_json = Column(JSON, nullable=True)
    variables_json = Column(JSON, nullable=True)
    paragraphs_json = Column(JSON, nullable=True)
    copybooks_json = Column(JSON, nullable=True)

    cobol_file = relationship("CobolFile", back_populates="analysis")


class Dependency(Base):
    __tablename__ = "dependencies"

    id = Column(Integer, primary_key=True, index=True)
    run_id = Column(Integer, ForeignKey("runs.id"), nullable=False)
    
    source_file = Column(String(255), nullable=False)
    target_file = Column(String(255), nullable=False)
    dependency_type = Column(String(50), nullable=True)
    line_number = Column(Integer, nullable=True)
    context = Column(Text, nullable=True)

    run = relationship("MigrationRun", back_populates="dependencies")


class Metric(Base):
    __tablename__ = "metrics"

    id = Column(Integer, primary_key=True, index=True)
    run_id = Column(Integer, ForeignKey("runs.id"), nullable=False, unique=True)
    
    total_programs = Column(Integer, default=0)
    total_copybooks = Column(Integer, default=0)
    total_dependencies = Column(Integer, default=0)
    avg_dependencies_per_program = Column(Integer, default=0)
    
    analysis_insights = Column(Text, nullable=True)
    mermaid_diagram = Column(Text, nullable=True)

    run = relationship("MigrationRun", back_populates="metrics")


class BusinessLogic(Base):
    __tablename__ = "business_logic"

    id = Column(Integer, primary_key=True, index=True)
    cobol_file_id = Column(Integer, ForeignKey("cobol_files.id"), nullable=False)
    
    business_purpose = Column(Text, nullable=True)
    raw_analysis = Column(Text, nullable=True)

    cobol_file = relationship("CobolFile", back_populates="business_logic")
    user_stories = relationship("UserStory", back_populates="business_logic", cascade="all, delete-orphan")
    features = relationship("Feature", back_populates="business_logic", cascade="all, delete-orphan")
    business_rules = relationship("BusinessRule", back_populates="business_logic", cascade="all, delete-orphan")

CobolFile.business_logic = relationship("BusinessLogic", back_populates="cobol_file", uselist=False, cascade="all, delete-orphan")


class UserStory(Base):
    __tablename__ = "user_stories"

    id = Column(Integer, primary_key=True, index=True)
    business_logic_id = Column(Integer, ForeignKey("business_logic.id"), nullable=False)
    
    story_id = Column(String(50), nullable=True)
    title = Column(Text, nullable=True)
    role = Column(Text, nullable=True)
    action = Column(Text, nullable=True)
    benefit = Column(Text, nullable=True)
    acceptance_criteria_json = Column(JSON, nullable=True) 

    business_logic = relationship("BusinessLogic", back_populates="user_stories")


class Feature(Base):
    __tablename__ = "features"

    id = Column(Integer, primary_key=True, index=True)
    business_logic_id = Column(Integer, ForeignKey("business_logic.id"), nullable=False)
    
    feature_id = Column(String(50), nullable=True)
    name = Column(Text, nullable=True)
    description = Column(Text, nullable=True)
    inputs_json = Column(JSON, nullable=True)
    outputs_json = Column(JSON, nullable=True)
    processing_steps_json = Column(JSON, nullable=True)

    business_logic = relationship("BusinessLogic", back_populates="features")


class BusinessRule(Base):
    __tablename__ = "business_rules"

    id = Column(Integer, primary_key=True, index=True)
    business_logic_id = Column(Integer, ForeignKey("business_logic.id"), nullable=False)
    
    rule_id = Column(String(50), nullable=True)
    description = Column(Text, nullable=True)
    condition = Column(Text, nullable=True)
    source_location = Column(String(255), nullable=True)

    business_logic = relationship("BusinessLogic", back_populates="business_rules")
