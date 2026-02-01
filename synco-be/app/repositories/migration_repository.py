from sqlalchemy.orm import Session
from app.models.migration import MigrationRun, CobolFile, Analysis, Dependency
from typing import List, Optional
from datetime import datetime

class MigrationRepository:
    def __init__(self, db: Session):
        self.db = db

    def create_run(self, cobol_source_path: str, target_output_path: str = "") -> MigrationRun:
        run = MigrationRun(
            status="Running",
            cobol_source_path=cobol_source_path,
            target_output_path=target_output_path
        )
        self.db.add(run)
        self.db.commit()
        self.db.refresh(run)
        return run

    def get_run(self, run_id: int) -> Optional[MigrationRun]:
        return self.db.query(MigrationRun).filter(MigrationRun.id == run_id).first()

    def get_run_with_details(self, run_id: int) -> Optional[MigrationRun]:
        from sqlalchemy.orm import joinedload
        
        return (
            self.db.query(MigrationRun)
            .options(joinedload(MigrationRun.agent_results))
            .filter(MigrationRun.id == run_id)
            .first()
        )

    def complete_run(self, run_id: int, status: str, notes: str = None):
        run = self.get_run(run_id)
        if run:
            run.status = status
            run.completed_at = datetime.utcnow()
            if notes:
                run.notes = notes
            self.db.commit()
            self.db.refresh(run)
        return run

    def save_cobol_file(self, run_id: int, file_name: str, file_path: str, content: str, is_copybook: bool = False) -> CobolFile:
        cobol_file = CobolFile(
            run_id=run_id,
            file_name=file_name,
            file_path=file_path,
            content=content,
            is_copybook=is_copybook
        )
        self.db.add(cobol_file)
        self.db.commit()
        self.db.refresh(cobol_file)
        return cobol_file

    def get_cobol_files(self, run_id: int) -> List[CobolFile]:
        return self.db.query(CobolFile).filter(CobolFile.run_id == run_id).all()

    def save_analysis(self, cobol_file_id: int, analysis_data: dict) -> Analysis:
        # Check if analysis exists
        existing = self.db.query(Analysis).filter(Analysis.cobol_file_id == cobol_file_id).first()
        if existing:
            # Update existing
            for key, value in analysis_data.items():
                if hasattr(existing, key):
                    setattr(existing, key, value)
            self.db.commit()
            self.db.refresh(existing)
            return existing
        else:
            # Create new
            analysis = Analysis(cobol_file_id=cobol_file_id, **analysis_data)
            self.db.add(analysis)
            self.db.commit()
            self.db.refresh(analysis)
            return analysis

    def save_dependency(self, run_id: int, source: str, target: str, dep_type: str, line: int = 0, context: str = ""):
        dep = Dependency(
            run_id=run_id,
            source_file=source,
            target_file=target,
            dependency_type=dep_type,
            line_number=line,
            context=context
        )
        self.db.add(dep)
        self.db.commit()
        return dep
    
    def save_business_logic(self, cobol_file_id: int, logic_data: dict):
        from app.models.migration import BusinessLogic, UserStory, Feature, BusinessRule
        
        # Check existing
        existing = self.db.query(BusinessLogic).filter(BusinessLogic.cobol_file_id == cobol_file_id).first()
        if existing:
            self.db.delete(existing)
            self.db.commit()
        
        # Create Main Logic Entry
        biz_logic = BusinessLogic(
            cobol_file_id=cobol_file_id,
            business_purpose=logic_data.get("business_purpose"),
            raw_analysis=logic_data.get("raw_analysis")
        )
        self.db.add(biz_logic)
        self.db.commit()
        self.db.refresh(biz_logic)
        
        # Save User Stories
        for us_data in logic_data.get("user_stories", []):
            us = UserStory(
                business_logic_id=biz_logic.id,
                story_id=us_data.get("id"),
                title=us_data.get("title"),
                role=us_data.get("role"),
                action=us_data.get("action"),
                benefit=us_data.get("benefit"),
                acceptance_criteria_json=us_data.get("acceptance_criteria")
            )
            self.db.add(us)
            
        # Save Features
        for f_data in logic_data.get("features", []):
            feat = Feature(
                business_logic_id=biz_logic.id,
                feature_id=f_data.get("id"),
                name=f_data.get("name"),
                description=f_data.get("description"),
                inputs_json=f_data.get("inputs"),
                outputs_json=f_data.get("outputs"),
                processing_steps_json=f_data.get("processing_steps")
            )
            self.db.add(feat)
            
        # Save Rules (Note: Feature rules might be separate, extracting generic rules here)
        for r_data in logic_data.get("business_rules", []):
            rule = BusinessRule(
                business_logic_id=biz_logic.id,
                rule_id=r_data.get("id"),
                description=r_data.get("description"),
                condition=r_data.get("condition"),
                source_location=r_data.get("source_location")
            )
            self.db.add(rule)
            
        self.db.commit()
        return biz_logic

    def create_agent_result(self, run_id: int, agent_name: str, status: str = "RUNNING"):
        from app.models.migration import AgentResult
        agent_result = AgentResult(
            run_id=run_id,
            agent_name=agent_name,
            status=status,
            progress=0
        )
        self.db.add(agent_result)
        self.db.commit()
        self.db.refresh(agent_result)
        return agent_result
    
    def update_agent_result(self, run_id: int, agent_name: str, status: str, data: dict = None, error: str = None, progress: int = 0, current_action: str = None):
        from app.models.migration import AgentResult
        agent_result = self.db.query(AgentResult).filter(
            AgentResult.run_id == run_id,
            AgentResult.agent_name == agent_name
        ).first()
        
        if not agent_result:
            agent_result = AgentResult(
                run_id=run_id,
                agent_name=agent_name,
                status=status,
                progress=progress
            )
            self.db.add(agent_result)
        
        agent_result.status = status
        agent_result.progress = progress
        if data is not None:
            agent_result.data = data
        if error:
            agent_result.error = error
        if current_action:
            agent_result.current_action = current_action
        self.db.commit()
        self.db.refresh(agent_result)
        return agent_result

    def reset_agent_results(self, run_id: int, agent_names: List[str], exclude_agents: List[str] = None):
        from app.models.migration import AgentResult
        agents_to_reset = [a for a in agent_names if a not in (exclude_agents or [])]
        
        self.db.query(AgentResult).filter(
            AgentResult.run_id == run_id,
            AgentResult.agent_name.in_(agents_to_reset)
        ).delete(synchronize_session=False)
        self.db.commit()
        
        for agent_name in agents_to_reset:
            self.create_agent_result(run_id, agent_name, "PENDING")

    def check_auto_run_agents_completed(self, run_id: int, auto_run_agents: List[str]) -> bool:
        from app.models.migration import AgentResult
        for agent_name in auto_run_agents:
            result = self.db.query(AgentResult).filter(
                AgentResult.run_id == run_id,
                AgentResult.agent_name == agent_name
            ).first()
            if not result or result.status != "COMPLETED":
                return False
        return True

    def get_chat_history(self, run_id: int) -> List[dict]:
        from app.models.migration import AgentResult
        chat_results = self.db.query(AgentResult).filter(
            AgentResult.run_id == run_id,
            AgentResult.agent_name == "chatbot_agent",
            AgentResult.status == "COMPLETED"
        ).order_by(AgentResult.created_at).all()
        
        history = []
        for result in chat_results:
            if result.data:
                history.append({
                    "user_message": result.data.get("user_message", ""),
                    "assistant_response": result.data.get("assistant_response", ""),
                    "created_at": result.created_at.isoformat() if result.created_at else None
                })
        return history

    def get_agent_results_data(self, run_id: int, agent_names: List[str] = None) -> dict:
        from app.models.migration import AgentResult
        query = self.db.query(AgentResult).filter(
            AgentResult.run_id == run_id,
            AgentResult.status == "COMPLETED"
        )
        if agent_names:
            query = query.filter(AgentResult.agent_name.in_(agent_names))
        
        results = query.all()
        return {r.agent_name: r.data for r in results if r.data}

    def update_agent_converted_file(self, run_id: int, agent_name: str, file_path: str, content: str) -> bool:
        from app.models.migration import AgentResult
        from sqlalchemy.orm.attributes import flag_modified
        import copy
        
        agent_result = self.db.query(AgentResult).filter(
            AgentResult.run_id == run_id,
            AgentResult.agent_name == agent_name
        ).first()
        
        if agent_result and agent_result.data:
            data_copy = copy.deepcopy(agent_result.data)
            if "converted_files" in data_copy:
                data_copy["converted_files"][file_path] = content
                agent_result.data = data_copy
                flag_modified(agent_result, "data")
                self.db.commit()
                self.db.refresh(agent_result)
                return True
        return False
