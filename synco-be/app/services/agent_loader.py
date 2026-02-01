from app.services.agent_registry import AgentRegistry
from app.services.agents.cobol.cobol_parser_agent import CobolParserAgent
from app.services.agents.cobol.json_cleaner_agent import JSONCleanerAgent
from app.services.agents.dependency.call_dependency_extractor_agent import CallDependencyExtractorAgent
from app.services.agents.dependency.copy_dependency_extractor_agent import CopyDependencyExtractorAgent
from app.services.agents.dependency.dependency_aggregator_agent import DependencyAggregatorAgent
from app.services.agents.dependency.file_op_dependency_extractor_agent import FileOpDependencyExtractorAgent
from app.services.agents.dependency.perform_dependency_extractor_agent import PerformDependencyExtractorAgent
from app.services.agents.structure.copybook_usage_analyzer_agent import CopybookUsageAnalyzerAgent
from app.services.agents.structure.mermaid_diagram_agent import MermaidDiagramAgent
from app.services.agents.structure.structure_insights_agent import StructureInsightsAgent
from app.services.agents.analytics.complexity_analyzer_agent import ComplexityAnalyzerAgent
from app.services.agents.conversion.python_conversion_strategy_agent import PythonConversionStrategyAgent
from app.services.agents.conversion.python_logic_generator_agent import PythonLogicGeneratorAgent
from app.services.agents.conversion.python_code_fixer_agent import PythonCodeFixerAgent
from app.services.agents.reporting.dashboard_summary_agent import DashboardSummaryAgent
from app.services.agents.chatbot.context_extractor_agent import ContextExtractorAgent
from app.services.agents.chatbot.chatbot_agent import ChatbotAgent
from app.services.agents.chatbot.chat_history_agent import ChatHistoryAgent
from app.services.agents.testing.python_executor_agent import PythonExecutorAgent
from app.services.agents.testing.cobol_executor_agent import CobolExecutorAgent
from app.services.agents.testing.code_comparison_agent import CodeComparisonAgent

def register_all_agents():
    agents = [
        CobolParserAgent,
        JSONCleanerAgent,
        CallDependencyExtractorAgent,
        CopyDependencyExtractorAgent,
        DependencyAggregatorAgent,
        FileOpDependencyExtractorAgent,
        PerformDependencyExtractorAgent,
        CopybookUsageAnalyzerAgent,
        MermaidDiagramAgent,
        StructureInsightsAgent,
        ComplexityAnalyzerAgent,
        PythonConversionStrategyAgent,
        PythonLogicGeneratorAgent,
        PythonCodeFixerAgent,
        DashboardSummaryAgent,
        ContextExtractorAgent,
        ChatbotAgent,
        ChatHistoryAgent,
        PythonExecutorAgent,
        CobolExecutorAgent,
        CodeComparisonAgent
    ]

    for agent_cls in agents:
        AgentRegistry.register(agent_cls)

