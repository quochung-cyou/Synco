import { memo } from "react";
import { Handle, Position, type NodeProps } from "@xyflow/react";
import type { AgentResult, AgentStatus } from "@/lib/types/migration";
import {
    FileSearch,
    Network,
    FileCode,
    Filter,
    Sparkles,
    GitBranch,
    Copy,
    Phone,
    Repeat,
    FolderOpen,
    GitMerge,
    Database,
    BookOpen,
    Lightbulb,
    Workflow,
    Loader2,
    CheckCircle2,
    XCircle,
    Clock,
    MessageSquare,
} from "lucide-react";

interface AgentNodeData {
    agentName: string;
    agentResult?: AgentResult;
    status: AgentStatus;
    progress?: number;
    current_action?: string;
    executionTime?: number;
}

const AGENT_ICONS: Record<string, typeof FileSearch> = {
    cobol_analyzer: FileSearch,
    dependency_mapper: Network,
    structure_analysis_agent: Workflow,
    file_filter_agent: Filter,
    cobol_parser_agent: FileCode,
    json_cleaner_agent: Sparkles,
    copy_dependency_extractor_agent: Copy,
    call_dependency_extractor_agent: Phone,
    perform_dependency_extractor_agent: Repeat,
    file_op_dependency_extractor_agent: FolderOpen,
    dependency_aggregator_agent: GitMerge,
    data_collector_agent: Database,
    copybook_usage_analyzer_agent: BookOpen,
    structure_insights_agent: Lightbulb,
    mermaid_diagram_agent: GitBranch,
    complexity_analyzer_agent: Sparkles,
    python_conversion_strategy_agent: Workflow,
    python_logic_generator_agent: FileCode,
    dashboard_summary_agent: Database,
    context_extractor_agent: FileSearch,
    chatbot_agent: MessageSquare,
};

const STATUS_CONFIG = {
    PENDING: {
        color: "text-muted-foreground",
        bgColor: "bg-muted/50",
        borderColor: "border-muted",
        icon: Clock,
    },
    RUNNING: {
        color: "text-blue-500",
        bgColor: "bg-blue-500/10",
        borderColor: "border-blue-500/50",
        icon: Loader2,
    },
    COMPLETED: {
        color: "text-green-500",
        bgColor: "bg-green-500/10",
        borderColor: "border-green-500/50",
        icon: CheckCircle2,
    },
    FAILED: {
        color: "text-red-500",
        bgColor: "bg-red-500/10",
        borderColor: "border-red-500/50",
        icon: XCircle,
    },
    SKIPPED: {
        color: "text-violet-500",
        bgColor: "bg-violet-500/10",
        borderColor: "border-violet-500/50",
        icon: Clock,
    },
};

export const AgentNode = memo(({ data }: NodeProps) => {
    const { agentName, status, progress, executionTime, current_action } = data as unknown as AgentNodeData;

    const AgentIcon = AGENT_ICONS[agentName] || FileSearch;
    const statusConfig = STATUS_CONFIG[status as keyof typeof STATUS_CONFIG];
    const StatusIcon = statusConfig.icon;

    const formattedName = agentName
        .replace(/_/g, " ")
        .split(" ")
        .map((word: string) => word.charAt(0).toUpperCase() + word.slice(1))
        .join(" ");

    return (
        <div
            className={`
        relative px-4 py-3 rounded-lg border-2 shadow-lg
        ${statusConfig.bgColor} ${statusConfig.borderColor}
        transition-all duration-300
        ${status === "RUNNING" ? "animate-pulse" : ""}
        min-w-[220px] max-w-[280px]
      `}
        >
            <Handle
                type="target"
                position={Position.Top}
                className="!w-3 !h-3 !bg-primary !border-2 !border-background"
            />

            <div className="flex items-start gap-3">
                <div className={`p-2 rounded-md ${statusConfig.bgColor} ${statusConfig.color}`}>
                    <AgentIcon className="w-5 h-5" />
                </div>

                <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 mb-1">
                        <h3 className="font-semibold text-sm text-foreground truncate">
                            {formattedName}
                        </h3>
                        <StatusIcon
                            className={`w-4 h-4 flex-shrink-0 ${statusConfig.color} ${status === "RUNNING" ? "animate-spin" : ""
                                }`}
                        />
                    </div>

                    <div className="space-y-2">
                        {(progress !== undefined || status === "RUNNING") && (
                            <div className="w-full bg-muted/50 rounded-full h-1.5 overflow-hidden border border-muted">
                                <div
                                    className={`h-full ${statusConfig.bgColor} transition-all duration-300 ${status === "RUNNING" && progress === 0 ? "animate-pulse w-full opacity-50" : ""}`}
                                    style={{ width: `${Math.max(progress || 0, status === "RUNNING" ? 5 : 0)}%` }}
                                />
                            </div>
                        )}

                        {current_action && (
                            <p className="text-xs font-medium text-foreground/80 truncate max-w-[200px]" title={current_action}>
                                {current_action}
                            </p>
                        )}


                        {executionTime !== undefined && executionTime > 0 && (
                            <p className="text-xs text-muted-foreground">
                                {executionTime.toFixed(2)}s
                            </p>
                        )}
                    </div>
                </div>
            </div>

            <Handle
                type="source"
                position={Position.Bottom}
                className="!w-3 !h-3 !bg-primary !border-2 !border-background"
            />
        </div>
    );
});

AgentNode.displayName = "AgentNode";
