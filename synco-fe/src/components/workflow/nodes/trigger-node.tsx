"use client";

import type { NodeProps } from "@xyflow/react";
import { Check, Clock, Play, Webhook, XCircle } from "lucide-react";
import { memo } from "react";
import { Position } from "@xyflow/react";
import { Handle } from "@xyflow/react";
import { cn } from "@/lib/utils";
import type { WorkflowNodeData } from "@/lib/workflow-store";

// Minimal Node UI components
export const NodeTitle = ({ className, children }: { className?: string, children: React.ReactNode }) => (
    <div className={cn("font-medium", className)}>{children}</div>
);

export const NodeDescription = ({ className, children }: { className?: string, children: React.ReactNode }) => (
    <div className={cn("text-muted-foreground", className)}>{children}</div>
);

export const NodeContainer = ({
    className,
    children,
    selected,
    status,
    handles
}: {
    className?: string,
    children: React.ReactNode,
    selected?: boolean,
    status?: string,
    handles: { target: boolean, source: boolean }
}) => (
    <div
        className={cn(
            "relative rounded-xl border bg-card text-card-foreground shadow-sm hover:shadow-md transition-all",
            selected && "ring-2 ring-primary border-primary",
            status === "error" && "border-red-500",
            status === "success" && "border-green-500",
            status === "running" && "border-blue-500 animate-pulse",
            className
        )}
    >
        {handles.target && (
            <Handle
                type="target"
                position={Position.Left}
                className="!w-4 !h-4 !bg-muted-foreground transition-colors hover:!bg-primary"
            />
        )}
        {children}
        {handles.source && (
            <Handle
                type="source"
                position={Position.Right}
                className="!w-4 !h-4 !bg-muted-foreground transition-colors hover:!bg-primary"
            />
        )}
    </div>
);


type TriggerNodeProps = NodeProps & {
    data?: WorkflowNodeData;
};

export const TriggerNode = memo(({ data, selected }: TriggerNodeProps) => {
    if (!data) {
        return null;
    }

    const triggerType = (data.config?.triggerType as string) || "Manual";
    const displayTitle = data.label || triggerType;
    const displayDescription = data.description || "Trigger";
    const status = data.status;

    // Select icon based on trigger type
    let TriggerIcon = Play;
    if (triggerType === "Schedule") {
        TriggerIcon = Clock;
    } else if (triggerType === "Webhook") {
        TriggerIcon = Webhook;
    }

    return (
        <NodeContainer
            className={cn(
                "flex h-48 w-48 flex-col items-center justify-center shadow-none transition-all duration-150 ease-out",
            )}
            handles={{ target: false, source: true }}
            status={status}
            selected={selected}
        >
            {/* Status indicator badge in top right */}
            {status && status !== "idle" && status !== "running" && (
                <div
                    className={cn(
                        "absolute top-2 right-2 rounded-full p-1",
                        status === "success" && "bg-green-500/50",
                        status === "error" && "bg-red-500/50"
                    )}
                >
                    {status === "success" && (
                        <Check className="size-3.5 text-white" strokeWidth={2.5} />
                    )}
                    {status === "error" && (
                        <XCircle className="size-3.5 text-white" strokeWidth={2.5} />
                    )}
                </div>
            )}

            <div className="flex flex-col items-center justify-center gap-3 p-6">
                <TriggerIcon className="size-12 text-blue-500" strokeWidth={1.5} />
                <div className="flex flex-col items-center gap-1 text-center">
                    <NodeTitle className="text-base">{displayTitle}</NodeTitle>
                    {displayDescription && (
                        <NodeDescription className="text-xs">
                            {displayDescription}
                        </NodeDescription>
                    )}
                </div>
            </div>
        </NodeContainer>
    );
});

TriggerNode.displayName = "TriggerNode";
