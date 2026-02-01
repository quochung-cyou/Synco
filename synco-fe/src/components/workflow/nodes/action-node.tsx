"use client";

import type { NodeProps } from "@xyflow/react";
import {
    AlertTriangle,
    Check,
    EyeOff,
    XCircle,
    Zap,
} from "lucide-react";
import { memo } from "react";
import { useAtomValue } from "jotai";
import { NodeContainer, NodeDescription, NodeTitle } from "./trigger-node";
import { cn } from "@/lib/utils";
import {
    executionLogsAtom,
    selectedExecutionIdAtom,
    type WorkflowNodeData,
} from "@/lib/workflow-store";
import { findActionById } from "@/lib/workflow-definitions";

type ActionNodeProps = NodeProps & {
    data?: WorkflowNodeData;
    id: string;
};

export const ActionNode = memo(({ data, selected, id }: ActionNodeProps) => {
    const selectedExecutionId = useAtomValue(selectedExecutionIdAtom);
    const executionLogs = useAtomValue(executionLogsAtom);

    if (!data) {
        return null;
    }

    const actionType = (data.config?.actionType as string) || "";
    const status = data.status;

    // Handle empty action type (new node without selected action)
    if (!actionType) {
        const isDisabled = data.enabled === false;
        return (
            <NodeContainer
                className={cn(
                    "flex h-48 w-48 flex-col items-center justify-center shadow-none transition-all duration-150 ease-out",
                    isDisabled && "opacity-50"
                )}
                handles={{ target: true, source: true }}
                status={status}
                selected={selected}
            >
                {isDisabled && (
                    <div className="absolute top-2 left-2 rounded-full bg-gray-500/50 p-1">
                        <EyeOff className="size-3.5 text-white" />
                    </div>
                )}
                <div className="flex flex-col items-center justify-center gap-3 p-6">
                    <Zap className="size-12 text-muted-foreground" strokeWidth={1.5} />
                    <div className="flex flex-col items-center gap-1 text-center">
                        <NodeTitle className="text-base">
                            {data.label || "Action"}
                        </NodeTitle>
                        <NodeDescription className="text-xs">
                            Select an action
                        </NodeDescription>
                    </div>
                </div>
            </NodeContainer>
        );
    }

    const actionInfo = findActionById(actionType);
    const displayTitle = data.label || actionInfo?.label || actionType;
    const displayDescription = data.description || actionInfo?.description || "";

    const ActionIcon = actionInfo?.icon || Zap;
    const isDisabled = data.enabled === false;

    return (
        <NodeContainer
            className={cn(
                "relative flex h-48 w-48 flex-col items-center justify-center shadow-none transition-all duration-150 ease-out",
                isDisabled && "opacity-50"
            )}
            handles={{ target: true, source: true }}
            status={status}
            selected={selected}
        >
            {/* Disabled badge in top left */}
            {isDisabled && (
                <div className="absolute top-2 left-2 rounded-full bg-gray-500/50 p-1">
                    <EyeOff className="size-3.5 text-white" />
                </div>
            )}

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
                <ActionIcon className="size-12 text-primary" strokeWidth={1.5} />
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

ActionNode.displayName = "ActionNode";
