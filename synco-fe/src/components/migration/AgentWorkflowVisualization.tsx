import { useEffect, useMemo } from "react";
import { useAtomValue, useSetAtom } from "jotai";
import {
    ReactFlow,
    Background,
    Controls,
    type Node,
    type Edge,
    ReactFlowProvider,
    useReactFlow,
} from "@xyflow/react";
import dagre from "dagre";
import "@xyflow/react/dist/style.css";

import { AgentNode } from "./AgentNode";
import {
    migrationRunAtom,
    startPollingAtom,
    stopPollingAtom,
} from "@/lib/migration-store";
import type { AgentStatus } from "@/lib/types/migration";

const nodeTypes = {
    agent: AgentNode,
};

const getLayoutedElements = (nodes, edges) => {
    const dagreGraph = new dagre.graphlib.Graph();
    dagreGraph.setDefaultEdgeLabel(() => ({}));
    dagreGraph.setGraph({ rankdir: "TB", ranksep: 80, nodesep: 60 });

    nodes.forEach((node) => {
        dagreGraph.setNode(node.id, { width: 250, height: 100 });
    });

    edges.forEach((edge) => {
        dagreGraph.setEdge(edge.source, edge.target);
    });

    dagre.layout(dagreGraph);

    const layoutedNodes = nodes.map((node) => {
        const nodeWithPosition = dagreGraph.node(node.id);
        return {
            ...node,
            position: {
                x: nodeWithPosition.x - 125,
                y: nodeWithPosition.y - 50,
            },
        };
    });

    return { nodes: layoutedNodes, edges };
};

function AgentWorkflowVisualizationContent() {
    const migrationRun = useAtomValue(migrationRunAtom);
    const startPolling = useSetAtom(startPollingAtom);
    const stopPolling = useSetAtom(stopPollingAtom);
    const { fitView } = useReactFlow();

    useEffect(() => {
        if (migrationRun) {
            startPolling(migrationRun.id);
        }

        return () => {
            stopPolling();
        };
    }, [migrationRun?.id, startPolling, stopPolling]);

    const { nodes, edges } = useMemo(() => {
        if (!migrationRun) {
            return { nodes: [], edges: [] };
        }

        const agentResultsMap = new Map(
            migrationRun.agent_results.map((result: any) => [result.agent_name, result])
        );

        const allAgentNames = Object.keys(migrationRun.agent_metadata);
        const flowNodes: Node[] = allAgentNames.map((agentName: string) => {
            const agentResult = agentResultsMap.get(agentName);
            const metadata = migrationRun.agent_metadata[agentName];
            const isOnDemand = !metadata?.auto_run;
            let status: AgentStatus = (agentResult?.status as AgentStatus) || "PENDING";
            if (isOnDemand && agentResult?.data?.skipped) {
                status = "SKIPPED" as AgentStatus;
            }

            return {
                id: agentName,
                type: "agent",
                position: { x: 0, y: 0 },
                data: {
                    agentName,
                    agentResult,
                    status,
                    progress: agentResult?.progress || 0,
                    current_action: isOnDemand && status === "SKIPPED" ? "On-demand agent" : agentResult?.current_action,
                    executionTime: agentResult?.execution_time || 0,
                    isOnDemand,
                },
            };
        });

        const flowEdges: Edge[] = [];
        const agentNameSet = new Set(allAgentNames);
        Object.entries(migrationRun.agent_metadata).forEach(([agentName, metadata]: [string, any]) => {
            metadata.dependencies.forEach((dependency: string) => {
                if (!agentNameSet.has(dependency)) return;
                const targetStatus = agentResultsMap.get(agentName)?.status;
                const isTargetSkipped = agentResultsMap.get(agentName)?.data?.skipped;
                flowEdges.push({
                    id: `${dependency}-${agentName}`,
                    source: dependency,
                    target: agentName,
                    type: "smoothstep",
                    animated: targetStatus === "RUNNING",
                    style: {
                        stroke: targetStatus === "COMPLETED"
                            ? isTargetSkipped ? "#8b5cf6" : "#22c55e"
                            : targetStatus === "RUNNING"
                                ? "#3b82f6"
                                : "#6b7280",
                        strokeWidth: 2,
                    },
                });
            });
        });

        return getLayoutedElements(flowNodes, flowEdges);
    }, [migrationRun]);

    useEffect(() => {
        if (nodes.length > 0) {
            setTimeout(() => {
                fitView({ padding: 0.2, duration: 800 });
            }, 100);
        }
    }, [nodes.length, fitView]);

    if (!migrationRun) {
        return (
            <div className="flex items-center justify-center h-full w-full">
                <p className="text-muted-foreground">No migration data available</p>
            </div>
        );
    }

    return (
        <div className="h-full w-full">
            <div className="absolute top-4 left-4 z-10 bg-background/95 backdrop-blur-sm border rounded-lg p-4 shadow-lg">
                <div className="space-y-2">
                    <div className="flex items-center gap-2">
                        <div className="w-3 h-3 rounded-full bg-green-500" />
                        <span className="text-sm font-medium">Run #{migrationRun.id}</span>
                    </div>
                    <div className="text-xs text-muted-foreground">
                        Status: <span className="font-semibold text-foreground">{migrationRun.status}</span>
                    </div>
                    <div className="text-xs text-muted-foreground">
                        Agents: {migrationRun.agent_results.filter((a: any) => a.status === "COMPLETED").length} / {migrationRun.agent_results.length}
                    </div>
                </div>
            </div>

            <ReactFlow
                nodes={nodes}
                edges={edges}
                nodeTypes={nodeTypes}
                fitView
                minZoom={0.1}
                maxZoom={1.5}
                defaultEdgeOptions={{
                    type: "smoothstep",
                }}
                proOptions={{ hideAttribution: true }}
                colorMode="dark"
            >
                <Background />
                <Controls />
            </ReactFlow>
        </div>
    );
}

export function AgentWorkflowVisualization() {
    return (
        <ReactFlowProvider>
            <AgentWorkflowVisualizationContent />
        </ReactFlowProvider>
    );
}
