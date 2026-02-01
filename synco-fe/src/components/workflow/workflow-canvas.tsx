"use client";

import {
    Background,
    Controls,
    ReactFlow,
    useEdgesState,
    useNodesState,
    type Connection,
    type Edge,
    type Node,
    type NodeTypes,
    type OnConnect,
    addEdge,
    useReactFlow,
    ReactFlowProvider,
} from "@xyflow/react";
import { useAtom, useAtomValue, useSetAtom } from "jotai";
import { nanoid } from "nanoid";
import { useCallback, useEffect, useMemo, useState } from "react";
import "@xyflow/react/dist/style.css";

import { Canvas } from "@/components/ai-elements/canvas";
import { Connection as ConnectionLine } from "@/components/ai-elements/connection";
import { Controls as ZoomControls } from "@/components/ai-elements/controls";
import { Edge as CustomEdge } from "@/components/ai-elements/edge";
import { Panel } from "@/components/ai-elements/panel";
import { ActionNode } from "@/components/workflow/nodes/action-node";
import { AddNode } from "@/components/workflow/nodes/add-node";
import { TriggerNode } from "@/components/workflow/nodes/trigger-node";
import { WorkflowContextMenu, useContextMenuHandlers, type ContextMenuState } from "@/components/workflow/workflow-context-menu";
import { WorkflowToolbar } from "@/components/workflow/workflow-toolbar";
import { NodeConfigPanel } from "@/components/workflow/node-config-panel";

import {
    addNodeAtom,
    deleteEdgeAtom,
    deleteNodeAtom,
    edgesAtom,
    nodesAtom,
    onEdgesChangeAtom,
    onNodesChangeAtom,
    selectedNodeAtom,
    updateNodeDataAtom,
    type WorkflowNode,
} from "@/lib/workflow-store";

const nodeTypes: NodeTypes = {
    trigger: TriggerNode,
    action: ActionNode,
    add: AddNode,
};

const edgeTypes = {
    temporary: CustomEdge.Temporary,
    animated: CustomEdge.Animated,
};

type WorkflowCanvasProps = {
    onRun?: () => void;
    isRunning?: boolean;
};

function WorkflowCanvasContent({ onRun, isRunning }: WorkflowCanvasProps) {
    const [nodes, setNodes] = useAtom(nodesAtom);
    const [edges, setEdges] = useAtom(edgesAtom);
    const [selectedNodeId, setSelectedNodeId] = useAtom(selectedNodeAtom);

    const onNodesChange = useSetAtom(onNodesChangeAtom);
    const onEdgesChange = useSetAtom(onEdgesChangeAtom);
    const addNode = useSetAtom(addNodeAtom);

    const { screenToFlowPosition } = useReactFlow();

    const [menuState, setMenuState] = useState<ContextMenuState>(null);
    const { onNodeContextMenu, onEdgeContextMenu, onPaneContextMenu } =
        useContextMenuHandlers(screenToFlowPosition, setMenuState);

    const onConnect: OnConnect = useCallback(
        (connection) => {
            const edge: Edge = {
                ...connection,
                id: nanoid(),
                type: "animated",
                animated: true,
            };
            setEdges((els) => addEdge(edge, els));
        },
        [setEdges]
    );

    const onInit = useCallback(() => {
        // Ensure we have a trigger node if empty
        if (nodes.length === 0) {
            const triggerNode: WorkflowNode = {
                id: nanoid(),
                type: "trigger",
                position: { x: 100, y: 100 },
                data: {
                    label: "Start",
                    description: "Trigger the workflow",
                    type: "trigger",
                    config: { triggerType: "Manual" },
                    status: "idle",
                },
            };
            // And an add node
            const addNode: WorkflowNode = {
                id: nanoid(),
                type: "add",
                position: { x: 400, y: 100 },
                data: {
                },
            };

            setNodes([triggerNode, addNode]);
        }
    }, [nodes.length, setNodes]);

    const onNodeClick = useCallback((event: React.MouseEvent, node: Node) => {
        if (node.type === "add") {
            const newNodeWidth = 200;
            const gap = 50;

            const newNode: WorkflowNode = {
                id: nanoid(),
                type: "action",
                position: { x: node.position.x, y: node.position.y },
                data: {
                    label: "Action",
                    type: "action",
                    config: {},
                    status: "idle"
                },
            };

            const updatedAddNode = {
                ...node,
                position: { x: node.position.x + newNodeWidth + gap, y: node.position.y }
            };

            addNode(newNode);
            setNodes((prev) => prev.map(n => n.id === node.id ? updatedAddNode : n).concat(newNode));

        } else {
            setSelectedNodeId(node.id);
        }
    }, [addNode, setNodes, setSelectedNodeId]);

    return (
        <div className="flex h-full w-full overflow-hidden">
            <div className="relative flex-1 h-full">
                <WorkflowToolbar onRun={onRun} isRunning={isRunning} />
                <Canvas
                    nodeTypes={nodeTypes}
                    edgeTypes={edgeTypes}
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    onConnect={onConnect}
                    onInit={onInit}
                    onNodeContextMenu={onNodeContextMenu}
                    onEdgeContextMenu={onEdgeContextMenu}
                    onPaneContextMenu={onPaneContextMenu}
                    onNodeClick={onNodeClick}
                    connectionLineComponent={ConnectionLine}
                >
                    <ZoomControls />
                    <Background />
                </Canvas>
                <WorkflowContextMenu
                    menuState={menuState}
                    onClose={() => setMenuState(null)}
                />
            </div>

            {/* Right Properties Panel */}
            {selectedNodeId && (
                <NodeConfigPanel />
            )}
        </div>
    );
}

export function WorkflowCanvas(props: WorkflowCanvasProps) {
    return (
        <ReactFlowProvider>
            <WorkflowCanvasContent {...props} />
        </ReactFlowProvider>
    );
}
