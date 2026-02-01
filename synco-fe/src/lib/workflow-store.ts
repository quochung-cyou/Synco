import type { Edge, EdgeChange, Node, NodeChange } from "@xyflow/react";
import { applyEdgeChanges, applyNodeChanges } from "@xyflow/react";
import { atom } from "jotai";

// -- API HANDLER ATOMS (Abstracted) --
export type SaveHandler = (data: { nodes: WorkflowNode[]; edges: WorkflowEdge[] }) => Promise<void>;
export const saveHandlerAtom = atom<SaveHandler | null>(null);

export type WorkflowNodeType = "trigger" | "action" | "add";

export type WorkflowNodeData = {
    label: string;
    description?: string;
    type: WorkflowNodeType;
    config?: Record<string, unknown>;
    status?: "idle" | "running" | "success" | "error";
    enabled?: boolean;
    onClick?: () => void;
    // Extra fields for visual state
    [key: string]: unknown;
};

export type WorkflowNode = Node<WorkflowNodeData>;
export type WorkflowEdge = Edge;

export type WorkflowVisibility = "private" | "public";

// Atoms for workflow state
export const nodesAtom = atom<WorkflowNode[]>([]);
export const edgesAtom = atom<WorkflowEdge[]>([]);
export const selectedNodeAtom = atom<string | null>(null);
export const selectedEdgeAtom = atom<string | null>(null);
export const isExecutingAtom = atom(false);
export const isLoadingAtom = atom(false);
export const isGeneratingAtom = atom(false);
export const currentWorkflowIdAtom = atom<string | null>(null);
export const currentWorkflowNameAtom = atom<string>("");
export const currentWorkflowVisibilityAtom = atom<WorkflowVisibility>("private");
export const isWorkflowOwnerAtom = atom<boolean>(true);

// UI state atoms
export const propertiesPanelActiveTabAtom = atom<string>("properties");
export const showMinimapAtom = atom(false);
export const selectedExecutionIdAtom = atom<string | null>(null);
export const rightPanelWidthAtom = atom<string | null>(null);
export const isPanelAnimatingAtom = atom<boolean>(false);
export const hasSidebarBeenShownAtom = atom<boolean>(false);
export const isSidebarCollapsedAtom = atom<boolean>(false);
export const isTransitioningFromHomepageAtom = atom<boolean>(false);

export const pendingIntegrationNodesAtom = atom<Set<string>>(new Set<string>());
export const newlyCreatedNodeIdAtom = atom<string | null>(null);
export const triggerExecuteAtom = atom(false);

export type ExecutionLogEntry = {
    nodeId: string;
    nodeName: string;
    nodeType: string;
    status: "pending" | "running" | "success" | "error";
    output?: unknown;
};

export const executionLogsAtom = atom<Record<string, ExecutionLogEntry>>({});

// Autosave functionality
let autosaveTimeoutId: NodeJS.Timeout | null = null;
const AUTOSAVE_DELAY = 1000;

export const hasUnsavedChangesAtom = atom(false);

export const autosaveAtom = atom(
    null,
    async (get, set, options?: { immediate?: boolean }) => {
        const handler = get(saveHandlerAtom);
        // Don't error if no handler, just skip
        if (!handler) {
            set(hasUnsavedChangesAtom, true); // Keep marked as unsaved
            return;
        }

        const workflowId = get(currentWorkflowIdAtom);
        const nodes = get(nodesAtom);
        const edges = get(edgesAtom);

        // Only autosave if we have a workflow ID - debatable for generic component, 
        // but useful to prevent saving before creation. 
        // For generic component, maybe we always save if handler exists?
        // Let's rely on handler existence.

        const saveFunc = async () => {
            try {
                await handler({ nodes, edges });
                set(hasUnsavedChangesAtom, false);
            } catch (error) {
                console.error("Autosave failed:", error);
            }
        };

        if (options?.immediate) {
            await saveFunc();
        } else {
            if (autosaveTimeoutId) {
                clearTimeout(autosaveTimeoutId);
            }
            autosaveTimeoutId = setTimeout(saveFunc, AUTOSAVE_DELAY);
        }
    }
);

// Derived atoms for node/edge operations
export const onNodesChangeAtom = atom(
    null,
    (get, set, changes: NodeChange[]) => {
        const currentNodes = get(nodesAtom);

        const filteredChanges = changes.filter((change) => {
            if (change.type === "remove") {
                const nodeToRemove = currentNodes.find((n) => n.id === change.id);
                return nodeToRemove?.data.type !== "trigger";
            }
            return true;
        });

        const newNodes = applyNodeChanges(
            filteredChanges,
            currentNodes
        ) as WorkflowNode[];
        set(nodesAtom, newNodes);

        const selectedNode = newNodes.find((n) => n.selected);
        if (selectedNode) {
            set(selectedNodeAtom, selectedNode.id);
            set(selectedEdgeAtom, null);
            const newlyCreatedId = get(newlyCreatedNodeIdAtom);
            if (newlyCreatedId && newlyCreatedId !== selectedNode.id) {
                set(newlyCreatedNodeIdAtom, null);
            }
        } else if (get(selectedNodeAtom)) {
            const currentSelection = get(selectedNodeAtom);
            const stillExists = newNodes.find((n) => n.id === currentSelection);
            if (!stillExists) {
                set(selectedNodeAtom, null);
            }
            set(newlyCreatedNodeIdAtom, null);
        }

        const hadDeletions = filteredChanges.some(
            (change) => change.type === "remove"
        );
        if (hadDeletions) {
            set(autosaveAtom, { immediate: true });
            return;
        }

        const hadPositionChanges = filteredChanges.some(
            (change) => change.type === "position" && change.dragging === false
        );
        if (hadPositionChanges) {
            set(autosaveAtom);
        }
    }
);

export const onEdgesChangeAtom = atom(
    null,
    (get, set, changes: EdgeChange[]) => {
        const currentEdges = get(edgesAtom);
        const newEdges = applyEdgeChanges(changes, currentEdges) as WorkflowEdge[];
        set(edgesAtom, newEdges);

        const selectedEdge = newEdges.find((e) => e.selected);
        if (selectedEdge) {
            set(selectedEdgeAtom, selectedEdge.id);
            set(selectedNodeAtom, null);
        } else if (get(selectedEdgeAtom)) {
            const currentSelection = get(selectedEdgeAtom);
            const stillExists = newEdges.find((e) => e.id === currentSelection);
            if (!stillExists) {
                set(selectedEdgeAtom, null);
            }
        }

        const hadDeletions = changes.some((change) => change.type === "remove");
        if (hadDeletions) {
            set(autosaveAtom, { immediate: true });
        }
    }
);

export type HistoryState = {
    nodes: WorkflowNode[];
    edges: WorkflowEdge[];
};

const historyAtom = atom<HistoryState[]>([]);
const futureAtom = atom<HistoryState[]>([]);

export const addNodeAtom = atom(null, (get, set, node: WorkflowNode) => {
    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);
    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);
    set(futureAtom, []);

    const updatedNodes = currentNodes.map((n) => ({ ...n, selected: false }));
    const newNode = { ...node, selected: true };
    const newNodes = [...updatedNodes, newNode];
    set(nodesAtom, newNodes);

    set(selectedNodeAtom, node.id);

    if (node.data.type === "action" && !node.data.config?.actionType) {
        set(newlyCreatedNodeIdAtom, node.id);
    }

    set(hasUnsavedChangesAtom, true);
    set(autosaveAtom, { immediate: true });
});

export const updateNodeDataAtom = atom(
    null,
    (get, set, { id, data }: { id: string; data: Partial<WorkflowNodeData> }) => {
        const currentNodes = get(nodesAtom);
        const oldNode = currentNodes.find((node) => node.id === id);
        const oldLabel = oldNode?.data.label;
        const newLabel = data.label;
        const isLabelChange = newLabel !== undefined && oldLabel !== newLabel;

        const newNodes = currentNodes.map((node) => {
            if (node.id === id) {
                return { ...node, data: { ...node.data, ...data } };
            }

            if (isLabelChange && oldLabel) {
                const updatedConfig = updateTemplatesInConfig(
                    node.data.config || {},
                    id,
                    oldLabel,
                    newLabel
                );

                if (updatedConfig !== node.data.config) {
                    return {
                        ...node,
                        data: {
                            ...node.data,
                            config: updatedConfig,
                        },
                    };
                }
            }

            return node;
        });

        set(nodesAtom, newNodes);

        if (!data.status) {
            set(hasUnsavedChangesAtom, true);
            set(autosaveAtom);
        }
    }
);

function updateTemplatesInConfig(
    config: Record<string, unknown>,
    nodeId: string,
    oldLabel: string,
    newLabel: string
): Record<string, unknown> {
    let hasChanges = false;
    const updated: Record<string, unknown> = {};

    for (const [key, value] of Object.entries(config)) {
        if (typeof value === "string") {
            const pattern = new RegExp(
                `\\{\\{@${escapeRegex(nodeId)}:${escapeRegex(oldLabel)}(\\.[^}]+)?\\}\\}`,
                "g"
            );
            const newValue = value.replace(pattern, (_match, fieldPart) => {
                hasChanges = true;
                return `{{@${nodeId}:${newLabel}${fieldPart || ""}}}`;
            });
            updated[key] = newValue;
        } else if (
            typeof value === "object" &&
            value !== null &&
            !Array.isArray(value)
        ) {
            const nestedUpdated = updateTemplatesInConfig(
                value as Record<string, unknown>,
                nodeId,
                oldLabel,
                newLabel
            );
            if (nestedUpdated !== value) {
                hasChanges = true;
            }
            updated[key] = nestedUpdated;
        } else {
            updated[key] = value;
        }
    }

    return hasChanges ? updated : config;
}

function escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

export const deleteNodeAtom = atom(null, (get, set, nodeId: string) => {
    const currentNodes = get(nodesAtom);
    const nodeToDelete = currentNodes.find((node) => node.id === nodeId);
    if (nodeToDelete?.data.type === "trigger") {
        return;
    }

    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);
    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);
    set(futureAtom, []);

    const newNodes = currentNodes.filter((node) => node.id !== nodeId);
    const newEdges = currentEdges.filter(
        (edge) => edge.source !== nodeId && edge.target !== nodeId
    );

    set(nodesAtom, newNodes);
    set(edgesAtom, newEdges);

    if (get(selectedNodeAtom) === nodeId) {
        set(selectedNodeAtom, null);
    }

    set(hasUnsavedChangesAtom, true);
    set(autosaveAtom, { immediate: true });
});

export const deleteEdgeAtom = atom(null, (get, set, edgeId: string) => {
    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);
    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);
    set(futureAtom, []);

    const newEdges = currentEdges.filter((edge) => edge.id !== edgeId);
    set(edgesAtom, newEdges);

    if (get(selectedEdgeAtom) === edgeId) {
        set(selectedEdgeAtom, null);
    }

    set(hasUnsavedChangesAtom, true);
    set(autosaveAtom, { immediate: true });
});

export const deleteSelectedItemsAtom = atom(null, (get, set) => {
    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);
    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);
    set(futureAtom, []);

    const selectedNodeIds = currentNodes
        .filter((node) => node.selected && node.data.type !== "trigger")
        .map((node) => node.id);

    const newNodes = currentNodes.filter((node) => {
        if (node.data.type === "trigger") return true;
        return !node.selected;
    });

    const newEdges = currentEdges.filter(
        (edge) =>
            !(
                edge.selected ||
                selectedNodeIds.includes(edge.source) ||
                selectedNodeIds.includes(edge.target)
            )
    );

    set(nodesAtom, newNodes);
    set(edgesAtom, newEdges);
    set(selectedNodeAtom, null);
    set(selectedEdgeAtom, null);

    set(hasUnsavedChangesAtom, true);
    set(autosaveAtom, { immediate: true });
});

export const undoAtom = atom(null, (get, set) => {
    const history = get(historyAtom);
    if (history.length === 0) return;

    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const future = get(futureAtom);

    set(futureAtom, [...future, { nodes: currentNodes, edges: currentEdges }]);

    const newHistory = [...history];
    const previousState = newHistory.pop();
    if (!previousState) return;
    set(historyAtom, newHistory);
    set(nodesAtom, previousState.nodes);
    set(edgesAtom, previousState.edges);

    set(hasUnsavedChangesAtom, true);
});

export const redoAtom = atom(null, (get, set) => {
    const future = get(futureAtom);
    if (future.length === 0) return;

    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);

    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);

    const newFuture = [...future];
    const nextState = newFuture.pop();
    if (!nextState) return;
    set(futureAtom, newFuture);
    set(nodesAtom, nextState.nodes);
    set(edgesAtom, nextState.edges);

    set(hasUnsavedChangesAtom, true);
});

export const canUndoAtom = atom((get) => get(historyAtom).length > 0);
export const canRedoAtom = atom((get) => get(futureAtom).length > 0);

export const clearWorkflowAtom = atom(null, (get, set) => {
    const currentNodes = get(nodesAtom);
    const currentEdges = get(edgesAtom);
    const history = get(historyAtom);
    set(historyAtom, [...history, { nodes: currentNodes, edges: currentEdges }]);
    set(futureAtom, []);

    set(nodesAtom, []);
    set(edgesAtom, []);
    set(selectedNodeAtom, null);
    set(selectedEdgeAtom, null);

    set(hasUnsavedChangesAtom, true);
    set(autosaveAtom, { immediate: true });
});
