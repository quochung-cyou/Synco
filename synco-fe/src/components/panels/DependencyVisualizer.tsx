
import { Card } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { ReactFlow, ReactFlowProvider, Background, Controls } from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { useAtomValue } from "jotai";
import { migrationRunAtom } from "@/lib/migration-store";
import { useMemo } from "react";
import dagre from "dagre";
import { Network, GitGraph, Table2 } from "lucide-react";
import { MermaidDiagram } from "@/components/ui/mermaid-diagram";
import { ScrollArea } from "@/components/ui/scroll-area";

// --- Dependency Graph Helpers ---
const getLayoutedElements = (nodes: any[], edges: any[]) => {
    const dagreGraph = new dagre.graphlib.Graph();
    dagreGraph.setDefaultEdgeLabel(() => ({}));

    // Reduced ranksep and nodesep for tighter layout
    dagreGraph.setGraph({ rankdir: "LR", ranksep: 150, nodesep: 50 });

    nodes.forEach((node) => {
        dagreGraph.setNode(node.id, { width: 180, height: 40 });
    });

    edges.forEach((edge) => {
        dagreGraph.setEdge(edge.source, edge.target);
    });

    dagre.layout(dagreGraph);

    return {
        nodes: nodes.map((node) => {
            const nodeWithPosition = dagreGraph.node(node.id);
            return {
                ...node,
                position: {
                    x: nodeWithPosition.x - 90,
                    y: nodeWithPosition.y - 20,
                },
            };
        }),
        edges,
    };
};

export function DependencyVisualizer() {
    const migrationRun = useAtomValue(migrationRunAtom);

    // Extract dependency data from dependency_aggregator_agent
    const dependencyData = useMemo(() => {
        const aggregator = migrationRun?.agent_results?.find(
            (r: any) => r.agent_name === 'dependency_aggregator_agent'
        );
        return aggregator?.data || {};
    }, [migrationRun]);

    // Extract mermaid chart from mermaid_diagram_agent
    const mermaidChart = useMemo(() => {
        const agent = migrationRun?.agent_results?.find(
            (r: any) => r.agent_name === 'mermaid_diagram_agent'
        );
        return agent?.data?.mermaid_diagram || '';
    }, [migrationRun]);

    // Prepare Graph Data
    const { nodes, edges } = useMemo(() => {
        const rawNodes = Object.keys(dependencyData).map(file => ({
            id: file,
            data: { label: file },
            position: { x: 0, y: 0 },
            style: {
                background: '#1e293b',
                color: '#fff',
                border: '1px solid #334155',
                borderRadius: '8px',
                fontSize: '10px',
                width: 180,
                padding: '8px',
                textAlign: 'center'
            },
        }));

        const rawEdges: any[] = [];
        Object.entries(dependencyData).forEach(([source, deps]: [string, any]) => {
            const dependencies = deps as any[];
            if (!Array.isArray(dependencies)) return;

            // Limit dependencies to avoid graph explosion
            dependencies.slice(0, 20).forEach((dep, idx) => {
                const targetId = dep.target;
                if (!targetId) return;

                // Only add edge if target is also in our file list (internal dependencies)
                // or if we want to show external ones too. Let's show all for now but careful with IDs.
                // Simple approach: Add edge if target exists in keys, or add "External" node.
                const targetExists = dependencyData[targetId];

                rawEdges.push({
                    id: `e-${source}-${targetId}-${idx}`,
                    source: source,
                    target: targetId,
                    animated: true,
                    style: { stroke: '#64748b' }
                });

                if (!targetExists && !rawNodes.find(n => n.id === targetId)) {
                    rawNodes.push({
                        id: targetId,
                        data: { label: targetId },
                        position: { x: 0, y: 0 },
                        style: {
                            background: '#64748b',
                            color: '#fff',
                            fontSize: '8px',
                            width: 150,
                            padding: '4px',
                            border: '1px solid #475569',
                            borderRadius: '4px',
                            textAlign: 'center'
                        },
                    });
                }
            });
        });

        // Slice to max 50 nodes for performance in this demo
        const limitedNodes = rawNodes.slice(0, 50);
        const nodeIds = new Set(limitedNodes.map(n => n.id));
        const limitedEdges = rawEdges.filter(e => nodeIds.has(e.source) && nodeIds.has(e.target));

        return getLayoutedElements(limitedNodes, limitedEdges);
    }, [dependencyData]);

    return (
        <Card className="w-full h-[500px] shrink-0 flex flex-col overflow-hidden border-border/50 bg-card/40 backdrop-blur">
            <div className="p-3 border-b border-border/50 bg-muted/20">
                <h3 className="font-semibold text-sm flex items-center gap-2">
                    <Network className="h-4 w-4 text-primary" /> Dependency & Architecture
                </h3>
            </div>

            <Tabs defaultValue="graph" className="flex-1 flex flex-col">
                <div className="px-3 py-2 border-b border-border/50 bg-background/50">
                    <TabsList className="grid grid-cols-4 w-full h-8">
                        <TabsTrigger value="graph" className="text-xs"><GitGraph className="h-3 w-3 mr-2" /> Graph</TabsTrigger>
                        <TabsTrigger value="architecture" className="text-xs"><Network className="h-3 w-3 mr-2" /> Arch</TabsTrigger>
                        <TabsTrigger value="matrix" className="text-xs"><Table2 className="h-3 w-3 mr-2" /> Matrix</TabsTrigger>
                        <TabsTrigger value="list" className="text-xs"><Network className="h-3 w-3 mr-2" /> Tree</TabsTrigger>
                    </TabsList>
                </div>

                <TabsContent value="graph" className="flex-1 p-0 m-0 min-h-0 relative">
                    <div className="absolute inset-0">
                        {nodes.length > 0 ? (
                            <ReactFlowProvider>
                                <ReactFlow
                                    nodes={nodes}
                                    edges={edges}
                                    fitView
                                    proOptions={{ hideAttribution: true }}
                                >
                                    <Background color="#333" gap={16} />
                                    <Controls showInteractive={false} />
                                </ReactFlow>
                            </ReactFlowProvider>
                        ) : (
                            <div className="flex items-center justify-center h-full text-muted-foreground text-xs">
                                No dependency data available
                            </div>
                        )}
                    </div>
                </TabsContent>

                <TabsContent value="architecture" className="flex-1 p-0 m-0 min-h-0 overflow-auto bg-background/50 relative">
                    {mermaidChart ? (
                        <div className="p-4 min-w-full min-h-full inline-block">
                            <MermaidDiagram chart={mermaidChart} />
                        </div>
                    ) : (
                        <div className="flex items-center justify-center h-full text-muted-foreground text-xs">
                            Architecture diagram not available yet
                        </div>
                    )}
                </TabsContent>

                <TabsContent value="matrix" className="flex-1 p-4 m-0 min-h-0 overflow-auto">
                    <div className="text-xs text-muted-foreground text-center">
                        Dependency Matrix View
                        <div className="mt-4 grid grid-cols-4 gap-2">
                            {Object.keys(dependencyData).slice(0, 16).map(file => (
                                <div key={file} className="p-2 border rounded bg-muted/20 truncate" title={file}>
                                    {file}
                                </div>
                            ))}
                        </div>
                    </div>
                </TabsContent>

                <TabsContent value="list" className="flex-1 p-0 m-0 min-h-0">
                    <ScrollArea className="h-full p-4">
                        <div className="space-y-4">
                            {Object.entries(dependencyData).map(([source, deps]: [string, any]) => {
                                const dependencies = Array.isArray(deps) ? deps : [];

                                return (
                                    <div key={source} className="border rounded-lg p-3 bg-muted/10">
                                        <div className="font-medium text-sm mb-2 text-primary">{source}</div>
                                        <div className="grid grid-cols-1 gap-1 pl-2 border-l-2 border-border/50">
                                            {dependencies.length > 0 ? dependencies.map((dep: any, i: number) => (
                                                <div key={i} className="text-xs text-muted-foreground flex justify-between">
                                                    <span>{dep.target}</span>
                                                    <span className="opacity-50 text-[10px] uppercase border px-1 rounded">{dep.type}</span>
                                                </div>
                                            )) : (
                                                <div className="text-xs text-muted-foreground italic">No external dependencies</div>
                                            )}
                                        </div>
                                    </div>
                                )
                            })}
                        </div>
                    </ScrollArea>
                </TabsContent>
            </Tabs>
        </Card>
    );
}
