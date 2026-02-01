import { useAtomValue } from "jotai";
import { migrationRunAtom } from "@/lib/migration-store";
import { Card } from "@/components/ui/card";
import { GitBranch, Maximize2 } from "lucide-react";
import { MermaidDiagram } from "@/components/ui/mermaid-diagram";
import { Button } from "@/components/ui/button";
import { useState } from "react";
import { MermaidDialog } from "@/components/modals/MermaidDialog";

export function MermaidDiagramPanel() {
    const migrationRun = useAtomValue(migrationRunAtom);
    const [isDialogOpen, setIsDialogOpen] = useState(false);

    if (!migrationRun) return null;

    const mermaidResult = migrationRun.agent_results.find(
        (r: any) => r.agent_name === "mermaid_diagram_agent" && r.status === "COMPLETED"
    );

    if (!mermaidResult?.data?.mermaid_diagram) return null;

    const chart = mermaidResult.data.mermaid_diagram;

    return (
        <>
            <Card className="border-border/50 bg-card/40 backdrop-blur p-5 flex flex-col h-[400px]">
                <div className="flex items-center justify-between mb-4">
                    <h3 className="text-sm font-semibold flex items-center gap-2">
                        <GitBranch className="h-4 w-4 text-primary" />
                        Mermaid Diagram
                    </h3>
                    <Button
                        variant="ghost"
                        size="icon"
                        className="h-6 w-6"
                        onClick={() => setIsDialogOpen(true)}
                    >
                        <Maximize2 className="h-3 w-3" />
                    </Button>
                </div>

                <div className="flex-1 min-h-0 bg-background/50 rounded-lg overflow-auto border border-border/30 relative group">
                    {/* Added overflow-auto to contain the diagram and allow scrolling */}
                    <div className="p-2 min-w-full min-h-full inline-block">
                        <MermaidDiagram chart={chart} />
                    </div>
                </div>
            </Card>

            <MermaidDialog
                isOpen={isDialogOpen}
                onClose={() => setIsDialogOpen(false)}
                chart={chart}
            />
        </>
    );
}
