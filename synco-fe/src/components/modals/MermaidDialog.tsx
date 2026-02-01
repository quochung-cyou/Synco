
import {
    Dialog,
    DialogContent,
    DialogHeader,
    DialogTitle,
    DialogDescription,
} from "@/components/ui/dialog";
import { MermaidDiagram } from "@/components/ui/mermaid-diagram";
import { GitBranch } from 'lucide-react';

interface MermaidDialogProps {
    isOpen: boolean;
    onClose: () => void;
    chart: string;
}

export function MermaidDialog({
    isOpen,
    onClose,
    chart,
}: MermaidDialogProps) {
    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-5xl h-[80vh] flex flex-col p-0 gap-0">
                <DialogHeader className="px-6 py-4 border-b border-border/50">
                    <DialogTitle className="flex items-center gap-2">
                        <GitBranch className="h-5 w-5 text-primary" />
                        System Architecture Diagram
                    </DialogTitle>
                    <DialogDescription>
                        Visual representation of the system structure and dependencies
                    </DialogDescription>
                </DialogHeader>
                <div className="flex-1 p-6 overflow-auto bg-background/50 flex items-center justify-center">
                    <MermaidDiagram chart={chart} />
                </div>
            </DialogContent>
        </Dialog>
    );
}
