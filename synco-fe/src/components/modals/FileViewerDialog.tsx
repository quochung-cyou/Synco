
import {
    Dialog,
    DialogContent,
    DialogHeader,
    DialogTitle,
    DialogDescription,
} from "@/components/ui/dialog";
import { FileText, Code as CodeIcon, SplitSquareHorizontal } from "lucide-react";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { useState } from "react";
import { FileViewerContent } from "./FileViewerContent";

interface FileViewerDialogProps {
    isOpen: boolean;
    onClose: () => void;
    fileName: string;
    content: string;
    code?: string;
}

export function FileViewerDialog({
    isOpen,
    onClose,
    fileName,
    content,
    code
}: FileViewerDialogProps) {
    const [viewMode, setViewMode] = useState<"split" | "analysis" | "code">(code ? "split" : "analysis");

    return (
        <Dialog open={isOpen} onOpenChange={onClose}>
            <DialogContent className="max-w-[1400px] w-[95vw] h-[90vh] flex flex-col p-0 gap-0 overflow-hidden border-border/50">
                <DialogHeader className="px-6 py-4 border-b border-border/50 bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60 flex-shrink-0">
                    <div className="flex items-center justify-between">
                        <div>
                            <DialogTitle className="flex items-center gap-2">
                                <FileText className="h-5 w-5 text-primary" />
                                {fileName}
                            </DialogTitle>
                            <DialogDescription>
                                Analysis & Source Code
                            </DialogDescription>
                        </div>

                        {code && (
                            <Tabs value={viewMode} onValueChange={(v) => setViewMode(v as any)} className="w-[300px]">
                                <TabsList className="grid w-full grid-cols-3">
                                    <TabsTrigger value="split" className="text-xs">
                                        <SplitSquareHorizontal className="h-3 w-3 mr-2" />
                                        Split
                                    </TabsTrigger>
                                    <TabsTrigger value="code" className="text-xs">
                                        <CodeIcon className="h-3 w-3 mr-2" />
                                        Code
                                    </TabsTrigger>
                                    <TabsTrigger value="analysis" className="text-xs">
                                        Docs
                                    </TabsTrigger>
                                </TabsList>
                            </Tabs>
                        )}
                    </div>
                </DialogHeader>

                <div className="flex-1 min-h-0 w-full bg-background/50">
                    <FileViewerContent
                        content={content}
                        code={code}
                        viewMode={viewMode}
                    />
                </div>
            </DialogContent>
        </Dialog>
    );
}
