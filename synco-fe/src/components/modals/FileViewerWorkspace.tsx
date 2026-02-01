
import { Button } from "@/components/ui/button";
import { FileViewerContent } from "./FileViewerContent";
import { X, SplitSquareHorizontal, Code as CodeIcon, FileText } from "lucide-react";
import { useState, useEffect } from "react";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";

interface FileViewerWorkspaceProps {
    isOpen: boolean;
    onClose: () => void;
    fileName: string;
    content: string;
    code?: string;
}

export function FileViewerWorkspace({
    isOpen,
    onClose,
    fileName,
    content,
    code
}: FileViewerWorkspaceProps) {
    const [viewMode, setViewMode] = useState<"split" | "analysis" | "code">(code ? "split" : "analysis");

    // Prevent body scroll when open
    useEffect(() => {
        if (isOpen) {
            document.body.style.overflow = 'hidden';
        } else {
            document.body.style.overflow = 'unset';
        }
        return () => { document.body.style.overflow = 'unset'; };
    }, [isOpen]);

    if (!isOpen) return null;

    return (
        <div className="fixed inset-0 z-50 bg-background flex flex-col animate-in fade-in duration-200">
            {/* Header Toolbar */}
            <div className="h-14 border-b border-border/50 flex items-center justify-between px-6 bg-muted/10 flex-shrink-0">
                <div className="flex items-center gap-4">
                    <div className="flex items-center gap-2">
                        <div className="h-8 w-8 rounded bg-primary/10 flex items-center justify-center text-primary">
                            <CodeIcon className="h-5 w-5" />
                        </div>
                        <div>
                            <h2 className="font-semibold text-sm">{fileName}</h2>
                            <p className="text-[10px] text-muted-foreground uppercase tracking-wider">Zen Workspace</p>
                        </div>
                    </div>
                </div>

                <div className="flex items-center gap-4">
                    {code && (
                        <Tabs value={viewMode} onValueChange={(v) => setViewMode(v as any)} className="w-[300px]">
                            <TabsList className="grid w-full grid-cols-3">
                                <TabsTrigger value="split" className="text-xs">
                                    <SplitSquareHorizontal className="h-3 w-3 mr-2" /> Split
                                </TabsTrigger>
                                <TabsTrigger value="code" className="text-xs">
                                    <CodeIcon className="h-3 w-3 mr-2" /> Code
                                </TabsTrigger>
                                <TabsTrigger value="analysis" className="text-xs">
                                    <FileText className="h-3 w-3 mr-2" /> Docs
                                </TabsTrigger>
                            </TabsList>
                        </Tabs>
                    )}

                    <Button variant="ghost" size="icon" onClick={onClose} className="hover:bg-destructive/10 hover:text-destructive rounded-full">
                        <X className="h-5 w-5" />
                    </Button>
                </div>
            </div>

            {/* Main Content Area */}
            <div className="flex-1 w-full min-h-0 bg-background/50 backdrop-blur-3xl">
                <FileViewerContent
                    content={content}
                    code={code}
                    viewMode={viewMode}
                />
            </div>

            {/* Status Bar */}
            <div className="h-6 border-t border-border/50 bg-[#007acc] text-white flex items-center px-4 text-[10px] font-mono justify-between flex-shrink-0">
                <div className="flex items-center gap-4">
                    <span>Ready</span>
                    <span>UTF-8</span>
                    <span>COBOL</span>
                </div>
                <div className="flex items-center gap-2">
                    <span className="opacity-80">Workspace Mode</span>
                </div>
            </div>
        </div>
    );
}
