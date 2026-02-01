
import { Sheet, SheetContent, SheetHeader, SheetTitle, SheetDescription } from "@/components/ui/sheet";
import { FileViewerContent } from "./FileViewerContent";
import { SplitSquareHorizontal, Code as CodeIcon, FileText } from "lucide-react";
import { useState } from "react";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";

interface FileViewerSheetProps {
    isOpen: boolean;
    onClose: () => void;
    fileName: string;
    content: string;
    code?: string;
}

export function FileViewerSheet({
    isOpen,
    onClose,
    fileName,
    content,
    code
}: FileViewerSheetProps) {
    const [viewMode, setViewMode] = useState<"split" | "analysis" | "code">(code ? "split" : "analysis");

    return (
        <Sheet open={isOpen} onOpenChange={onClose}>
            <SheetContent className="w-[85vw] sm:max-w-none p-0 gap-0 border-l border-border/50 flex flex-col bg-background/95 backdrop-blur-xl supports-[backdrop-filter]:bg-background/60">
                <SheetHeader className="px-6 py-4 border-b border-border/50 flex flex-row items-center justify-between space-y-0">
                    <div className="flex flex-col gap-1">
                        <SheetTitle className="flex items-center gap-2 text-lg">
                            <FileText className="h-5 w-5 text-primary" />
                            {fileName}
                        </SheetTitle>
                        <SheetDescription>
                            Detailed analysis and source inspection
                        </SheetDescription>
                    </div>

                    <div className="flex items-center gap-4 pr-6">
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
                                        Docs
                                    </TabsTrigger>
                                </TabsList>
                            </Tabs>
                        )}
                        {/* Sheet built-in close button is sometimes tricky to position with custom headers, using SheetClose or default X logic. 
                            Default X is top-right. We can just let standard Sheet handle close via overlay click or standard X. 
                        */}
                    </div>
                </SheetHeader>

                <div className="flex-1 w-full min-h-0 overflow-hidden">
                    <FileViewerContent
                        content={content}
                        code={code}
                        viewMode={viewMode}
                    />
                </div>
            </SheetContent>
        </Sheet>
    );
}
