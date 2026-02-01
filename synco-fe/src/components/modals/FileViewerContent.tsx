
import { ScrollArea } from "@/components/ui/scroll-area";
import { FileText, Code as CodeIcon } from "lucide-react";
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism';

interface FileViewerContentProps {
    content: string;
    code?: string;
    viewMode?: "split" | "analysis" | "code";
    className?: string;
}

export function FileViewerContent({
    content,
    code,
    viewMode = "split",
    className
}: FileViewerContentProps) {
    const mode = code ? viewMode : "analysis";

    if (mode === 'split' && code) {
        return (
            <div className={`grid grid-cols-2 h-full w-full divide-x divide-border/50 ${className || ''}`}>
                <div className="h-full flex flex-col bg-[#1e1e1e] min-w-0 overflow-hidden">
                    <div className="px-4 py-2 bg-muted/20 border-b border-border/10 text-xs font-mono text-muted-foreground flex items-center gap-2 flex-shrink-0">
                        <CodeIcon className="h-3 w-3" /> Source Code
                    </div>
                    <div className="flex-1 w-full min-h-0 relative">
                        <ScrollArea className="h-full w-full">
                            <SyntaxHighlighter
                                language="cobol"
                                style={vscDarkPlus}
                                customStyle={{ margin: 0, padding: '1.5rem', background: 'transparent', fontSize: '13px', minHeight: '100%', width: '100%' }}
                                showLineNumbers={true}
                                wrapLines={true}
                            >
                                {code}
                            </SyntaxHighlighter>
                        </ScrollArea>
                    </div>
                </div>

                <div className="h-full flex flex-col bg-background min-w-0 overflow-hidden">
                    <div className="px-4 py-2 bg-muted/20 border-b border-border/50 text-xs font-medium text-muted-foreground flex items-center gap-2 flex-shrink-0">
                        <FileText className="h-3 w-3" /> Analysis Result
                    </div>
                    <div className="flex-1 w-full min-h-0 relative">
                        <ScrollArea className="h-full w-full">
                            <div className="p-6 prose prose-sm dark:prose-invert max-w-none pb-20">
                                <ReactMarkdown remarkPlugins={[remarkGfm]}>
                                    {content}
                                </ReactMarkdown>
                            </div>
                        </ScrollArea>
                    </div>
                </div>
            </div>
        );
    }

    if (mode === 'code' && code) {
        return (
            <div className={`h-full flex flex-col bg-[#1e1e1e] min-w-0 w-full overflow-hidden ${className || ''}`}>
                <div className="flex-1 w-full min-h-0 relative">
                    <ScrollArea className="h-full w-full">
                        <SyntaxHighlighter
                            language="cobol"
                            style={vscDarkPlus}
                            customStyle={{ margin: 0, padding: '1.5rem', background: 'transparent', fontSize: '14px', minHeight: '100%' }}
                            showLineNumbers={true}
                        >
                            {code}
                        </SyntaxHighlighter>
                    </ScrollArea>
                </div>
            </div>
        );
    }

    // Default: Analysis Only
    return (
        <div className={`h-full flex flex-col bg-background min-w-0 w-full overflow-hidden ${className || ''}`}>
            <ScrollArea className="flex-1 h-full w-full">
                <div className="p-8 max-w-4xl mx-auto prose prose-sm dark:prose-invert pb-20">
                    <ReactMarkdown remarkPlugins={[remarkGfm]}>
                        {content}
                    </ReactMarkdown>
                </div>
            </ScrollArea>
        </div>
    );
}
