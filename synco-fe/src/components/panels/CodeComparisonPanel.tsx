import { useState } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { migrationRunAtom, updateMigrationStatusAtom } from '@/lib/migration-store';
import { rerunAgents } from '@/lib/api';
import { Code, CheckCircle2, ChevronDown, ChevronUp, GitCompare, Terminal, FileCode, Play, Loader2 } from 'lucide-react';
import { toast } from "sonner";
import { Card } from '@/components/ui/card';
import { Badge } from "@/components/ui/badge";
import { Button } from '@/components/ui/button';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import { oneDark } from 'react-syntax-highlighter/dist/esm/styles/prism';
import { motion, AnimatePresence } from 'motion/react';

interface ConvertedFile {
    fileName: string;
    cobolCode: string;
    pythonCode: string;
}

export function CodeComparisonPanel() {
    const migrationRun = useAtomValue(migrationRunAtom);
    const updateMigrationStatus = useSetAtom(updateMigrationStatusAtom);
    const [expandedFile, setExpandedFile] = useState<string | null>(null);
    const [selectedTab, setSelectedTab] = useState<'side-by-side' | 'cobol' | 'python'>('side-by-side');
    const [isVerifying, setIsVerifying] = useState(false);

    const pythonLogicResult = migrationRun?.agent_results?.find(
        (r: any) => r.agent_name === 'python_logic_generator_agent'
    );

    const jsonCleanerResult = migrationRun?.agent_results?.find(
        (r: any) => r.agent_name === 'json_cleaner_agent'
    );

    const convertedFiles: ConvertedFile[] = [];

    if (pythonLogicResult?.data?.converted_files && jsonCleanerResult?.data) {
        Object.entries(pythonLogicResult.data.converted_files).forEach(([fileName, pythonCode]) => {
            const cobolData = jsonCleanerResult.data[fileName];
            if (cobolData?.code) {
                convertedFiles.push({
                    fileName,
                    cobolCode: cobolData.code,
                    pythonCode: pythonCode as string
                });
            }
        });
    }

    if (!migrationRun || convertedFiles.length === 0) return null;

    const toggleFile = (fileName: string) => {
        setExpandedFile(expandedFile === fileName ? null : fileName);
    };

    const handleVerify = async () => {
        if (!migrationRun) return;

        setIsVerifying(true);
        try {
            await rerunAgents(
                migrationRun.id,
                ["python_executor_agent", "cobol_executor_agent", "code_comparison_agent"],
                {}
            );

            toast.success("Verification started", {
                description: "Agents queued for execution."
            });

            // Poll until agents are completed
            while (true) {
                await new Promise(resolve => setTimeout(resolve, 1000));
                const updatedRun = await updateMigrationStatus(migrationRun.id);

                if (!updatedRun?.agent_results) continue;

                const agentsToCheck = ["python_executor_agent", "cobol_executor_agent", "code_comparison_agent"];

                const allCompleted = agentsToCheck.every(agentName => {
                    const result = updatedRun.agent_results.find((r: any) => r.agent_name === agentName);

                    if (!result) return false; // Agent result not present yet

                    const isSkipped = result.status === "COMPLETED" &&
                        result.data?.skipped === true &&
                        result.data?.reason === "Not auto-run agent";

                    if (isSkipped) return false; // Wait if it's the "not auto-run" skipped state (haven't started properly or previous state)

                    return result.status === "COMPLETED" || result.status === "FAILED";
                });

                if (allCompleted) {
                    break;
                }
            }

            toast.success("Verification completed");

        } catch (error) {
            console.error("Verification failed:", error);
            toast.error("Verification failed", {
                description: "Failed to start verification agents."
            });
        } finally {
            setIsVerifying(false);
        }
    };

    const customStyle = {
        ...oneDark,
        'pre[class*="language-"]': {
            ...oneDark['pre[class*="language-"]'],
            margin: 0,
            borderRadius: '0.5rem',
            fontSize: '0.75rem',
            background: 'hsl(var(--background))',
        },
        'code[class*="language-"]': {
            ...oneDark['code[class*="language-"]'],
            fontSize: '0.75rem',
            fontFamily: 'JetBrains Mono, Monaco, Consolas, monospace',
        }
    };

    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5 flex flex-col">
            <div className="flex items-center justify-between mb-4">
                <div className="flex items-center gap-2">
                    <div className="relative">
                        <GitCompare className="h-4 w-4 text-primary" />
                        <div className="absolute -top-1 -right-1 w-2 h-2 bg-green-500 rounded-full animate-pulse" />
                    </div>
                    <h3 className="font-semibold text-sm">Code Comparison</h3>
                </div>
                <Badge variant="secondary" className="text-xs bg-gradient-to-r from-blue-500/20 to-purple-500/20 border-0">
                    {convertedFiles.length} {convertedFiles.length === 1 ? 'file' : 'files'}
                </Badge>
            </div>

            <ScrollArea className="flex-1 -mx-2 px-2">
                <div className="space-y-3">
                    {convertedFiles.map((file) => (
                        <motion.div
                            key={file.fileName}
                            initial={{ opacity: 0, y: 10 }}
                            animate={{ opacity: 1, y: 0 }}
                            className="rounded-lg border border-border/50 overflow-hidden bg-background/50"
                        >
                            <button
                                onClick={() => toggleFile(file.fileName)}
                                className="w-full flex items-center justify-between p-3 hover:bg-muted/50 transition-colors"
                            >
                                <div className="flex items-center gap-2">
                                    <FileCode className="h-4 w-4 text-amber-500" />
                                    <span className="text-sm font-medium">{file.fileName}</span>
                                    <Badge variant="outline" className="text-[10px] h-4 px-1.5 bg-gradient-to-r from-amber-500/10 to-orange-500/10 border-amber-500/30 text-amber-400">
                                        COBOL
                                    </Badge>
                                    <span className="text-muted-foreground">→</span>
                                    <Badge variant="outline" className="text-[10px] h-4 px-1.5 bg-gradient-to-r from-blue-500/10 to-cyan-500/10 border-blue-500/30 text-blue-400">
                                        Python
                                    </Badge>
                                </div>
                                <div className="flex items-center gap-2">
                                    <Badge
                                        variant="outline"
                                        className="text-[10px] h-5 px-1.5 bg-green-500/10 border-green-500/30 text-green-400"
                                    >
                                        <CheckCircle2 className="h-3 w-3 mr-1" />
                                        Converted
                                    </Badge>
                                    {expandedFile === file.fileName ? (
                                        <ChevronUp className="h-4 w-4 text-muted-foreground" />
                                    ) : (
                                        <ChevronDown className="h-4 w-4 text-muted-foreground" />
                                    )}
                                </div>
                            </button>

                            <AnimatePresence>
                                {expandedFile === file.fileName && (
                                    <motion.div
                                        initial={{ height: 0, opacity: 0 }}
                                        animate={{ height: 'auto', opacity: 1 }}
                                        exit={{ height: 0, opacity: 0 }}
                                        transition={{ duration: 0.2 }}
                                        className="border-t border-border/50"
                                    >
                                        <div className="p-3">
                                            <Tabs value={selectedTab} onValueChange={(v) => setSelectedTab(v as any)} className="w-full">
                                                <TabsList className="mb-3 w-full justify-start bg-muted/50">
                                                    <TabsTrigger value="side-by-side" className="text-xs gap-1.5">
                                                        <GitCompare className="h-3 w-3" />
                                                        Side by Side
                                                    </TabsTrigger>
                                                    <TabsTrigger value="cobol" className="text-xs gap-1.5">
                                                        <Terminal className="h-3 w-3" />
                                                        COBOL
                                                    </TabsTrigger>
                                                    <TabsTrigger value="python" className="text-xs gap-1.5">
                                                        <Code className="h-3 w-3" />
                                                        Python
                                                    </TabsTrigger>
                                                </TabsList>

                                                <TabsContent value="side-by-side" className="mt-0">
                                                    <div className="grid grid-cols-2 gap-3">
                                                        <div className="space-y-2">
                                                            <div className="flex items-center gap-2 mb-2">
                                                                <div className="w-2 h-2 rounded-full bg-amber-500" />
                                                                <span className="text-xs font-medium text-muted-foreground">COBOL Source</span>
                                                            </div>
                                                            <div className="rounded-lg overflow-hidden border border-border/30 max-h-[400px] overflow-y-auto">
                                                                <SyntaxHighlighter
                                                                    language="cobol"
                                                                    style={customStyle}
                                                                    showLineNumbers
                                                                    lineNumberStyle={{
                                                                        color: 'hsl(var(--muted-foreground))',
                                                                        opacity: 0.5,
                                                                        fontSize: '0.65rem',
                                                                        minWidth: '2em'
                                                                    }}
                                                                    customStyle={{
                                                                        margin: 0,
                                                                        padding: '0.75rem',
                                                                        background: 'hsl(var(--background))',
                                                                    }}
                                                                >
                                                                    {file.cobolCode}
                                                                </SyntaxHighlighter>
                                                            </div>
                                                        </div>
                                                        <div className="space-y-2">
                                                            <div className="flex items-center gap-2 mb-2">
                                                                <div className="w-2 h-2 rounded-full bg-blue-500" />
                                                                <span className="text-xs font-medium text-muted-foreground">Python Output</span>
                                                            </div>
                                                            <div className="rounded-lg overflow-hidden border border-border/30 max-h-[400px] overflow-y-auto">
                                                                <SyntaxHighlighter
                                                                    language="python"
                                                                    style={customStyle}
                                                                    showLineNumbers
                                                                    lineNumberStyle={{
                                                                        color: 'hsl(var(--muted-foreground))',
                                                                        opacity: 0.5,
                                                                        fontSize: '0.65rem',
                                                                        minWidth: '2em'
                                                                    }}
                                                                    customStyle={{
                                                                        margin: 0,
                                                                        padding: '0.75rem',
                                                                        background: 'hsl(var(--background))',
                                                                    }}
                                                                >
                                                                    {file.pythonCode}
                                                                </SyntaxHighlighter>
                                                            </div>
                                                        </div>
                                                    </div>
                                                </TabsContent>

                                                <TabsContent value="cobol" className="mt-0">
                                                    <div className="rounded-lg overflow-hidden border border-border/30 max-h-[500px] overflow-y-auto">
                                                        <SyntaxHighlighter
                                                            language="cobol"
                                                            style={customStyle}
                                                            showLineNumbers
                                                            lineNumberStyle={{
                                                                color: 'hsl(var(--muted-foreground))',
                                                                opacity: 0.5,
                                                                fontSize: '0.65rem',
                                                                minWidth: '2.5em'
                                                            }}
                                                            customStyle={{
                                                                margin: 0,
                                                                padding: '1rem',
                                                                background: 'hsl(var(--background))',
                                                            }}
                                                        >
                                                            {file.cobolCode}
                                                        </SyntaxHighlighter>
                                                    </div>
                                                </TabsContent>

                                                <TabsContent value="python" className="mt-0">
                                                    <div className="rounded-lg overflow-hidden border border-border/30 max-h-[500px] overflow-y-auto">
                                                        <SyntaxHighlighter
                                                            language="python"
                                                            style={customStyle}
                                                            showLineNumbers
                                                            lineNumberStyle={{
                                                                color: 'hsl(var(--muted-foreground))',
                                                                opacity: 0.5,
                                                                fontSize: '0.65rem',
                                                                minWidth: '2.5em'
                                                            }}
                                                            customStyle={{
                                                                margin: 0,
                                                                padding: '1rem',
                                                                background: 'hsl(var(--background))',
                                                            }}
                                                        >
                                                            {file.pythonCode}
                                                        </SyntaxHighlighter>
                                                    </div>
                                                </TabsContent>
                                            </Tabs>
                                        </div>
                                    </motion.div>
                                )}
                            </AnimatePresence>
                        </motion.div>
                    ))}
                </div>
            </ScrollArea>

            <div className="mt-4 pt-4 border-t border-border/50">
                <Button
                    variant="outline"
                    className="w-full h-10 border-dashed hover:bg-muted/50 transition-all text-muted-foreground hover:text-foreground"
                    onClick={handleVerify}
                    disabled={isVerifying}
                >
                    {isVerifying ? (
                        <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                    ) : (
                        <Play className="h-4 w-4 mr-2" />
                    )}
                    <span className="text-sm font-medium">
                        {isVerifying ? "Verifying..." : "Verify Logic Equivalence"}
                    </span>
                </Button>
            </div>
        </Card>
    );
}
