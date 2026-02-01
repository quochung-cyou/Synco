
import { useState } from 'react';
import { useAtomValue, useSetAtom } from 'jotai';
import { migrationRunAtom, updateMigrationStatusAtom } from '@/lib/migration-store';
import { rerunAgents } from '@/lib/api';
import { CheckCircle2, XCircle, AlertTriangle, ChevronDown, ChevronUp, Activity, Wrench, Loader2 } from 'lucide-react';
import { Card } from '@/components/ui/card';
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { toast } from "sonner";
import { ScrollArea } from '@/components/ui/scroll-area';
import { motion, AnimatePresence } from 'motion/react';
import { cn } from '@/lib/utils';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';

// Types matching the provided JSON structure
interface Difference {
    line: number;
    cobol: string;
    python: string;
}

interface ComparisonResult {
    passed: boolean;
    reason: string;
    file_name: string;
    cobol_lines: number;
    python_lines: number;
    differences: Difference[];
    cobol_output: string;
    python_output: string;
    cobol_available: boolean;
    python_available: boolean;
}

interface CodeComparisonData {
    summary: {
        total: number;
        failed: number;
        passed: number;
        pass_rate: number;
    };
    comparison_results: Record<string, ComparisonResult>;
}

export function OutputComparisonPanel() {
    const migrationRun = useAtomValue(migrationRunAtom);
    const updateMigrationStatus = useSetAtom(updateMigrationStatusAtom);
    const [expandedFile, setExpandedFile] = useState<string | null>(null);
    const [isFixing, setIsFixing] = useState(false);

    const comparisonResult = migrationRun?.agent_results?.find(
        (r: any) => r.agent_name === 'code_comparison_agent' && r.status === 'COMPLETED'
    );

    if (!comparisonResult?.data) return null;

    const data = comparisonResult.data as CodeComparisonData;

    const { summary, comparison_results } = data;

    if (!summary || !comparison_results) return null;

    const toggleFile = (fileName: string) => {
        setExpandedFile(expandedFile === fileName ? null : fileName);
    };

    const handleAutoFix = async () => {
        if (!migrationRun) return;

        const failedFiles = Object.values(comparison_results).filter(r => !r.passed);
        if (failedFiles.length === 0) {
            toast.info("No failed files to fix");
            return;
        }

        const fixRequests = failedFiles.map(file => {
            let detailedError = file.reason;
            if (file.differences && file.differences.length > 0) {
                const diffs = file.differences.map(d =>
                    `Line ${d.line}: Expected '${d.cobol}', but got '${d.python}'`
                ).join("\n");
                detailedError = `${file.reason}\nDetails:\n${diffs}`;
            }

            return {
                file_name: file.file_name,
                error: detailedError,
                user_feedback: "Fix the Python code so its output matches the COBOL output exactly."
            };
        });

        setIsFixing(true);
        try {
            await rerunAgents(
                migrationRun.id,
                ["python_code_fixer_agent"],
                {
                    fix_requests: fixRequests
                }
            );

            toast.success("Auto-fix started", {
                description: `Queued fixes for ${failedFiles.length} files.`
            });

            // Poll for status update
            while (true) {
                await new Promise(resolve => setTimeout(resolve, 1000));
                const updatedRun = await updateMigrationStatus(migrationRun.id);

                if (!updatedRun?.agent_results) continue;

                const result = updatedRun.agent_results.find((r: any) => r.agent_name === "python_code_fixer_agent");

                if (result) {
                    const isSkipped = result.status === "COMPLETED" &&
                        result.data?.skipped === true &&
                        result.data?.reason === "Not auto-run agent";

                    if (!isSkipped && (result.status === "COMPLETED" || result.status === "FAILED")) {
                        if (result.status === "FAILED") {
                            toast.error("Auto-fix execution failed");
                        } else {
                            toast.success("Auto-fix execution completed");
                        }
                        break;
                    }
                }
            }

        } catch (error) {
            console.error("Auto-fix failed:", error);
            toast.error("Auto-fix failed", {
                description: "Failed to start fix agent."
            });
        } finally {
            setIsFixing(false);
        }
    };

    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5 flex flex-col gap-4">
            <div className="flex items-center justify-between">
                <div className="flex items-center gap-2">
                    <div className="p-2 rounded-full bg-primary/10">
                        <Activity className="h-4 w-4 text-primary" />
                    </div>
                    <div>
                        <h3 className="font-semibold text-sm">Output Verification</h3>
                        <p className="text-xs text-muted-foreground">Execution results comparison</p>
                    </div>
                </div>
                <div className="flex items-center gap-3">
                    {summary.failed > 0 && (
                        <Button
                            size="sm"
                            variant="destructive"
                            onClick={handleAutoFix}
                            disabled={isFixing}
                            className="h-8 text-xs gap-2"
                        >
                            {isFixing ? (
                                <Loader2 className="h-3 w-3 animate-spin" />
                            ) : (
                                <Wrench className="h-3 w-3" />
                            )}
                            {isFixing ? "Fixing..." : "Auto-Fix Failed"}
                        </Button>
                    )}
                    <div className="text-right">
                        <div className="text-sm font-bold">{summary.pass_rate}%</div>
                        <div className="text-[10px] text-muted-foreground uppercase tracking-wider">Pass Rate</div>
                    </div>
                    <div className="h-8 w-8 relative flex items-center justify-center">
                        <div className="absolute inset-0 rounded-full border-2 border-muted/30" />
                        <div
                            className="absolute inset-0 rounded-full border-2 border-primary border-t-transparent -rotate-90"
                            style={{
                                clipPath: `polygon(0 0, 100% 0, 100% 100%, 0 100%)`,
                                transform: `rotate(${(summary.pass_rate / 100) * 360}deg)`
                            }}
                        />
                    </div>
                </div>
            </div>

            {/* Stats Grid */}
            <div className="grid grid-cols-3 gap-2">
                <div className="bg-background/50 rounded-lg p-2 border border-border/50 text-center">
                    <div className="text-lg font-bold">{summary.total}</div>
                    <div className="text-[10px] text-muted-foreground uppercase">Total</div>
                </div>
                <div className="bg-green-500/10 rounded-lg p-2 border border-green-500/20 text-center">
                    <div className="text-lg font-bold text-green-500">{summary.passed}</div>
                    <div className="text-[10px] text-green-600/70 uppercase">Passed</div>
                </div>
                <div className="bg-red-500/10 rounded-lg p-2 border border-red-500/20 text-center">
                    <div className="text-lg font-bold text-red-500">{summary.failed}</div>
                    <div className="text-[10px] text-red-600/70 uppercase">Failed</div>
                </div>
            </div>

            <ScrollArea className="h-[400px] -mx-2 px-2">
                <div className="space-y-2">
                    {Object.values(comparison_results).map((result) => (
                        <motion.div
                            key={result.file_name}
                            initial={{ opacity: 0, y: 5 }}
                            animate={{ opacity: 1, y: 0 }}
                            className={cn(
                                "group rounded-lg border overflow-hidden transition-all",
                                result.passed
                                    ? "border-green-500/20 bg-green-500/5 hover:bg-green-500/10"
                                    : "border-red-500/20 bg-red-500/5 hover:bg-red-500/10"
                            )}
                        >
                            <button
                                onClick={() => toggleFile(result.file_name)}
                                className="w-full flex items-center justify-between p-3"
                            >
                                <div className="flex items-center gap-3">
                                    {result.passed ? (
                                        <CheckCircle2 className="h-4 w-4 text-green-500" />
                                    ) : (
                                        <XCircle className="h-4 w-4 text-red-500" />
                                    )}
                                    <div className="flex flex-col items-start">
                                        <span className="text-sm font-medium">{result.file_name}</span>
                                        <span className={cn(
                                            "text-[10px]",
                                            result.passed ? "text-green-600/70" : "text-red-500/70"
                                        )}>
                                            {result.reason}
                                        </span>
                                    </div>
                                </div>
                                <div className="flex items-center gap-2">
                                    <Badge variant="outline" className="bg-background/50 h-5 px-1.5 text-[10px]">
                                        COBOL: {result.cobol_lines}L
                                    </Badge>
                                    <Badge variant="outline" className="bg-background/50 h-5 px-1.5 text-[10px]">
                                        Py: {result.python_lines}L
                                    </Badge>
                                    {expandedFile === result.file_name ? (
                                        <ChevronUp className="h-4 w-4 text-muted-foreground" />
                                    ) : (
                                        <ChevronDown className="h-4 w-4 text-muted-foreground" />
                                    )}
                                </div>
                            </button>

                            <AnimatePresence>
                                {expandedFile === result.file_name && (
                                    <motion.div
                                        initial={{ height: 0, opacity: 0 }}
                                        animate={{ height: 'auto', opacity: 1 }}
                                        exit={{ height: 0, opacity: 0 }}
                                        className="border-t border-border/30 bg-background/30"
                                    >
                                        <div className="p-3 space-y-3">
                                            {!result.passed && result.differences.length > 0 && (
                                                <div className="space-y-2">
                                                    <div className="flex items-center gap-2 text-xs font-semibold text-red-500">
                                                        <AlertTriangle className="h-3 w-3" />
                                                        Detected Differences
                                                    </div>
                                                    <div className="rounded border border-red-500/20 bg-red-500/5 divide-y divide-red-500/10">
                                                        {result.differences.map((diff, i) => (
                                                            <div key={i} className="p-2 text-xs grid grid-cols-[40px_1fr_1fr] gap-2">
                                                                <span className="text-muted-foreground font-mono">L{diff.line}</span>
                                                                <div className="font-mono text-amber-600/90 dark:text-amber-400 break-all">{diff.cobol}</div>
                                                                <div className="font-mono text-blue-600/90 dark:text-blue-400 break-all">{diff.python}</div>
                                                            </div>
                                                        ))}
                                                    </div>
                                                </div>
                                            )}

                                            <Tabs defaultValue={result.passed ? "cobol" : "diff"} className="w-full">
                                                <TabsList className="h-7 w-full justify-start bg-muted/50 p-0.5">
                                                    <TabsTrigger value="cobol" className="text-[10px] h-6 px-2">COBOL Output</TabsTrigger>
                                                    <TabsTrigger value="python" className="text-[10px] h-6 px-2">Python Output</TabsTrigger>
                                                </TabsList>
                                                <TabsContent value="cobol" className="mt-2">
                                                    <pre className="p-3 rounded-lg bg-black/5 dark:bg-white/5 text-[10px] font-mono leading-relaxed whitespace-pre-wrap">
                                                        {result.cobol_output}
                                                    </pre>
                                                </TabsContent>
                                                <TabsContent value="python" className="mt-2">
                                                    <pre className="p-3 rounded-lg bg-black/5 dark:bg-white/5 text-[10px] font-mono leading-relaxed whitespace-pre-wrap">
                                                        {result.python_output}
                                                    </pre>
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
        </Card>
    );
}
