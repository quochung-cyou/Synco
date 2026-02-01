import { useState } from 'react';
import { useAtomValue } from 'jotai';
import { migrationRunAtom } from '@/lib/migration-store';
import { FileText, Search } from 'lucide-react';
import { Card } from '@/components/ui/card';
import { Badge } from "@/components/ui/badge";
import { ScrollArea } from '@/components/ui/scroll-area';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { FileViewerSheet } from "@/components/modals/FileViewerSheet";

export function FileAnalysisPanel() {
    const migrationRun = useAtomValue(migrationRunAtom);
    const [searchQuery, setSearchQuery] = useState('');
    const [selectedFile, setSelectedFile] = useState<{ name: string; content: string; code?: string } | null>(null);

    const cobolParserResult = migrationRun?.agent_results?.find(
        (r: any) => r.agent_name === 'cobol_parser_agent'
    );

    const files = cobolParserResult?.data && typeof cobolParserResult.data === 'object'
        ? Object.entries(cobolParserResult.data).map(([name, data]: [string, any]) => {
            // Handle both new structure {analysis, code} and old structure (string/dict)
            const analysis = typeof data === 'object' && data.analysis ? data.analysis : data;
            const code = typeof data === 'object' && data.code ? data.code : '';
            // If analysis is an object (from json_cleaner), stringify it for the viewer or keep as is?
            // The viewer expects string content currently. Let's ensure it's a string if it's an object.
            const contentStr = typeof analysis === 'object' ? JSON.stringify(analysis, null, 2) : String(analysis);

            return {
                name,
                content: contentStr,
                code: code
            };
        })
        : [];

    const filteredFiles = files.filter(file =>
        file.name.toLowerCase().includes(searchQuery.toLowerCase())
    );

    if (!migrationRun) return null;

    return (
        <>
            <Card className="border-border/50 bg-card/40 backdrop-blur p-5 flex flex-col max-h-[400px]">
                <div className="flex items-center justify-between mb-4">
                    <div className="flex items-center gap-2">
                        <FileText className="h-4 w-4 text-primary" />
                        <h3 className="font-semibold text-sm">Analyzed Files</h3>
                    </div>

                    <Badge variant="secondary" className="text-xs">
                        {files.length}
                    </Badge>
                </div>

                <div className="relative mb-3">
                    <Search className="absolute left-2 top-2.5 h-3.5 w-3.5 text-muted-foreground" />
                    <Input
                        placeholder="Search files..."
                        value={searchQuery}
                        onChange={(e) => setSearchQuery(e.target.value)}
                        className="pl-8 h-8 text-xs bg-background/50"
                    />
                </div>

                <ScrollArea className="flex-1 -mx-2 px-2">
                    <div className="space-y-1">
                        {filteredFiles.map((file) => (
                            <Button
                                key={file.name}
                                variant="ghost"
                                size="sm"
                                className="w-full justify-start text-xs font-normal h-8 truncate group"
                                onClick={() => setSelectedFile(file)}
                            >
                                <FileText className="h-3.5 w-3.5 mr-2 text-primary/70 shrink-0 group-hover:text-primary transition-colors" />
                                <span className="truncate">{file.name}</span>
                                {file.code && <Badge variant="outline" className="ml-auto text-[10px] h-4 px-1 text-muted-foreground">Code</Badge>}
                            </Button>
                        ))}
                        {filteredFiles.length === 0 && (
                            <div className="text-center py-8 text-muted-foreground text-xs">
                                No files found
                            </div>
                        )}
                    </div>
                </ScrollArea>
            </Card>

            {selectedFile && (
                <FileViewerSheet
                    isOpen={!!selectedFile}
                    onClose={() => setSelectedFile(null)}
                    fileName={selectedFile.name}
                    content={selectedFile.content}
                    code={selectedFile.code}
                />
            )}
        </>
    );
}
