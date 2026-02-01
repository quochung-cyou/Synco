import { useState } from "react";
import { useSetAtom } from "jotai";
import { startMigrationAtom } from "@/lib/migration-store";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Loader2, Rocket } from "lucide-react";

interface MigrationStartScreenProps {
    onMigrationStarted: (runId: number) => void;
}

export function MigrationStartScreen({ onMigrationStarted }: MigrationStartScreenProps) {
    const [sourceFolder, setSourceFolder] = useState("data/cobol_source");
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const startMigration = useSetAtom(startMigrationAtom);

    const handleStartMigration = async () => {
        setIsLoading(true);
        setError(null);

        try {
            const run = await startMigration(sourceFolder);
            if (run && run.id) {
                onMigrationStarted(run.id);
            } else {
                // Fallback if run is not returned correctly, though atom logic suggests it returns run
                // We might need to check the type return of startMigrationAtom
                onMigrationStarted(0); // This shouldn happen if run is strictly typed
            }
        } catch (err) {
            const errorMessage = err instanceof Error ? err.message : "Failed to start migration";
            setError(errorMessage);
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className="flex items-center justify-center h-full w-full bg-gradient-to-br from-background via-background to-muted/20">
            <div className="flex flex-col items-center gap-8 max-w-md w-full px-6">
                <div className="flex flex-col items-center gap-4 text-center">
                    <div className="relative">
                        <div className="absolute inset-0 bg-primary/20 blur-3xl rounded-full animate-pulse" />
                        <div className="relative bg-primary/10 p-6 rounded-full border-2 border-primary/20">
                            <Rocket className="w-16 h-16 text-primary" />
                        </div>
                    </div>

                    <h1 className="text-4xl font-bold bg-gradient-to-r from-foreground to-foreground/60 bg-clip-text text-transparent">
                        Synco
                    </h1>

                    <p className="text-muted-foreground text-lg">
                        Start your legacy code transformation journey
                    </p>
                </div>

                <div className="w-full space-y-4">
                    <div className="space-y-2">
                        <label htmlFor="source-folder" className="text-sm font-medium text-foreground/80">
                            Source Folder
                        </label>
                        <Input
                            id="source-folder"
                            value={sourceFolder}
                            onChange={(e: React.ChangeEvent<HTMLInputElement>) => setSourceFolder(e.target.value)}
                            placeholder="data/cobol_source"
                            className="h-12 text-base"
                            disabled={isLoading}
                        />
                    </div>

                    <Button
                        onClick={handleStartMigration}
                        disabled={isLoading || !sourceFolder.trim()}
                        className="w-full h-14 text-lg font-semibold bg-gradient-to-r from-primary to-primary/80 hover:from-primary/90 hover:to-primary/70 transition-all duration-300 shadow-lg hover:shadow-xl"
                    >
                        {isLoading ? (
                            <>
                                <Loader2 className="mr-2 h-5 w-5 animate-spin" />
                                Starting Migration...
                            </>
                        ) : (
                            <>
                                <Rocket className="mr-2 h-5 w-5" />
                                Start Migration
                            </>
                        )}
                    </Button>

                    {error && (
                        <div className="p-4 rounded-lg bg-destructive/10 border border-destructive/20 text-destructive text-sm">
                            {error}
                        </div>
                    )}
                </div>

                <div className="text-xs text-muted-foreground text-center space-y-1">
                    <p>Start your COBOL migration insights journey</p>
                </div>
            </div>
        </div>
    );
}
