"use client";

import { useAtom, useSetAtom } from "jotai";
import {
    Play,
    Redo2,
    Save,
    Trash2,
    Undo2,
    Loader2,
} from "lucide-react";
import { Button } from "@/components/ui/button";
import { ButtonGroup } from "@/components/ui/button-group";
import {
    canRedoAtom,
    canUndoAtom,
    clearWorkflowAtom,
    autosaveAtom,
    undoAtom,
    redoAtom,
    hasUnsavedChangesAtom,
} from "@/lib/workflow-store";

type WorkflowToolbarProps = {
    onRun?: () => void;
    isRunning?: boolean;
};

export function WorkflowToolbar({ onRun, isRunning }: WorkflowToolbarProps) {
    const [canUndo] = useAtom(canUndoAtom);
    const [canRedo] = useAtom(canRedoAtom);
    const undo = useSetAtom(undoAtom);
    const redo = useSetAtom(redoAtom);
    const clearWorkflow = useSetAtom(clearWorkflowAtom);
    const triggerSave = useSetAtom(autosaveAtom);
    const [hasUnsavedChanges] = useAtom(hasUnsavedChangesAtom);

    const handleClear = () => {
        if (window.confirm("Are you sure you want to clear the workflow?")) {
            clearWorkflow();
        }
    };

    const handleSave = () => {
        triggerSave({ immediate: true });
    };

    return (
        <div className="absolute top-4 right-4 z-10 flex items-center gap-2">
            <div className="flex items-center gap-2 rounded-lg border bg-background/80 p-1 backdrop-blur-sm">
                <ButtonGroup>
                    <Button
                        onClick={() => undo()}
                        disabled={!canUndo}
                        size="icon"
                        variant="ghost"
                        title="Undo"
                    >
                        <Undo2 className="size-4" />
                    </Button>
                    <Button
                        onClick={() => redo()}
                        disabled={!canRedo}
                        size="icon"
                        variant="ghost"
                        title="Redo"
                    >
                        <Redo2 className="size-4" />
                    </Button>
                </ButtonGroup>

                <div className="h-4 w-px bg-border mx-1" />

                <Button
                    onClick={handleClear}
                    size="icon"
                    variant="ghost"
                    title="Clear Workflow"
                    className="text-destructive hover:text-destructive"
                >
                    <Trash2 className="size-4" />
                </Button>

                <div className="h-4 w-px bg-border mx-1" />

                <Button
                    onClick={handleSave}
                    size="sm"
                    variant={hasUnsavedChanges ? "default" : "secondary"}
                    title="Save Workflow"
                    className="gap-2"
                >
                    <Save className="size-4" />
                    Save
                </Button>

                {onRun && (
                    <Button
                        onClick={onRun}
                        disabled={isRunning || hasUnsavedChanges}
                        size="sm"
                        variant="outline"
                        title={hasUnsavedChanges ? "Save before running" : "Run Workflow"}
                        className="gap-2 ml-2"
                    >
                        {isRunning ? <Loader2 className="size-4 animate-spin" /> : <Play className="size-4" />}
                        Run
                    </Button>
                )}
            </div>
        </div>
    );
}
