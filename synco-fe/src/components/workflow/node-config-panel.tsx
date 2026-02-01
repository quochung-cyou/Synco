"use client";

import { useAtom, useAtomValue, useSetAtom } from "jotai";
import { X, Trash2 } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Textarea } from "@/components/ui/textarea";
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select";
import {
    nodesAtom,
    selectedNodeAtom,
    updateNodeDataAtom,
    deleteNodeAtom,
} from "@/lib/workflow-store";
import { ACTION_DEFINITIONS } from "@/lib/workflow-definitions";
import { ScrollArea } from "@/components/ui/scroll-area";

export function NodeConfigPanel() {
    const [selectedNodeId, setSelectedNodeId] = useAtom(selectedNodeAtom);
    const nodes = useAtomValue(nodesAtom);
    const updateNodeData = useSetAtom(updateNodeDataAtom);
    const deleteNode = useSetAtom(deleteNodeAtom);

    const selectedNode = nodes.find((n) => n.id === selectedNodeId);

    if (!selectedNode) {
        return null;
    }

    const handleClose = () => {
        setSelectedNodeId(null);
    };

    const handleUpdate = (key: string, value: any) => {
        if (key === "actionType") {
            // if actionType changes, we might want to reset config or set defaults
            updateNodeData({
                id: selectedNode.id,
                data: {
                    config: { ...selectedNode.data.config, actionType: value },
                    // optional: set label to action label if not set
                },
            });
            return;
        }

        if (key === "label" || key === "description") {
            updateNodeData({
                id: selectedNode.id,
                data: { [key]: value },
            });
        } else {
            // Assume it is a config update
            updateNodeData({
                id: selectedNode.id,
                data: {
                    config: { ...selectedNode.data.config, [key]: value },
                },
            });
        }
    };

    const handleDelete = () => {
        if (window.confirm("Are you sure you want to delete this step?")) {
            deleteNode(selectedNode.id);
            setSelectedNodeId(null);
        }
    }

    const actionType = selectedNode.data.config?.actionType as string | undefined;

    return (
        <div className="flex h-full w-[350px] flex-col border-l bg-background">
            <div className="flex items-center justify-between border-b px-4 py-3">
                <h2 className="font-semibold">Step Configuration</h2>
                <Button size="icon" variant="ghost" onClick={handleClose}>
                    <X className="size-4" />
                </Button>
            </div>

            <ScrollArea className="flex-1 p-4">
                <div className="space-y-6">
                    <div className="space-y-2">
                        <Label>Step Type</Label>
                        {selectedNode.data.type === "action" ? (
                            <Select
                                value={actionType}
                                onValueChange={(val) => handleUpdate("actionType", val)}
                                disabled={selectedNode.data.type !== "action"}
                            >
                                <SelectTrigger>
                                    <SelectValue placeholder="Select action type" />
                                </SelectTrigger>
                                <SelectContent>
                                    {Object.values(ACTION_DEFINITIONS).map((def) => (
                                        <SelectItem key={def.id} value={def.id}>
                                            <div className="flex items-center gap-2">
                                                <def.icon className="size-4" />
                                                <span>{def.label}</span>
                                            </div>
                                        </SelectItem>
                                    ))}
                                </SelectContent>
                            </Select>
                        ) : (
                            <Input value="Trigger" disabled />
                        )}

                    </div>

                    <div className="space-y-2">
                        <Label>Label</Label>
                        <Input
                            value={selectedNode.data.label || ""}
                            onChange={(e) => handleUpdate("label", e.target.value)}
                            placeholder="Step Name"
                        />
                    </div>

                    <div className="space-y-2">
                        <Label>Description</Label>
                        <Textarea
                            value={selectedNode.data.description || ""}
                            onChange={(e) => handleUpdate("description", e.target.value)}
                            placeholder="Describe this step..."
                            className="resize-none h-20"
                        />
                    </div>

                    {/* Dynamic Config Fields based on Action Type - Simplified for now */}
                    {actionType === "HTTP Request" && (
                        <div className="space-y-4 pt-4 border-t">
                            <h3 className="font-medium text-sm text-muted-foreground">HTTP Configuration</h3>
                            <div className="space-y-2">
                                <Label>Method</Label>
                                <Select
                                    value={(selectedNode.data.config?.method as string) || "GET"}
                                    onValueChange={(val) => handleUpdate("method", val)}
                                >
                                    <SelectTrigger><SelectValue /></SelectTrigger>
                                    <SelectContent>
                                        <SelectItem value="GET">GET</SelectItem>
                                        <SelectItem value="POST">POST</SelectItem>
                                        <SelectItem value="PUT">PUT</SelectItem>
                                        <SelectItem value="DELETE">DELETE</SelectItem>
                                    </SelectContent>
                                </Select>
                            </div>
                            <div className="space-y-2">
                                <Label>URL</Label>
                                <Input
                                    value={(selectedNode.data.config?.url as string) || ""}
                                    onChange={(e) => handleUpdate("url", e.target.value)}
                                    placeholder="https://api.example.com"
                                />
                            </div>
                            <div className="space-y-2">
                                <Label>Headers (JSON)</Label>
                                <Textarea
                                    value={(selectedNode.data.config?.headers as string) || "{}"}
                                    onChange={(e) => handleUpdate("headers", e.target.value)}
                                    className="font-mono text-xs"
                                />
                            </div>
                        </div>
                    )}

                    {actionType === "Database Query" && (
                        <div className="space-y-4 pt-4 border-t">
                            <h3 className="font-medium text-sm text-muted-foreground">Database Configuration</h3>
                            <div className="space-y-2">
                                <Label>Query</Label>
                                <Textarea
                                    value={(selectedNode.data.config?.query as string) || ""}
                                    onChange={(e) => handleUpdate("query", e.target.value)}
                                    placeholder="SELECT * FROM users"
                                    className="font-mono text-xs h-32"
                                />
                            </div>
                        </div>
                    )}

                </div>
            </ScrollArea>

            <div className="p-4 border-t bg-muted/20">
                <Button
                    variant="destructive"
                    className="w-full gap-2"
                    onClick={handleDelete}
                >
                    <Trash2 className="size-4" />
                    Delete Step
                </Button>
            </div>
        </div>
    );
}
