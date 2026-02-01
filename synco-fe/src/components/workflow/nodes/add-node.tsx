"use client";

import type { NodeProps } from "@xyflow/react";
import { Plus } from "lucide-react";
import { Button } from "@/components/ui/button";

type AddNodeData = {
    onClick?: () => void;
};

export function AddNode({ data }: NodeProps & { data?: AddNodeData }) {
    return (
        <div className="flex flex-col items-center justify-center gap-4 rounded-lg border border-border border-dashed bg-background/50 p-6 backdrop-blur-sm">
            <div className="text-center">
                <h1 className="mb-2 font-bold text-lg">
                    Start building workflow
                </h1>
            </div>
            <Button className="gap-2 shadow-lg" onClick={data.onClick} size="default">
                <Plus className="size-4" />
                Add a Step
            </Button>
        </div>
    );
}
