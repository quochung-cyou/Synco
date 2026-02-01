
import { Zap, Database, Code, GitBranch, Mail, MessageSquare } from "lucide-react";
import type { LucideIcon } from "lucide-react";

export type ActionDefinition = {
    id: string;
    label: string;
    description: string;
    icon: LucideIcon;
    category: "system" | "integration" | "ai";
};

export const ACTION_DEFINITIONS: Record<string, ActionDefinition> = {
    "HTTP Request": {
        id: "HTTP Request",
        label: "HTTP Request",
        description: "Make an HTTP request",
        icon: Zap,
        category: "system",
    },
    "Database Query": {
        id: "Database Query",
        label: "Database Query",
        description: "Execute a database query",
        icon: Database,
        category: "system",
    },
    "Execute Code": {
        id: "Execute Code",
        label: "Execute Code",
        description: "Run custom JavaScript/Python",
        icon: Code,
        category: "system",
    },
    "Condition": {
        id: "Condition",
        label: "Condition",
        description: "Branch workflow based on logic",
        icon: GitBranch,
        category: "system",
    },
    "Send Email": {
        id: "Send Email",
        label: "Send Email",
        description: "Send an email via SMTP/Resend",
        icon: Mail,
        category: "integration",
    },
    "Send Slack Message": {
        id: "Send Slack Message",
        label: "Send Slack Message",
        description: "Post a message to Slack",
        icon: MessageSquare,
        category: "integration",
    },
};

export function findActionById(id: string): ActionDefinition | undefined {
    return ACTION_DEFINITIONS[id];
}
