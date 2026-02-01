export type MigrationStatus = "Pending" | "Running" | "Completed" | "Failed";
export type AgentStatus = "PENDING" | "RUNNING" | "COMPLETED" | "FAILED" | "SKIPPED";

export interface AgentDependencyDetail {
    agent_name: string;
    description: string | null;
    required: boolean;
}

export interface AgentMetadata {
    type: string;
    dependencies: string[];
    auto_run: boolean;
    dependency_details: AgentDependencyDetail[];
}

export interface AgentResult {
    agent_name: string;
    status: AgentStatus;
    file_path: string | null;
    data: any;
    error: string | null;
    execution_time: number;
    progress: number;
    current_action?: string;
    created_at: string;
}

export interface MigrationRun {
    id: number;
    started_at: string;
    completed_at: string | null;
    status: MigrationStatus;
    cobol_source_path: string;
    notes: string;
    agent_results: AgentResult[];
    agent_metadata: Record<string, AgentMetadata>;
    execution_order: string[];
}

export interface StartMigrationResponse {
    run_id: number;
    status: MigrationStatus;
}
