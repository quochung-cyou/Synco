import type { MigrationRun, StartMigrationResponse } from "./types/migration";

const API_BASE_URL = "http://127.0.0.1:8000";

export class ApiError extends Error {
    constructor(
        message: string,
        public status?: number,
        public data?: any
    ) {
        super(message);
        this.name = "ApiError";
    }
}

async function handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new ApiError(
            errorData.detail || `API error: ${response.statusText}`,
            response.status,
            errorData
        );
    }
    return response.json();
}

export async function startMigration(
    sourceFolder: string = "data/cobol_source"
): Promise<StartMigrationResponse> {
    const url = `${API_BASE_URL}/api/v1/migration/runs?source_folder=${encodeURIComponent(sourceFolder)}`;

    const response = await fetch(url, {
        method: "POST",
        headers: {
            "accept": "application/json",
        },
    });

    return handleResponse<StartMigrationResponse>(response);
}

export async function getMigrationStatus(runId: number): Promise<MigrationRun> {
    const url = `${API_BASE_URL}/api/v1/migration/runs/${runId}`;

    const response = await fetch(url, {
        method: "GET",
        headers: {
            "accept": "application/json",
        },
    });

    return handleResponse<MigrationRun>(response);
}

export interface RerunResponse {
    run_id: number;
    status: string;
    agents_to_rerun: string[];
    dependent_agents: string[];
}

export async function sendChatMessage(
    runId: number,
    message: string
): Promise<RerunResponse> {
    const url = `${API_BASE_URL}/api/v1/migration/runs/${runId}/rerun`;

    const response = await fetch(url, {
        method: "POST",
        headers: {
            "accept": "application/json",
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            agents_to_rerun: ["context_extractor_agent", "chatbot_agent"],
            additional_context: { user_message: message }
        }),
    });

    return handleResponse<RerunResponse>(response);
}
export async function rerunAgents(
    runId: number,
    agentsToRerun: string[],
    additionalContext: Record<string, any> = {}
): Promise<RerunResponse> {
    const url = `${API_BASE_URL}/api/v1/migration/runs/${runId}/rerun`;

    const response = await fetch(url, {
        method: "POST",
        headers: {
            "accept": "application/json",
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            agents_to_rerun: agentsToRerun,
            additional_context: additionalContext
        }),
    });

    return handleResponse<RerunResponse>(response);
}
