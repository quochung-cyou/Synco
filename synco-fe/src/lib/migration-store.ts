import { atom } from "jotai";
import type { MigrationRun } from "./types/migration";
import { startMigration as apiStartMigration, getMigrationStatus } from "./api";

export const migrationRunAtom = atom<MigrationRun | null>(null);
export const isLoadingAtom = atom<boolean>(false);
export const errorAtom = atom<string | null>(null);
export const pollingIntervalIdAtom = atom<number | null>(null);

export const startMigrationAtom = atom(
    null,
    async (get, set, sourceFolder: string = "data/cobol_source") => {
        set(isLoadingAtom, true);
        set(errorAtom, null);

        try {
            const response = await apiStartMigration(sourceFolder);
            const migrationRun = await getMigrationStatus(response.run_id);
            set(migrationRunAtom, migrationRun);
            return migrationRun;
        } catch (error) {
            const errorMessage = error instanceof Error ? error.message : "Failed to start migration";
            set(errorAtom, errorMessage);
            throw error;
        } finally {
            set(isLoadingAtom, false);
        }
    }
);

export const updateMigrationStatusAtom = atom(
    null,
    async (get, set, runId: number) => {
        try {
            const migrationRun = await getMigrationStatus(runId);
            set(migrationRunAtom, migrationRun);
            return migrationRun;
        } catch (error) {
            const errorMessage = error instanceof Error ? error.message : "Failed to fetch migration status";
            set(errorAtom, errorMessage);
            throw error;
        }
    }
);

export const startPollingAtom = atom(
    null,
    (get, set, runId: number) => {
        const existingInterval = get(pollingIntervalIdAtom);
        if (existingInterval) {
            clearInterval(existingInterval);
        }

        const intervalId = window.setInterval(async () => {
            const currentRun = get(migrationRunAtom);
            if (currentRun && (currentRun.status === "Completed" || currentRun.status === "Failed")) {
                const id = get(pollingIntervalIdAtom);
                if (id) {
                    clearInterval(id);
                    set(pollingIntervalIdAtom, null);
                }
                return;
            }

            try {
                await set(updateMigrationStatusAtom, runId);
            } catch (error) {
                console.error("Polling error:", error);
            }
        }, 500);

        set(pollingIntervalIdAtom, intervalId);
    }
);

export const stopPollingAtom = atom(
    null,
    (get, set) => {
        const intervalId = get(pollingIntervalIdAtom);
        if (intervalId) {
            clearInterval(intervalId);
            set(pollingIntervalIdAtom, null);
        }
    }
);
