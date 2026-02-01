import { useState, useEffect } from 'react'
import { Header } from '@/components/layout/Header'
import { ChatArea } from '@/components/chat/ChatArea'
import { AnalysisSummary } from '@/components/panels/AnalysisSummary'
import { FileAnalysisPanel } from '@/components/panels/FileAnalysisPanel'
import { MigrationChallenges } from '@/components/panels/MigrationChallenges'
import { NodeDetails } from '@/components/panels/NodeDetails'
import { ResourcesCard } from '@/components/panels/ResourcesCard'
import { DependencyVisualizer } from '@/components/panels/DependencyVisualizer'
import { MermaidDiagramPanel } from '@/components/panels/MermaidDiagramPanel'
import { CodeComparisonPanel } from '@/components/panels/CodeComparisonPanel'
import { OutputComparisonPanel } from '@/components/panels/OutputComparisonPanel'
import { MigrationStartScreen } from '@/components/migration/MigrationStartScreen'
import { AgentWorkflowVisualization } from '@/components/migration/AgentWorkflowVisualization'
import { useAtomValue, useSetAtom } from 'jotai'
import { migrationRunAtom, updateMigrationStatusAtom } from '@/lib/migration-store'
import { Toaster } from "@/components/ui/sonner"

export default function App() {
  const migrationRun = useAtomValue(migrationRunAtom);
  const updateMigrationStatus = useSetAtom(updateMigrationStatusAtom);
  const [hasStarted, setHasStarted] = useState(false);

  useEffect(() => {
    const handleUrlCheck = () => {
      let runId: number | null = null;

      // Check path for /runs/:id
      const pathRegex = /^\/runs\/(\d+)/;
      const match = window.location.pathname.match(pathRegex);

      if (match) {
        runId = parseInt(match[1], 10);
      } else {
        // Fallback check for query param
        const params = new URLSearchParams(window.location.search);
        const runIdParam = params.get('run_id');
        if (runIdParam) {
          runId = parseInt(runIdParam, 10);
        }
      }

      if (runId && !isNaN(runId)) {
        updateMigrationStatus(runId).then(() => {
          setHasStarted(true);
        }).catch(console.error);
      } else {
        setHasStarted(false);
      }
    };

    // Check on mount
    handleUrlCheck();

    // Listen for history changes (popstate)
    window.addEventListener('popstate', handleUrlCheck);
    return () => window.removeEventListener('popstate', handleUrlCheck);
  }, [updateMigrationStatus]);

  const handleMigrationStarted = (runId: number) => {
    setHasStarted(true);
    window.history.pushState({}, '', `/runs/${runId}`);
  };

  return (
    <div className="min-h-screen bg-background text-foreground flex flex-col">
      <Header />
      <main className="flex-1 w-full h-[calc(100vh-64px)] overflow-hidden bg-muted/10">
        {!hasStarted || !migrationRun ? (
          <div className="h-full w-full flex items-center justify-center p-6">
            <div className="w-full max-w-5xl aspect-video border rounded-xl overflow-hidden shadow-sm bg-background">
              <MigrationStartScreen onMigrationStarted={handleMigrationStarted} />
            </div>
          </div>
        ) : (
          <div className="grid grid-cols-12 gap-4 h-full p-4">
            {/* Left Column: Workflow Visualization - Takes 6/12 columns */}
            {/* Left Column: Workflow Visualization - Takes 6/12 columns */}
            <div className="col-span-12 md:col-span-6 flex flex-col gap-4 h-full overflow-y-auto">
              <div className="h-[600px] w-full border rounded-xl overflow-hidden shadow-sm bg-background shrink-0">
                <AgentWorkflowVisualization />
              </div>
              <CodeComparisonPanel />
              <OutputComparisonPanel />
            </div>

            {/* Middle Column: Analysis & Files - Takes 3/12 columns */}
            <div className="col-span-12 md:col-span-3 h-full min-h-0 flex flex-col gap-4 overflow-y-auto pr-1">
              <ResourcesCard />
              <FileAnalysisPanel />
              <MermaidDiagramPanel />
              <DependencyVisualizer />
              <AnalysisSummary />
              <MigrationChallenges />
            </div>

            {/* Right Column: Chat - Takes 3/12 columns */}
            <div className="col-span-12 md:col-span-3 flex flex-col">
              <div className="h-[1500px] w-full shrink-0">
                <ChatArea />
              </div>
            </div>
          </div>
        )}
      </main>
      <Toaster />
    </div>
  )
}
