import { TrendingUp } from 'lucide-react'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { useAtomValue } from 'jotai'
import { migrationRunAtom } from '@/lib/migration-store'
import { useMemo } from 'react'

export function AnalysisSummary() {
    const migrationRun = useAtomValue(migrationRunAtom)

    const summaryData = useMemo(() => {
        if (!migrationRun?.agent_results) return null

        const agent = migrationRun.agent_results.find(
            (r) => r.agent_name === 'dashboard_summary_agent' && r.status === 'COMPLETED'
        )

        return agent?.data?.['Analysis Summary'] || null
    }, [migrationRun])

    const lastUpdated = useMemo(() => {
        if (!migrationRun?.completed_at && !migrationRun?.started_at) return null
        const date = new Date(migrationRun.completed_at || migrationRun.started_at)
        return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
    }, [migrationRun])

    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5">
            <h3 className="text-sm font-semibold mb-4 flex items-center gap-2">
                <TrendingUp className="h-4 w-4 text-primary" />
                Analysis Summary
            </h3>
            <div className="space-y-3">
                <div className="space-y-1">
                    <p className="text-xs text-muted-foreground">Total Programs</p>
                    <p className="text-lg font-semibold">{summaryData?.['Total Programs'] || 0}</p>
                </div>
                <div className="space-y-1">
                    <p className="text-xs text-muted-foreground">
                        Total Copybooks
                    </p>
                    <p className="text-lg font-semibold">{summaryData?.['Total Copybooks'] || 0}</p>
                </div>
                <div className="space-y-1">
                    <p className="text-xs text-muted-foreground">
                        Avg Complexity
                    </p>
                    <p className="text-lg font-semibold">{summaryData?.['Avg Complexity'] || 0}</p>
                </div>
                {lastUpdated && (
                    <div className="pt-2 border-t border-border/30">
                        <Badge variant="outline" className="text-xs bg-background/50">
                            Last updated {lastUpdated}
                        </Badge>
                    </div>
                )}
            </div>
        </Card>
    )
}
