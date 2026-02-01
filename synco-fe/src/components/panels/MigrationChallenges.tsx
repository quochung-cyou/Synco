import { AlertCircle, Link2, Database, Clock, Activity } from 'lucide-react'
import { Card } from '@/components/ui/card'
import { useAtomValue } from 'jotai'
import { migrationRunAtom } from '@/lib/migration-store'
import { useMemo } from 'react'

export function MigrationChallenges() {
    const migrationRun = useAtomValue(migrationRunAtom)

    const challengesData = useMemo(() => {
        if (!migrationRun?.agent_results) return {}

        const agent = migrationRun.agent_results.find(
            (r) => r.agent_name === 'dashboard_summary_agent' && r.status === 'COMPLETED'
        )

        return agent?.data?.['Migration Challenges'] || {}
    }, [migrationRun])

    const getIconAndColor = (key: string) => {
        const lowerKey = key.toLowerCase()
        if (lowerKey.includes('cics')) return { icon: Link2, color: 'text-orange-500' }
        if (lowerKey.includes('sql')) return { icon: Database, color: 'text-blue-500' }
        if (lowerKey.includes('file')) return { icon: Clock, color: 'text-cyan-500' } // Using Clock for I/O patterns as per original
        return { icon: Activity, color: 'text-primary' }
    }

    // Default challenges if no data (or just empty)
    // Actually if we have data, we show it. If not, maybe show "No challenges detected" or 0s.
    // The previous mocked version had 3 specific items. Let's try to show what's in the data.
    // If data is empty, we show a default set with 0%?
    // User request example shows these 3 keys. So we can expect them.

    const challengesList = Object.keys(challengesData).length > 0
        ? Object.entries(challengesData)
        : [
            ['CICS Dependencies', '0% of programs'],
            ['SQL Dependencies', '0% of programs'],
            ['File I/O Patterns', '0% of programs']
        ]

    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5">
            <h3 className="text-sm font-semibold mb-4 flex items-center gap-2">
                <AlertCircle className="h-4 w-4 text-primary" />
                Migration Challenges
            </h3>
            <div className="space-y-2">
                {challengesList.map(([key, value]) => {
                    const { icon: Icon, color } = getIconAndColor(key)
                    return (
                        <div key={key} className="flex items-start gap-2">
                            <Icon className={`h-3.5 w-3.5 mt-0.5 flex-shrink-0 ${color}`} />
                            <div className="text-xs space-y-1">
                                <p className="font-medium">{key}</p>
                                <p className="text-muted-foreground">{String(value)}</p>
                            </div>
                        </div>
                    )
                })}
            </div>
        </Card>
    )
}
