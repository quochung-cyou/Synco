import { CheckCircle2 } from 'lucide-react'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'

export function NodeDetails() {
    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5">
            <h3 className="text-sm font-semibold mb-4 flex items-center gap-2">
                <CheckCircle2 className="h-4 w-4 text-primary" />
                Node Details
            </h3>
            <div className="space-y-3 text-xs">
                <div>
                    <p className="text-muted-foreground mb-1">Program</p>
                    <p className="font-medium font-mono text-foreground">
                        BDSMFJL.cbl
                    </p>
                </div>
                <div>
                    <p className="text-muted-foreground mb-1">Type</p>
                    <Badge variant="secondary" className="text-xs">
                        Main Batch
                    </Badge>
                </div>
                <div>
                    <p className="text-muted-foreground mb-1">Dependencies</p>
                    <p className="font-medium">8 Primary • 23 Secondary</p>
                </div>
                <div>
                    <p className="text-muted-foreground mb-1">Complexity</p>
                    <div className="flex items-center gap-2">
                        <div className="flex-1 h-2 bg-background rounded-full overflow-hidden">
                            <div className="h-full w-1/2 bg-gradient-to-r from-amber-500 to-orange-500 rounded-full" />
                        </div>
                        <span className="font-medium">Moderate</span>
                    </div>
                </div>
            </div>
        </Card>
    )
}
