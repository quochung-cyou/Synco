import { BookOpen, FileText } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card } from '@/components/ui/card'

export function ResourcesCard() {
    return (
        <Card className="border-border/50 bg-card/40 backdrop-blur p-5">
            <h3 className="text-sm font-semibold mb-4 flex items-center gap-2">
                <BookOpen className="h-4 w-4 text-primary" />
                Resources
            </h3>
            <Button
                size="sm"
                variant="outline"
                className="w-full justify-start gap-2 border-border/50 bg-transparent hover:bg-muted/40 text-xs"
            >
                <FileText className="h-4 w-4" />
                View All Runs & Guides
            </Button>
        </Card>
    )
}
