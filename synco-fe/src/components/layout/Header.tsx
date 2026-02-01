import { MessageSquare } from 'lucide-react'

export function Header() {
    return (
        <header className="border-b border-border bg-card/30 backdrop-blur-sm sticky top-0 z-10">
            <div className="mx-auto max-w-7xl px-6 py-6 w-full">
                <div className="space-y-2">
                    <div className="flex items-center gap-3">
                        <MessageSquare className="h-6 w-6 text-primary" />
                        <h1 className="text-2xl font-semibold tracking-tight">
                            Synco - COBOL Migration Insights
                        </h1>
                    </div>
                    <p className="text-sm text-muted-foreground">
                        AI-powered analysis for Mainframe Modernization
                    </p>
                </div>
            </div>
        </header>
    )
}
