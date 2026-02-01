'use client'

import { useState, useEffect, useRef, useCallback } from 'react'
import { MessageSquare, RefreshCw, Send, Sparkles } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card } from '@/components/ui/card'
import { Textarea } from '@/components/ui/textarea'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { useAtomValue, useSetAtom } from 'jotai'
import { migrationRunAtom } from '@/lib/migration-store'
import { sendChatMessage, getMigrationStatus } from '@/lib/api'

export function ChatArea() {
    const migrationRun = useAtomValue(migrationRunAtom)
    const setMigrationRun = useSetAtom(migrationRunAtom)
    const [prompt, setPrompt] = useState('')
    const [isLoading, setIsLoading] = useState(false)
    const processedAgentsRef = useRef<Set<string>>(new Set())

    const [messages, setMessages] = useState<any[]>([])
    const historyLoadedRef = useRef<number | null>(null)

    useEffect(() => {
        if (!migrationRun?.id || historyLoadedRef.current === migrationRun.id) return

        const historyAgent = migrationRun.agent_results.find(
            (a: any) => a.agent_name === 'chat_history_agent'
        )

        if (historyAgent?.status === 'COMPLETED' && historyAgent?.data?.history) {
            const history = historyAgent.data.history
            const loadedMessages: any[] = []

            history.forEach((item: any, index: number) => {
                if (item.user_message) {
                    loadedMessages.push({
                        id: `history-user-${index}`,
                        type: 'user',
                        content: item.user_message,
                        priority: 2,
                        timestamp: new Date(item.timestamp).toLocaleTimeString('en-US', {
                            hour: 'numeric',
                            minute: '2-digit',
                        }),
                    })
                }
                if (item.assistant_response) {
                    loadedMessages.push({
                        id: `history-assistant-${index}`,
                        type: 'assistant',
                        content: item.assistant_response,
                        priority: 2,
                        timestamp: new Date(item.timestamp).toLocaleTimeString('en-US', {
                            hour: 'numeric',
                            minute: '2-digit',
                        }),
                    })
                }
            })

            if (loadedMessages.length > 0) {
                setMessages(prev => [...prev, ...loadedMessages].sort((a, b) => a.priority - b.priority))
                historyLoadedRef.current = migrationRun.id
            }
        }
    }, [migrationRun?.id, migrationRun?.agent_results])

    useEffect(() => {
        if (!migrationRun || !migrationRun.agent_results) return

        const runId = migrationRun.id

        migrationRun.agent_results.forEach(agent => {
            const key = `${runId}-${agent.agent_name}`
            if (agent.status === 'COMPLETED' && !processedAgentsRef.current.has(key)) {
                let messageContent = null
                let details = null

                if (agent.agent_name === 'structure_insights_agent' && agent.data?.insights) {
                    messageContent = agent.data.insights
                } else if (agent.agent_name === 'dashboard_summary_agent' && agent.data) {
                    const analysis = agent.data['Analysis Summary'] || {}
                    const challenges = agent.data['Migration Challenges'] || {}

                    messageContent = `### Analysis Complete
**Summary:**
* Total Programs: ${analysis['Total Programs'] || 0}
* Avg Complexity: ${analysis['Avg Complexity'] || 0}
* Total Copybooks: ${analysis['Total Copybooks'] || 0}

**Migration Challenges:**
${Object.entries(challenges).map(([k, v]) => `* ${k}: ${v}`).join('\n')}
`
                    details = {
                        totalPrograms: analysis['Total Programs'],
                        avgComplexity: analysis['Avg Complexity'],
                        ...Object.fromEntries(Object.entries(challenges).map(([k, v]) => [k.replace(/\s+/g, ''), v]))
                    }
                } else if (agent.agent_name === 'python_conversion_strategy_agent' && agent.data?.conversion_plan) {
                    const plan = agent.data.conversion_plan as any[];
                    const strategySummary = plan.map((item: any) => {
                        return `#### ${item.file_path} (Complexity: ${item.complexity})\n${item.strategy}`
                    }).join('\n\n')

                    messageContent = `### Conversion Strategy Generated
I have generated a conversion strategy for ${plan.length} files.

${strategySummary}
`
                }

                if (messageContent) {
                    setMessages((prev) => [
                        ...prev,
                        {
                            id: prev.length + 1,
                            type: 'assistant',
                            content: messageContent,
                            priority: 1,
                            timestamp: new Date().toLocaleTimeString('en-US', {
                                hour: 'numeric',
                                minute: '2-digit',
                            }),
                            details: details
                        },
                    ].sort((a, b) => a.priority - b.priority))
                    processedAgentsRef.current.add(key)
                }
            }
        })

    }, [migrationRun])

    const isChatEnabled = migrationRun?.status === 'Completed' || migrationRun?.status === 'Failed'

    const suggestionTags = [
        { label: 'Circular Dependencies', color: 'hover:bg-slate-700/40' },
        { label: 'Critical Files', color: 'hover:bg-slate-700/40' },
        { label: 'Impact Analysis', color: 'hover:bg-slate-700/40' },
        { label: 'Copybook Usage', color: 'hover:bg-slate-700/40' },
        { label: 'Dependency Summary', color: 'hover:bg-slate-700/40' },
        { label: 'Main Programs', color: 'hover:bg-slate-700/40' },
    ]

    const handleSendMessage = useCallback(async () => {
        if (!prompt.trim() || !migrationRun?.id || isLoading) return

        const userMessage = prompt.trim()
        setMessages((prev) => [
            ...prev,
            {
                id: prev.length + 1,
                type: 'user',
                content: userMessage,
                timestamp: new Date().toLocaleTimeString('en-US', {
                    hour: 'numeric',
                    minute: '2-digit',
                }),
            },
        ])
        setPrompt('')
        setIsLoading(true)

        try {
            await sendChatMessage(migrationRun.id, userMessage)

            let attempts = 0
            const maxAttempts = 60
            const pollInterval = 1000

            const pollForResponse = async () => {
                while (attempts < maxAttempts) {
                    attempts++
                    await new Promise(resolve => setTimeout(resolve, pollInterval))

                    const status = await getMigrationStatus(migrationRun.id)
                    setMigrationRun(status)

                    const chatbotResult = status.agent_results.find(
                        (a: any) => a.agent_name === 'chatbot_agent'
                    )

                    if (chatbotResult?.status === 'COMPLETED' && chatbotResult?.data?.assistant_response) {
                        setMessages((prev) => [
                            ...prev,
                            {
                                id: prev.length + 1,
                                type: 'assistant',
                                content: chatbotResult.data.assistant_response,
                                timestamp: new Date().toLocaleTimeString('en-US', {
                                    hour: 'numeric',
                                    minute: '2-digit',
                                }),
                            },
                        ])
                        return
                    }

                    if (chatbotResult?.status === 'FAILED') {
                        throw new Error(chatbotResult.error || 'Chat failed')
                    }
                }
                throw new Error('Timeout waiting for response')
            }

            await pollForResponse()
        } catch (error: any) {
            setMessages((prev) => [
                ...prev,
                {
                    id: prev.length + 1,
                    type: 'assistant',
                    content: `Error: ${error.message || 'Failed to get response'}`,
                    timestamp: new Date().toLocaleTimeString('en-US', {
                        hour: 'numeric',
                        minute: '2-digit',
                    }),
                },
            ])
        } finally {
            setIsLoading(false)
        }
    }, [prompt, migrationRun?.id, isLoading, setMigrationRun])

    return (
        <div className="h-full flex flex-col">
            <Card className="border-border/50 bg-card/40 backdrop-blur overflow-hidden flex flex-col h-full">
                <div className="border-b border-border/50 bg-background/60 px-6 py-4">
                    <div className="flex items-center gap-3">
                        <div className="w-10 h-10 rounded-lg bg-gradient-to-br from-primary/20 to-primary/10 flex items-center justify-center">
                            <Sparkles className="h-5 w-5 text-primary" />
                        </div>
                        <div>
                            <h3 className="text-sm font-semibold text-foreground">AI Migration Assistant</h3>
                            <p className="text-xs text-muted-foreground">Ask about your COBOL migration analysis</p>
                        </div>
                    </div>
                </div>
                <div className="flex-1 overflow-y-auto p-6 space-y-4">
                    {messages.length === 0 ? (
                        <div className="h-full flex flex-col items-center justify-center text-center space-y-4">
                            <div className="w-16 h-16 rounded-full bg-primary/10 flex items-center justify-center">
                                <MessageSquare className="h-8 w-8 text-primary/60" />
                            </div>
                            <div className="space-y-1">
                                <p className="font-semibold text-foreground">
                                    Start your migration analysis
                                </p>
                                <p className="text-sm text-muted-foreground">
                                    Ask questions about your COBOL codebase and migration
                                    path
                                </p>
                            </div>
                        </div>
                    ) : (
                        messages.map((message) => (
                            <div
                                key={message.id}
                                className={`flex ${message.type === 'user'
                                    ? 'justify-end'
                                    : 'justify-start'
                                    }`}
                            >
                                <div
                                    className={`max-w-lg space-y-2 ${message.type === 'user' ? 'items-end' : 'items-start'
                                        }`}
                                >
                                    <div
                                        className={`rounded-lg px-4 py-3 ${message.type === 'user'
                                            ? 'bg-primary text-primary-foreground rounded-br-none'
                                            : 'bg-muted/40 text-foreground border border-border/50 rounded-bl-none'
                                            }`}
                                    >
                                        <div className="text-sm leading-relaxed prose prose-sm dark:prose-invert max-w-none break-words">
                                            <ReactMarkdown remarkPlugins={[remarkGfm]}>
                                                {String(message.content)}
                                            </ReactMarkdown>
                                        </div>
                                    </div>
                                    <span className="text-xs text-muted-foreground px-2">
                                        {message.timestamp}
                                    </span>
                                    {message.details && (
                                        <div
                                            className={`grid grid-cols-2 gap-2 text-xs ${message.type === 'user' ? 'hidden' : ''
                                                }`}
                                        >
                                            {Object.entries(message.details).map(
                                                ([key, value]) => (
                                                    <div
                                                        key={key}
                                                        className="bg-background/50 border border-border/30 rounded px-2 py-1.5"
                                                    >
                                                        <p className="text-muted-foreground capitalize">
                                                            {key.replace(/([A-Z])/g, ' $1').toLowerCase()}
                                                        </p>
                                                        <p className="font-semibold text-foreground">
                                                            {String(value)}
                                                        </p>
                                                    </div>
                                                )
                                            )}
                                        </div>
                                    )}
                                </div>
                            </div>
                        ))
                    )}
                    {isLoading && (
                        <div className="flex justify-start">
                            <div className="bg-muted/40 border border-border/50 rounded-lg rounded-bl-none px-4 py-3">
                                <div className="flex items-center gap-2">
                                    <div className="w-2 h-2 bg-primary rounded-full animate-bounce" />
                                    <div className="w-2 h-2 bg-primary rounded-full animate-bounce delay-100" />
                                    <div className="w-2 h-2 bg-primary rounded-full animate-bounce delay-200" />
                                </div>
                            </div>
                        </div>
                    )}
                </div>

                <div className="border-t border-border/50 bg-background/40 p-6 space-y-4">
                    <div className="flex flex-wrap gap-2">
                        {suggestionTags.map((tag) => (
                            <button
                                key={tag.label}
                                onClick={() => setPrompt(tag.label)}
                                disabled={!isChatEnabled}
                                className={`inline-flex items-center rounded-full border border-border/40 bg-muted/20 px-3 py-1.5 text-xs font-medium text-muted-foreground transition-all hover:border-border/70 ${tag.color} disabled:opacity-50 disabled:cursor-not-allowed`}
                            >
                                {tag.label}
                            </button>
                        ))}
                    </div>

                    <div className="space-y-3">
                        <Textarea
                            placeholder={isChatEnabled ? "Ask about copybooks, dependencies, complexity analysis, migration strategy..." : "Migration in progress..."}
                            value={prompt}
                            disabled={!isChatEnabled}
                            onChange={(e) => setPrompt(e.target.value)}
                            onKeyDown={(e) => {
                                if (e.key === 'Enter' && e.ctrlKey && isChatEnabled) {
                                    handleSendMessage()
                                }
                            }}
                            className="min-h-20 resize-none border-border/50 bg-background placeholder-muted-foreground/40 rounded-lg disabled:opacity-50"
                        />
                        <div className="flex justify-between items-center">
                            <p className="text-xs text-muted-foreground">
                                Press Ctrl+Enter to send
                            </p>
                            <Button
                                size="sm"
                                disabled={isLoading || !prompt.trim() || !isChatEnabled}
                                className="gap-2 bg-primary hover:bg-primary/90"
                                onClick={handleSendMessage}
                            >
                                {isLoading ? (
                                    <>
                                        <RefreshCw className="h-4 w-4 animate-spin" />
                                        Analyzing
                                    </>
                                ) : (
                                    <>
                                        <Send className="h-4 w-4" />
                                        Send
                                    </>
                                )}
                            </Button>
                        </div>
                    </div>
                </div>
            </Card>
        </div>
    )
}
