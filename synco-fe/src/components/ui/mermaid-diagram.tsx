
import { useEffect, useRef } from 'react';
import mermaid from 'mermaid';

mermaid.initialize({
    startOnLoad: true,
    theme: 'dark',
    securityLevel: 'loose',
});

export const MermaidDiagram = ({ chart }: { chart: string }) => {
    const ref = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (ref.current && chart) {
            // Clear previous content
            ref.current.innerHTML = '';
            const id = `mermaid-${Math.random().toString(36).substr(2, 9)}`;
            try {
                mermaid.render(id, chart).then((result) => {
                    if (ref.current) {
                        ref.current.innerHTML = result.svg;
                    }
                });
            } catch (error) {
                console.error("Failed to render mermaid diagram", error);
                ref.current.innerHTML = "Failed to render diagram";
            }

        }
    }, [chart]);

    return <div ref={ref} className="mermaid flex justify-center w-full overflow-x-auto" />;
};
