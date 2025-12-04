import { Check, Copy, Github } from "lucide-react";
import { useEffect, useRef, useState } from "react";

const steps = [
  {
    number: "01",
    title: "Clone",
    code: "git clone https://github.com/kestrel-lang/kestrel",
  },
  {
    number: "02",
    title: "Build",
    code: "cd kestrel && cargo build --release",
  },
  {
    number: "03",
    title: "Fly",
    code: "kestrel run hello.ks",
  },
];

export default function GetStarted() {
  const [isVisible, setIsVisible] = useState(false);
  const [copiedIndex, setCopiedIndex] = useState<number | null>(null);
  const sectionRef = useRef<HTMLElement>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setIsVisible(true);
        }
      },
      { threshold: 0.2 }
    );

    if (sectionRef.current) {
      observer.observe(sectionRef.current);
    }

    return () => observer.disconnect();
  }, []);

  const copyToClipboard = async (text: string, index: number) => {
    await navigator.clipboard.writeText(text);
    setCopiedIndex(index);
    setTimeout(() => setCopiedIndex(null), 2000);
  };

  return (
    <section
      ref={sectionRef}
      id="get-started"
      className="relative bg-[var(--color-forest)] overflow-hidden flex-grow">
      {/* Subtle pattern */}
      <div className="absolute inset-0 opacity-[0.08]">
        <div
          className="absolute inset-0"
          style={{
            backgroundImage: `radial-gradient(circle at 2px 2px, white 1px, transparent 0)`,
            backgroundSize: "32px 32px",
          }}
        />
      </div>

      {/* Accent glow */}
      <div className="absolute top-1/4 -right-32 w-96 h-96 bg-[var(--color-gold)] opacity-20 blur-3xl rounded-full" />
      <div className="absolute bottom-1/4 -left-32 w-64 h-64 bg-[var(--color-cream)] opacity-10 blur-3xl rounded-full" />

      <div className="relative z-10 h-full flex flex-col justify-center px-6 md:px-12 lg:px-24 py-20">
        {/* Section header - left aligned */}
        <div
          className={`max-w-2xl mb-16 transition-all duration-1000 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <span className="font-mono text-[var(--color-gold)] text-sm uppercase tracking-widest">
            Get Started
          </span>
          <h2 className="font-serif text-5xl md:text-6xl lg:text-7xl font-black text-white mt-4 tracking-tight">
            Take <span className="text-[var(--color-gold)]">Flight.</span>
          </h2>
          <p className="mt-4 text-xl text-white/70 font-serif">
            Three commands. No configuration nightmares.
          </p>
        </div>

        {/* Steps - stacked */}
        <div
          className={`flex flex-col gap-6 max-w-xl transition-all duration-1000 delay-200 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          {steps.map((step, index) => (
            <div
              key={index}
              className="group"
              style={{ transitionDelay: `${index * 100}ms` }}>
              {/* Step number and title */}
              <div className="flex items-center gap-3 mb-3">
                <span className="font-mono text-2xl font-bold text-[var(--color-gold)]">
                  {step.number}
                </span>
                <span className="font-serif text-xl font-bold text-white">
                  {step.title}
                </span>
              </div>

              {/* Code block */}
              <div className="relative bg-[var(--color-slate)] rounded-xl p-4 group/code">
                <pre className="font-mono text-sm text-gray-300">
                  <span className="text-[var(--color-gold)]">$ </span>
                  {step.code}
                </pre>
                <button
                  onClick={() => copyToClipboard(step.code, index)}
                  className="absolute top-1/2 -translate-y-1/2 right-3 p-2 bg-white/10 rounded-lg font-mono text-xs text-gray-400 hover:bg-white/20 hover:text-white transition-all opacity-0 group-hover/code:opacity-100">
                  {copiedIndex === index ? (
                    <Check className="w-4 h-4 text-[var(--color-gold)]" />
                  ) : (
                    <Copy className="w-4 h-4" />
                  )}
                </button>
              </div>
            </div>
          ))}
        </div>

        {/* CTA */}
        <div
          className={`mt-16 transition-all duration-1000 delay-400 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <a
            href="https://github.com/kestrel-lang/kestrel"
            className="inline-flex items-center gap-3 px-8 py-4 bg-[var(--color-gold)] text-[var(--color-slate)] font-serif text-lg font-bold rounded-xl hover:bg-[var(--color-cream)] transition-all shadow-lg hover:shadow-xl hover:-translate-y-1">
            <Github className="w-5 h-5" />
            View on GitHub
          </a>
          <p className="mt-4 text-white/60 font-mono text-sm">
            Open source. Contributions welcome.
          </p>
        </div>
      </div>
    </section>
  );
}
