import { Bird, BookOpen, Heart, Shield } from "lucide-react";
import type { ComponentType } from "react";
import { useEffect, useRef, useState } from "react";

interface Feature {
  Icon: ComponentType<{ className?: string; strokeWidth?: number }>;
  title: string;
  description: string;
  details: string[];
}

const features: Feature[] = [
  {
    Icon: Heart,
    title: "Designed With Care",
    description:
      'Every feature is intentional. No historical baggage, no "we\'ve always done it this way." Just thoughtful design choices that make your life easier.',
    details: [
      "Consistent, predictable syntax",
      "Sensible defaults everywhere",
      "Small, orthogonal feature set",
      "Error messages that actually help",
    ],
  },
  {
    Icon: BookOpen,
    title: "Made To Be Read",
    description:
      "Code is read far more than it's written. Labeled parameters, clear type signatures, and expressive syntax mean your future self will thank you.",
    details: [
      "Named arguments at call sites",
      "Self-documenting function calls",
      "No cryptic operators or sigils",
      "Readable without comments",
    ],
  },
  {
    Icon: Shield,
    title: "Sound By Default",
    description:
      "The type system proves your code is correct. No null pointer exceptions, no data races, no undefined behavior. If it compiles, it works.",
    details: [
      "No null — use Optional instead",
      "Exhaustive pattern matching",
      "Immutable by default",
      "Memory safety guaranteed",
    ],
  },
  {
    Icon: Bird,
    title: "Batteries Included",
    description:
      "A rich standard library means less time hunting for packages. Iterators, collections, serialization, and IO — all designed to work together.",
    details: [
      "Powerful iterator combinators",
      "Generic collections",
      "JSON & serde built-in",
      "Result-based error handling",
    ],
  },
];

function FeatureCard({ feature, index }: { feature: Feature; index: number }) {
  const [isVisible, setIsVisible] = useState(false);
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setIsVisible(true);
        }
      },
      { threshold: 0.3 }
    );

    if (ref.current) {
      observer.observe(ref.current);
    }

    return () => observer.disconnect();
  }, []);

  return (
    <div
      ref={ref}
      className={`group transition-all duration-700 ${
        isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
      }`}
      style={{ transitionDelay: `${index * 100}ms` }}>
      <div className="flex items-start gap-4">
        <div className="flex-shrink-0 w-10 h-10 rounded-lg bg-[var(--color-forest)]/10 flex items-center justify-center group-hover:bg-[var(--color-forest)]/20 transition-colors">
          <feature.Icon
            className="w-5 h-5 text-[var(--color-forest)]"
            strokeWidth={1.5}
          />
        </div>
        <div>
          <h3 className="font-serif text-xl font-bold text-[var(--color-slate)] mb-2">
            {feature.title}
          </h3>
          <p className="text-[var(--color-slate-light)] font-serif text-sm mb-3 leading-relaxed">
            {feature.description}
          </p>
          <ul className="space-y-1.5">
            {feature.details.map((detail, i) => (
              <li
                key={i}
                className="flex items-center text-[var(--color-slate)] font-mono text-xs">
                <span className="w-1.5 h-1.5 bg-[var(--color-rust)] rounded-full mr-2" />
                {detail}
              </li>
            ))}
          </ul>
        </div>
      </div>
    </div>
  );
}

export default function Features() {
  const [isVisible, setIsVisible] = useState(false);
  const sectionRef = useRef<HTMLElement>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setIsVisible(true);
        }
      },
      { threshold: 0.1 }
    );

    if (sectionRef.current) {
      observer.observe(sectionRef.current);
    }

    return () => observer.disconnect();
  }, []);

  return (
    <section
      ref={sectionRef}
      className="scroll-section relative bg-[#f0ebe3] overflow-hidden">
      {/* Subtle pattern */}
      <div className="absolute inset-0 opacity-[0.03]">
        <div
          className="absolute inset-0"
          style={{
            backgroundImage: `radial-gradient(circle at 2px 2px, var(--color-slate) 1px, transparent 0)`,
            backgroundSize: "32px 32px",
          }}
        />
      </div>

      <div className="relative z-10 h-full flex flex-col justify-center px-6 md:px-12 lg:px-24 py-20">
        {/* Section header */}
        <div
          className={`max-w-2xl mb-16 transition-all duration-1000 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <span className="font-mono text-[var(--color-forest)] text-sm uppercase tracking-widest">
            Why Kestrel?
          </span>
          <h2 className="font-serif text-5xl md:text-6xl lg:text-7xl font-black text-[var(--color-slate)] mt-4 tracking-tight">
            Built Different.
          </h2>
          <p className="mt-4 text-xl text-[var(--color-rust)] font-mono">
            Clarity. Safety. Joy.
          </p>
        </div>

        {/* Feature grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-x-16 gap-y-12 max-w-4xl">
          {features.map((feature, index) => (
            <FeatureCard key={index} feature={feature} index={index} />
          ))}
        </div>
      </div>
    </section>
  );
}
