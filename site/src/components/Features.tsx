import { Bird, Feather, Heart, Shield } from "lucide-react";
import type { ComponentType } from "react";
import { useEffect, useRef, useState } from "react";

interface Feature {
  Icon: ComponentType<{ className?: string; strokeWidth?: number }>;
  title: string;
  description: string;
}

const features: Feature[] = [
  {
    Icon: Feather,
    title: "Respects Your Cognitive Load",
    description:
      "You spend more time reading code than writing it. Kestrel clears the clutter—no cryptic symbols or boilerplate—so you can focus purely on the logic you're trying to express.",
  },
  {
    Icon: Shield,
    title: "The Compiler is Your Guardian",
    description:
      "Fear of crashing shouldn't hold you back. Kestrel's type system silently handles the safety checks, giving you the confidence that if it builds, it works. No runtime surprises.",
  },
  {
    Icon: Bird,
    title: "Travel Light",
    description:
      "High performance usually requires complex memory management. Kestrel simplifies ownership, giving you the raw speed of the metal without the mental burden of manual allocation.",
  },
  {
    Icon: Heart,
    title: "Joy in the Details",
    description:
      "From helpful error messages that feel like a conversation, to a standard library that feels complete—Kestrel is designed to keep you in your creative flow.",
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
      <div className="flex items-start gap-5">
        <div className="flex-shrink-0 w-12 h-12 rounded-xl bg-[var(--color-forest)]/10 flex items-center justify-center group-hover:bg-[var(--color-forest)]/20 transition-colors">
          <feature.Icon
            className="w-6 h-6 text-[var(--color-forest)]"
            strokeWidth={1.5}
          />
        </div>
        <div>
          <h3 className="font-serif text-2xl font-bold text-[var(--color-slate)] mb-3">
            {feature.title}
          </h3>
          <p className="text-[var(--color-slate-light)] font-serif text-base leading-relaxed">
            {feature.description}
          </p>
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

      <div className="relative z-10 h-full flex flex-col justify-center px-6 md:px-12 lg:px-24 py-24">
        {/* Section header */}
        <div
          className={`max-w-2xl mb-20 transition-all duration-1000 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <span className="font-mono text-[var(--color-forest)] text-sm uppercase tracking-widest">
            Why Kestrel?
          </span>
          <h2 className="font-serif text-5xl md:text-6xl lg:text-7xl font-black text-[var(--color-slate)] mt-4 tracking-tight">
            Built Different.
          </h2>
          <p className="mt-6 text-xl text-[var(--color-rust)] font-mono">
            So you can build something beautiful.
          </p>
        </div>

        {/* Feature grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-x-20 gap-y-16 max-w-5xl">
          {features.map((feature, index) => (
            <FeatureCard key={index} feature={feature} index={index} />
          ))}
        </div>
      </div>
    </section>
  );
}
