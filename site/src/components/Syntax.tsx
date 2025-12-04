import React, { useEffect, useRef, useState } from "react";

interface SyntaxFeature {
  category: string;
  items: {
    name: string;
    code: string;
    description: string;
  }[];
}

const syntaxFeatures: SyntaxFeature[] = [
  {
    category: "Types & Data",
    items: [
      {
        name: "Structs",
        code: `struct Point {
    let x: Float
    let y: Float
}`,
        description:
          "Lightweight data structures with immutable fields by default",
      },
      {
        name: "Generics",
        code: `struct Box[T] {
    var contents: T
}`,
        description:
          "Type parameters make your code reusable without sacrificing safety",
      },
      {
        name: "Type Aliases",
        code: `type Coordinates = (Float, Float)
type StringList = Array[String]`,
        description: "Give complex types meaningful names",
      },
    ],
  },
  {
    category: "Functions & Methods",
    items: [
      {
        name: "Functions",
        code: `func greet(name: String) -> String {
    "Hello, " + name + "!"
}`,
        description: "First-class functions with clear return types",
      },
      {
        name: "Labeled Args",
        code: `func move(from start: Point, 
         to end: Point) -> Path`,
        description: "Readable call sites that document themselves",
      },
      {
        name: "Methods",
        code: `struct Counter {
    var count: Int
    
    mutating func increment() {
        self.count = self.count + 1
    }
}`,
        description: "Mutation is explicit and intentional",
      },
    ],
  },
  {
    category: "Protocols & Abstraction",
    items: [
      {
        name: "Protocols",
        code: `protocol Hashable {
    func hash() -> Int
}`,
        description: "Define contracts that types must fulfill",
      },
      {
        name: "Conformance",
        code: `struct User: Hashable {
    let id: Int
    
    func hash() -> Int { self.id }
}`,
        description: "Types declare and implement their capabilities",
      },
      {
        name: "Constraints",
        code: `func findDuplicates[T](items: [T]) -> [T]
    where T: Hashable`,
        description: "Generic bounds ensure type safety",
      },
    ],
  },
];

function tokenize(code: string): React.ReactNode[] {
  const keywords = [
    "struct",
    "protocol",
    "func",
    "let",
    "var",
    "type",
    "mutating",
    "where",
    "self",
  ];
  const types = [
    "Int",
    "Float",
    "String",
    "Bool",
    "Array",
    "Point",
    "Box",
    "Counter",
    "User",
    "Hashable",
    "Path",
    "T",
  ];

  const tokens: React.ReactNode[] = [];
  let current = "";
  let i = 0;
  let key = 0;

  const pushCurrent = () => {
    if (current) {
      if (keywords.includes(current)) {
        tokens.push(
          <span key={key++} className="token-keyword">
            {current}
          </span>
        );
      } else if (types.includes(current)) {
        tokens.push(
          <span key={key++} className="token-type">
            {current}
          </span>
        );
      } else if (/^\d+(\.\d+)?$/.test(current)) {
        tokens.push(
          <span key={key++} className="token-number">
            {current}
          </span>
        );
      } else {
        tokens.push(<span key={key++}>{current}</span>);
      }
      current = "";
    }
  };

  while (i < code.length) {
    const char = code[i];

    // String literals
    if (char === '"') {
      pushCurrent();
      let str = '"';
      i++;
      while (i < code.length && code[i] !== '"') {
        str += code[i];
        i++;
      }
      str += '"';
      i++;
      tokens.push(
        <span key={key++} className="token-string">
          {str}
        </span>
      );
      continue;
    }

    // Comments
    if (char === "/" && code[i + 1] === "/") {
      pushCurrent();
      let comment = "";
      while (i < code.length && code[i] !== "\n") {
        comment += code[i];
        i++;
      }
      tokens.push(
        <span key={key++} className="token-comment">
          {comment}
        </span>
      );
      continue;
    }

    // Punctuation and operators
    if (/[{}()\[\]:;.,=<>+\-*/%|&!?@]/.test(char)) {
      pushCurrent();
      if (char === "-" && code[i + 1] === ">") {
        tokens.push(
          <span key={key++} className="token-operator">
            {"->"}
          </span>
        );
        i += 2;
        continue;
      }
      tokens.push(
        <span key={key++} className="token-punctuation">
          {char}
        </span>
      );
      i++;
      continue;
    }

    // Whitespace
    if (/\s/.test(char)) {
      pushCurrent();
      tokens.push(<span key={key++}>{char}</span>);
      i++;
      continue;
    }

    // Identifiers
    current += char;
    i++;
  }

  pushCurrent();
  return tokens;
}

function CodeBlock({ code }: { code: string }) {
  return (
    <pre className="font-mono text-xs md:text-sm text-gray-300 whitespace-pre overflow-x-auto">
      {tokenize(code)}
    </pre>
  );
}

export default function Syntax() {
  const [isVisible, setIsVisible] = useState(false);
  const [activeCategory, setActiveCategory] = useState(0);
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

  return (
    <section
      ref={sectionRef}
      className="scroll-section relative bg-[var(--color-cream)] py-20 overflow-hidden">
      {/* Decorative elements */}
      <div className="absolute top-1/4 right-0 w-96 h-96 rounded-full bg-[var(--color-forest)] opacity-5 blur-3xl" />
      <div className="absolute bottom-1/4 left-0 w-64 h-64 rounded-full bg-[var(--color-gold)] opacity-10 blur-3xl" />

      <div className="relative z-10 max-w-7xl mx-auto px-6 h-full flex flex-col justify-center">
        {/* Section header */}
        <div
          className={`text-center mb-16 transition-all duration-1000 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <span className="font-mono text-[var(--color-forest)] text-sm uppercase tracking-widest">
            The Syntax
          </span>
          <h2 className="font-serif text-5xl md:text-6xl font-bold text-[var(--color-slate)] mt-4">
            Familiar, Yet{" "}
            <span className="text-[var(--color-rust)]">Fresh</span>
          </h2>
          <p className="mt-6 text-xl text-[var(--color-slate-light)] font-serif italic max-w-2xl mx-auto">
            If you've written Swift, Rust, or TypeScript, you'll feel right at
            home. If you haven't, you'll wonder why you waited.
          </p>
        </div>

        {/* Category tabs */}
        <div
          className={`flex justify-center gap-4 mb-12 transition-all duration-1000 delay-200 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          {syntaxFeatures.map((feature, i) => (
            <button
              key={i}
              onClick={() => setActiveCategory(i)}
              className={`px-6 py-3 rounded-xl font-serif text-lg transition-all ${
                i === activeCategory
                  ? "bg-[var(--color-rust)] text-white shadow-lg"
                  : "bg-white text-[var(--color-slate)] hover:bg-[var(--color-rust)]/10"
              }`}>
              {feature.category}
            </button>
          ))}
        </div>

        {/* Syntax cards */}
        <div
          className={`grid grid-cols-1 md:grid-cols-3 gap-6 transition-all duration-1000 delay-400 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          {syntaxFeatures[activeCategory].items.map((item, i) => (
            <div
              key={i}
              className="bg-[var(--color-slate)] rounded-2xl overflow-hidden shadow-xl hover:shadow-2xl transition-all hover:-translate-y-1">
              {/* Card header */}
              <div className="px-5 py-3 bg-[var(--color-slate-light)]/30 border-b border-white/10 flex items-center gap-2">
                <div className="w-2 h-2 rounded-full bg-[var(--color-rust)]" />
                <span className="font-mono text-sm text-white">
                  {item.name}
                </span>
              </div>

              {/* Code */}
              <div className="p-5">
                <CodeBlock code={item.code} />
              </div>

              {/* Description */}
              <div className="px-5 pb-5">
                <p className="text-gray-400 font-serif text-sm italic">
                  {item.description}
                </p>
              </div>
            </div>
          ))}
        </div>

        {/* Fun tagline */}
        <p
          className={`text-center mt-12 text-[var(--color-slate-light)] font-serif italic transition-all duration-1000 delay-600 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          "Finally, a language where the syntax stays out of your way." — A
          happy developer, probably
        </p>
      </div>
    </section>
  );
}
