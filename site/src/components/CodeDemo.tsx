import { Play, RotateCcw, X } from "lucide-react";
import { useCallback, useEffect, useRef, useState } from "react";

interface CodeExample {
  title: string;
  filename: string;
  code: string;
  output: string[];
}

const codeExamples: CodeExample[] = [
  {
    title: "Self-Documenting Calls",
    filename: "game.ks",
    code: `// No more mystery parameters
let player = Player(
    named: "Kestrel",
    at: Position(x: 100, y: 200),
    withHealth: 100
)

// Compare to: Player("Kestrel", Position(100, 200), 100)
// Which one would you rather read at 2am?

player.move(to: Position(x: 150, y: 200), speed: 5.0)
player.takeDamage(amount: 25, from: "goblin")

print(player.name, "has", player.health, "HP")`,
    output: ["> Kestrel has 75 HP"],
  },
  {
    title: "Fearless Refactoring",
    filename: "api.ks",
    code: `// The compiler catches your mistakes

struct User {
    let id: Int
    let email: String
    let role: Role
}

enum Role {
    case admin
    case member
    case guest
}

func permissions(for user: User) -> [Permission] {
    match user.role {
        .admin  => Permission.all(),
        .member => [.read, .write],
        .guest  => [.read]
        // Forgot a case? Compiler says no.
    }
}`,
    output: ["> Compiled successfully ✓"],
  },
  {
    title: "No Null. Ever.",
    filename: "safe.ks",
    code: `// Optional forces you to handle the absence

func findUser(byId id: Int) -> User? {
    database.users.find { $0.id == id }
}

let user = findUser(byId: 42)

// This won't compile:
// print(user.name)  // Error: user might be nil

// This will:
match user {
    .some(u) => print("Found:", u.name),
    .none    => print("User not found")
}

// Or use the ? operator for ergonomics
let name = user?.name ?? "Anonymous"`,
    output: ["> Found: Alice"],
  },
  {
    title: "Iterators That Compose",
    filename: "data.ks",
    code: `// Chain operations without intermediate allocations

let transactions = accounts
    .filter { $0.isActive }
    .flatMap { $0.transactions }
    .filter { $0.amount > 1000.0 }
    .map { ($0.date, $0.amount) }
    .take(10)
    .collect()

// Lazy evaluation - only computes what you need
let first = users
    .filter { $0.age >= 18 }
    .find { $0.country == "Japan" }

print("Found:", first?.name ?? "nobody")`,
    output: ["> Found: Yuki"],
  },
];

function tokenize(code: string): React.ReactNode[] {
  const keywords = [
    "struct",
    "enum",
    "case",
    "protocol",
    "func",
    "let",
    "var",
    "fn",
    "if",
    "else",
    "for",
    "while",
    "return",
    "import",
    "init",
    "static",
    "mutating",
    "self",
    "true",
    "false",
    "public",
    "match",
  ];
  const types = [
    "Int",
    "Float",
    "String",
    "Bool",
    "Array",
    "Optional",
    "User",
    "Role",
    "Permission",
    "Player",
    "Position",
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
            -&gt;
          </span>
        );
        i += 2;
        continue;
      }
      if (char === "=" && code[i + 1] === ">") {
        tokens.push(
          <span key={key++} className="token-operator">
            =&gt;
          </span>
        );
        i += 2;
        continue;
      }
      if (char === "|" && code[i + 1] === "|") {
        tokens.push(
          <span key={key++} className="token-operator">
            ||
          </span>
        );
        i += 2;
        continue;
      }
      if (char === "?" && code[i + 1] === "?") {
        tokens.push(
          <span key={key++} className="token-operator">
            ??
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

export default function CodeDemo() {
  const [currentExample, setCurrentExample] = useState(0);
  const [displayedCode, setDisplayedCode] = useState("");
  const [showOutput, setShowOutput] = useState(false);
  const [isTyping, setIsTyping] = useState(false);
  const [isVisible, setIsVisible] = useState(false);
  const sectionRef = useRef<HTMLElement>(null);
  const typingRef = useRef<number | null>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting && !isVisible) {
          setIsVisible(true);
        }
      },
      { threshold: 0.3 }
    );

    if (sectionRef.current) {
      observer.observe(sectionRef.current);
    }

    return () => observer.disconnect();
  }, [isVisible]);

  const typeCode = useCallback((code: string) => {
    setIsTyping(true);
    setShowOutput(false);
    setDisplayedCode("");

    let index = 0;
    const typeChar = () => {
      if (index < code.length) {
        setDisplayedCode(code.slice(0, index + 1));
        index++;
        // Variable speed - faster for whitespace
        const delay = /\s/.test(code[index - 1]) ? 8 : 18;
        typingRef.current = window.setTimeout(typeChar, delay);
      } else {
        setIsTyping(false);
        // Show output after a pause
        typingRef.current = window.setTimeout(() => setShowOutput(true), 500);
      }
    };

    typeChar();
  }, []);

  useEffect(() => {
    if (isVisible) {
      typeCode(codeExamples[currentExample].code);
    }

    return () => {
      if (typingRef.current) {
        clearTimeout(typingRef.current);
      }
    };
  }, [currentExample, isVisible, typeCode]);

  const handleExampleChange = (index: number) => {
    if (typingRef.current) {
      clearTimeout(typingRef.current);
    }
    setCurrentExample(index);
  };

  const example = codeExamples[currentExample];

  return (
    <section
      ref={sectionRef}
      className="scroll-section relative bg-[var(--color-slate)] overflow-hidden">
      {/* Subtle pattern */}
      <div className="absolute inset-0 opacity-[0.05]">
        <div
          className="absolute inset-0"
          style={{
            backgroundImage: `radial-gradient(circle at 2px 2px, white 1px, transparent 0)`,
            backgroundSize: "32px 32px",
          }}
        />
      </div>

      {/* Accent glow */}
      <div className="absolute top-1/4 -right-32 w-96 h-96 bg-[var(--color-rust)] opacity-10 blur-3xl rounded-full" />
      <div className="absolute bottom-1/4 -left-32 w-64 h-64 bg-[var(--color-forest)] opacity-10 blur-3xl rounded-full" />

      <div className="relative z-10 h-full flex flex-col justify-center px-6 md:px-12 lg:px-24 py-20">
        {/* Section header - left aligned */}
        <div
          className={`max-w-2xl mb-12 transition-all duration-1000 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <span className="font-mono text-[var(--color-rust-light)] text-sm uppercase tracking-widest">
            See It In Action
          </span>
          <h2 className="font-serif text-5xl md:text-6xl lg:text-7xl font-black text-white mt-4 tracking-tight">
            Code That <span className="text-[var(--color-gold)]">Speaks.</span>
          </h2>
          <p className="mt-4 text-xl text-white/60 font-mono">
            Watch Kestrel come to life.
          </p>
        </div>

        {/* Code editor */}
        <div
          className={`bg-[#1a2a3a] rounded-xl shadow-2xl overflow-hidden transition-all duration-1000 delay-200 max-w-4xl ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          {/* Window controls and tabs bar */}
          <div className="flex items-center bg-[#0d1a24] border-b border-white/5">
            {/* Window controls */}
            <div className="flex gap-2 px-4 py-3">
              <div className="w-3 h-3 rounded-full bg-[var(--color-rust)]" />
              <div className="w-3 h-3 rounded-full bg-[var(--color-gold)]" />
              <div className="w-3 h-3 rounded-full bg-[var(--color-forest)]" />
            </div>

            {/* IDE-style tabs */}
            <div className="flex-1 flex items-end overflow-x-auto">
              {codeExamples.map((ex, i) => (
                <button
                  key={i}
                  onClick={() => handleExampleChange(i)}
                  className={`relative flex items-center gap-2 px-4 py-2 font-mono text-xs transition-all border-r border-white/5 whitespace-nowrap ${
                    i === currentExample
                      ? "bg-[#1a2a3a] text-white/90"
                      : "bg-[#0d1a24] text-white/40 hover:text-white/60 hover:bg-white/5"
                  }`}>
                  <span
                    className={`w-2 h-2 rounded-full flex-shrink-0 ${
                      i === currentExample
                        ? "bg-[var(--color-gold)]"
                        : "bg-white/20"
                    }`}
                  />
                  {ex.filename}
                  {i === currentExample && (
                    <X className="w-3 h-3 text-white/30 hover:text-white/60 ml-1 flex-shrink-0" />
                  )}
                  {/* Active tab indicator */}
                  {i === currentExample && (
                    <div className="absolute bottom-0 left-0 right-0 h-[2px] bg-[var(--color-gold)]" />
                  )}
                </button>
              ))}
            </div>
          </div>

          {/* Code content */}
          <div className="p-6 font-mono text-sm leading-relaxed min-h-[400px] overflow-x-auto">
            <pre className="text-gray-300 whitespace-pre-wrap">
              {tokenize(displayedCode)}
              {isTyping && (
                <span className="cursor-blink text-[var(--color-rust)]">▌</span>
              )}
            </pre>
          </div>

          {/* Console output */}
          <div
            className={`border-t border-white/10 transition-all duration-500 ${
              showOutput ? "opacity-100 max-h-40" : "opacity-0 max-h-0"
            } overflow-hidden`}>
            <div className="p-4 bg-black/20">
              <div className="flex items-center gap-2 mb-2">
                <Play className="w-3 h-3 text-[var(--color-forest)] fill-[var(--color-forest)]" />
                <span className="text-[var(--color-forest)] font-mono text-xs">
                  Output
                </span>
              </div>
              {example.output.map((line, i) => (
                <div
                  key={i}
                  className="font-mono text-sm text-[var(--color-gold)]"
                  style={{
                    animation: showOutput
                      ? `fadeIn 0.3s ease-out ${i * 0.1}s forwards`
                      : "none",
                    opacity: 0,
                  }}>
                  {line}
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Replay button */}
        <div
          className={`mt-6 transition-all duration-1000 delay-400 ${
            isVisible ? "opacity-100 translate-y-0" : "opacity-0 translate-y-10"
          }`}>
          <button
            onClick={() => typeCode(example.code)}
            disabled={isTyping}
            className="inline-flex items-center gap-2 px-4 py-2 text-white/70 font-mono text-sm hover:text-[var(--color-gold)] transition-all disabled:opacity-50 disabled:cursor-not-allowed">
            <RotateCcw className="w-4 h-4" />
            {isTyping ? "Typing..." : "Replay"}
          </button>
        </div>
      </div>

      <style>{`
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(5px); }
          to { opacity: 1; transform: translateY(0); }
        }
      `}</style>
    </section>
  );
}
