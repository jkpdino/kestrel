import { Coffee, Github, Heart, MessageCircle, Twitter } from "lucide-react";

export default function Footer() {
  return (
    <footer className="bg-[var(--color-slate)] py-16 relative overflow-hidden">
      {/* Subtle top border */}
      <div className="absolute top-0 left-0 right-0 h-px bg-gradient-to-r from-transparent via-[var(--color-rust)] to-transparent" />

      <div className="max-w-6xl mx-auto px-6">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-12">
          {/* Brand */}
          <div className="md:col-span-2">
            <h3 className="font-serif text-4xl font-bold text-white mb-4">
              Kestrel
            </h3>
            <p className="text-gray-400 font-serif italic max-w-sm">
              A programming language that hovers gracefully between power and
              elegance. Precise. Expressive. Swift.
            </p>
            <div className="mt-6 flex gap-4">
              <a
                href="https://github.com/kestrel-lang/kestrel"
                className="w-10 h-10 rounded-lg bg-white/10 flex items-center justify-center hover:bg-[var(--color-rust)] transition-colors">
                <Github className="w-5 h-5 text-white" />
              </a>
              <a
                href="https://twitter.com/kestrellang"
                className="w-10 h-10 rounded-lg bg-white/10 flex items-center justify-center hover:bg-[var(--color-rust)] transition-colors">
                <Twitter className="w-5 h-5 text-white" />
              </a>
              <a
                href="https://discord.gg/kestrel"
                className="w-10 h-10 rounded-lg bg-white/10 flex items-center justify-center hover:bg-[var(--color-rust)] transition-colors">
                <MessageCircle className="w-5 h-5 text-white" />
              </a>
            </div>
          </div>

          {/* Links */}
          <div>
            <h4 className="font-mono text-[var(--color-rust-light)] text-sm uppercase tracking-wider mb-4">
              Resources
            </h4>
            <ul className="space-y-3">
              {["Documentation", "Examples", "Standard Library", "Roadmap"].map(
                (item) => (
                  <li key={item}>
                    <a
                      href="#"
                      className="text-gray-400 hover:text-white font-serif transition-colors">
                      {item}
                    </a>
                  </li>
                )
              )}
            </ul>
          </div>

          <div>
            <h4 className="font-mono text-[var(--color-rust-light)] text-sm uppercase tracking-wider mb-4">
              Community
            </h4>
            <ul className="space-y-3">
              {["GitHub", "Discord", "Twitter", "Contributing"].map((item) => (
                <li key={item}>
                  <a
                    href="#"
                    className="text-gray-400 hover:text-white font-serif transition-colors">
                    {item}
                  </a>
                </li>
              ))}
            </ul>
          </div>
        </div>

        {/* Bottom bar */}
        <div className="mt-16 pt-8 border-t border-white/10 flex flex-col md:flex-row justify-between items-center gap-4">
          <p className="text-gray-500 font-mono text-sm">
            © {new Date().getFullYear()} Kestrel Programming Language. MIT
            License.
          </p>
          <p className="text-gray-600 font-serif italic text-sm inline-flex items-center gap-1">
            Made with <Heart className="w-4 h-4 text-red-500 fill-red-500" />{" "}
            and a lot of <Coffee className="w-4 h-4" />
          </p>
        </div>

        {/* Easter egg */}
        <div className="mt-8 text-center">
          <p className="text-gray-700 font-mono text-xs hover:text-[var(--color-rust)] transition-colors cursor-default">
            🦅 Fun fact: Kestrels can hover in mid-air while hunting. This
            language can't, but it tries its best.
          </p>
        </div>
      </div>
    </footer>
  );
}
