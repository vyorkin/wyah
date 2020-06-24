GHC_OPTIONS := --ghc-options='-fdiagnostics-color=never -ferror-spans -fhide-source-paths' # -fprint-unicode-syntax

dev: all
	ghcid --restart=src/Wyah/Chapter5/Lexer.x --restart=src/Wyah/Chapter5/Parser.y  --command="cabal new-repl $(GHC_OPTIONS)" | source-highlight -s haskell -f esc
repl:
	cabal new-repl $(GHC_OPTIONS)

all:
	cabal new-build $(GHC_OPTIONS) all
clean:
	cabal new-clean
check:
	cabal new-check
tags:
	rm -f tags codex.tags
	codex update --force
	haskdogs --hasktags-args "-b"
prof:
	cabal new-configure --enable-profiling
noprof:
	cabal new-configure --disable-profiling
hoogle:
	hoogle server --local

.PHONY: dev repl clean all check tags prof noprof hoogle
