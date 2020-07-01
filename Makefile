GHC_OPTIONS := --ghc-options='-fdiagnostics-color=never -ferror-spans -fhide-source-paths' # -fprint-unicode-syntax

dev: all
	ghcid --restart=src/Wyah/Chapter7/Lexer.x --restart=src/Wyah/Chapter7/Parser.y  --command="cabal repl $(GHC_OPTIONS)" | source-highlight -s haskell -f esc
repl:
	cabal repl $(GHC_OPTIONS)
all:
	cabal build $(GHC_OPTIONS) all
clean:
	cabal clean
check:
	cabal check
test:
	cabal test
test-accept:
	cabal test --test-options=--accept
tags:
	rm -f tags codex.tags
	codex update --force
	haskdogs --hasktags-args "-b"
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling
hoogle:
	hoogle server --local

.PHONY: dev repl clean all test check tags prof noprof hoogle
