.PHONY: all deps build repl test install clean_compiled clean

all: build test install

deps:
	cabal build --enable-tests --only-dependencies

build:
	cabal build --enable-tests

repl:
	cabal repl

test:
	cabal test --test-show-details=direct

install: clean_compiled
	cabal install --install-method=copy --installdir=compiled

clean_compiled:
	rm -r compiled/lemmatchers-*

clean: clean_compiled
	cabal clean
