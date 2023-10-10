# *Lemma*tchers

**Tag sequences domain specific language with CLI and web interface for CSV data**

[![build, test, docs](https://github.com/SCDH/lemmatchers/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/SCDH/lemmatchers/actions/workflows/haskell-ci.yml)
[![API docs](https://img.shields.io/badge/API_docs-Haddock-mediumorchid?logo=bookstack&logoColor=lightgrey&labelColor=333)](https://scdh.github.io/lemmatchers/)

## Requirements

- Standard Haskell environment (GHC, cabal)

## Build & Run

This package uses standard cabal procedures like these to build, test and run the executables as well as a `ghci` with the `Lemmatchers` library pre-loaded:

```
$ cabal build --enable-tests
$ cabal test --test-show-details=direct
$ cabal run -v0 lemmatchers-cli matchers_file < input.csv > results.csv
$ cabal run -v0 lemmatchers-web
$ cabal repl
```

but there's also a [Makefile](Makefile) to make it easy to compile the programs `lemmatchers-cli` and `lemmatchers-web` as executable files in the `compiled` directory with `make install`:

The command line interface expects a matchers file as command line argument, reads CSV from STDIN and writes CSV to STDOUT:

```
$ compiled/lemmatchers-cli data/matchers.txt < data/data.csv > results.csv
```

To have the web interface at `http://localhost:4217/`, just call the web backend without arguments:

```
$ compiled/lemmatchers-web
Serving Lemmatchers from port 4217...
```

## Dockerized web interface

This repository contains a Dockerfile, so you can built and run a container with standard docker procedures:

```
$ docker build -t lemmatchers-web .
$ docker run -dp 8080:4217 lemmatchers-web
```

to run the web interface at `http://localhost:8080`.

## Matcher

Die Matcher sind mit `---` und Leerzeilen getrennt und können mehrere Pattern enthalten:

```
matcher Two Tier
PN N DET PN

---

matcher Three Tier
PN N DET PN N LN

---

matcher Probleme
(N PN) CNJ
PN -(DN N) * -PN
```

Diese Matcher "treffen" genau die dort durch Leerzeichen getrennten Lemmata in der definierten Reihenfolge. Jede durch Leerzeichen getrennte Stelle entspricht einem Lemma, es kann nichts übersprungen werden. Es sind jedoch äußerst flexible Prädikate für einzelne Lemmata möglich:

- `CNJ` trifft nur den Tag `CNJ`.
- `-PN` trifft alles bis auf den Tag `PN`.
- `(N PN)` trifft die Tags `N` **oder** `PN`. (Beliebige Länge erlaubt.)
- `-(DN N)` trifft alle Tags, die **nicht** `DN` **und nicht** `N` sind. (Beliebige Länge erlaubt.)
- `*` trifft alle Tags.

Eine Analyse der Beispieldaten hat folgende erlaubte Tags ergeben:

```haskell
data Tag
  = AJ | AV | CNJ | DET | DN | DP | EN | GN | IP | J | LN
  | MN | MOD | N | NA | NU | PN | PRP | QN | QP | REL | RN
  | SN | TN | V | WN | XP
```

## Contributors and License

&copy; 2023 Mirko Westermeier  
Service Center for Digital Humanities  
University of Münster

Released under the MIT license (see LICENSE for details).
