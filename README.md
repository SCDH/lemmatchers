# *Lemma*tchers

**Search tag sequences in large lemma data**

## Requirements

- Standard Haskell environment (GHC, cabal)

## Build & Run

Build with dependencies:

```
cabal build --enable-tests
```

Run tests:

```
cabal test --test-show-details=direct
```

### Run command line interface

Works with STDIN/STDOUT:

```
cabal run -v0 lemmatchers-cli matchers_file < input.csv > results.csv
```

### Run the web frontend

```
cabal run -v0 lemmatchers-web
```

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
