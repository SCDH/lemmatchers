# *Lemma*tchers

**Search tag sequences in large lemma data**

## Requirements

- Standard Haskell environment (GHC, cabal)

## Build

```
cabal build
```

## Run

```
cabal run lemmatchers-cli
```

## Dateien

Aktuell werden die Daten aus `data/data.csv` gelesen, die Matcher aus `data/matchers.txt` gelesen und die Ergebnisse nach `data/results.csv` geschrieben.

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
N PN
PN N N DN N DET PN N LN
```

Diese Matcher "treffen" genau die dort durch Leerzeichen getrennten Lemmata in der definierten Reihenfolge. Jede durch Leerzeichen getrennte Stelle entspricht einem Lemma, es kann nichts übersprungen werden. Es sind jedoch äußerst flexible Prädikate für einzelne Lemmata möglich:

- `NU` trifft nur den Tag `NU`.
- `-QP` trifft alles bis auf den Tag `QP`.
- `(GN REL XP)` trifft die Tags `GN` **oder** `REL` **oder** `XP`. (Beliebige Länge erlaubt.)
- `-(PRP QN)` trifft alle Tags, die **nicht** `PRP` **und nicht** `QN` sind. (Beliebige Länge erlaubt.)
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
