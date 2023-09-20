module LemmatchersSpec where

import Test.Hspec
import Lemmatchers.Matchers
import Lemmatchers.TagRecords

spec :: Spec
spec = describe "Lemmatchers tests" $ do
  simpleExample

simpleExample :: Spec
simpleExample = describe "Simple example" $ do

  let matchersString =  "matcher Foo Bar\n\
                        \PN N DET\n\
                        \\n\
                        \---\n\
                        \\n\
                        \matcher Baz Quux\n\
                        \-N (PN PRP)\n\
                        \-(DN AV) * V\n"

      p1 = Pattern [Exact PN, Exact N, Exact DET]
      p2 = Pattern [Not N, OneOf [PN, PRP]]
      p3 = Pattern [NoneOf [DN, AV], Any, Exact V]
      m1 = Matcher "Foo Bar" [p1]
      m2 = Matcher "Baz Quux" [p2, p3]
      matchers = Matchers [m1, m2]

      lr1 = LR "1" "d1" (Lemmas [ Lemma "a" "b" NA ] )
      lr2 = LR "2" "d2" (Lemmas [ Lemma "c" "d" MOD
                                , Lemma "e" "f" PN
                                , Lemma "g" "h" N
                                , Lemma "i" "j" DET
                                , Lemma "k" "l" DET
                                ] )
      lr3 = LR "3" "d3" (Lemmas [ Lemma "m" "n" MOD
                                , Lemma "o" "p" PRP
                                , Lemma "q" "r" NU
                                , Lemma "s" "t" REL
                                , Lemma "u" "v" V
                                ] )

      mm1lr1  = matchRecord m1 lr1
      mm1lr2  = matchRecord m1 lr2
      mm1lr3  = matchRecord m1 lr3
      mm2lr1  = matchRecord m2 lr1
      mm2lr2  = matchRecord m2 lr2
      mm2lr3  = matchRecord m2 lr3
      matches = concat [mm1lr1, mm1lr2, mm1lr3, mm2lr1, mm2lr2, mm2lr3]
      
  context "Stringification" $ do
    it "Parse correct Matchers" $
      read matchersString `shouldBe` matchers
    it "Roundtrip: read . show" $
      read (show matchers) `shouldBe` matchers
    it "Roundtrip: show . read" $
      show (read matchersString :: Matchers) `shouldBe` matchersString

  context "Matching with Match stringification" $ do
    it "m1 lr1: no matches" $ mm1lr1 `shouldBe` []
    it "m1 lr2: one correct match" $
      map show mm1lr2 `shouldBe`
        [ "Foo Bar ; PN N DET ; 2 ; d2 ; e[f]PN g[h]N i[j]DET" ]
    it "m1 lr3: no matches" $ mm1lr3 `shouldBe` []
    it "m2 lr1: no matches" $ mm2lr1 `shouldBe` []
    it "m2 lr2: one correct match" $
      map show mm2lr2 `shouldBe`
        [ "Baz Quux ; -N (PN PRP) ; 2 ; d2 ; c[d]MOD e[f]PN" ]
    it "m2 lr3: two correct matches" $
      map show mm2lr3 `shouldBe`
        [ "Baz Quux ; -N (PN PRP) ; 3 ; d3 ; m[n]MOD o[p]PRP"
        , "Baz Quux ; -(DN AV) * V ; 3 ; d3 ; q[r]NU s[t]REL u[v]V"
        ]

  context "CSV import and export" $ do

    it "Correct records parsing from CSV" $
      csvToLemmaRecords
        "id_text,Designation,lemma\r\n\
        \1,d1,a[b]NA\r\n\
        \2,d2,c[d]MOD e[f]PN g[h]N i[j]DET k[l]DET\r\n\
        \3,d3,m[n]MOD o[p]PRP q[r]NU s[t]REL u[v]V\r\n\
        \"
        `shouldBe` [lr1, lr2, lr3]

    it "Correct matching results CSV export" $
      matchesToCsv matches `shouldBe`
        "matcher,pattern,id_text,Designation,lemma\r\n\
        \Foo Bar,PN N DET,2,d2,e[f]PN g[h]N i[j]DET\r\n\
        \Baz Quux,-N (PN PRP),2,d2,c[d]MOD e[f]PN\r\n\
        \Baz Quux,-N (PN PRP),3,d3,m[n]MOD o[p]PRP\r\n\
        \Baz Quux,-(DN AV) * V,3,d3,q[r]NU s[t]REL u[v]V\r\n\
        \"
