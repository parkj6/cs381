module Sentence where

-- s ::= n v n | s `and` s
-- n ::= `cats` | `dogs` | `ducks`
-- v ::= `chase` | `cuddle`

data Noun = Cats | Dogs | Ducks
  deriving (Eq,Show)

data Verb = Chase | Cuddle
  deriving (Eq,Show)

data Sentence = NVN Noun Verb Noun
              | And Sentence Sentence
  deriving (Eq,Show)

-- | The sentence: cats chase dogs and dogs cuddle ducks
ex1 :: Sentence
ex1 = And (NVN Cats Chase Dogs) (NVN Dogs Cuddle Ducks)

-- | Checks to see if we only cuddle.
isNice :: Sentence -> Bool
-- isNice (NVN n1 v n2) = v == Cuddle
isNice (NVN _ Cuddle _) = True
-- isNice (NVN _ Chase  _) = False
isNice (And s1 s2)      = isNice s1 && isNice s2
