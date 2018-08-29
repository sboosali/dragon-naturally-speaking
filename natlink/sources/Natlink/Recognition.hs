--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Recognition where

--------------------------------------------------

import Natlink.Term.Types
import Natlink.Recognition.Types

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-|

>>> :set -XOverloadedLists
>>> recognitionOnlyAsWritten ["a", "recognition"]
["a", "recognition"]

-}

recognitionOnlyAsWritten :: Recognition -> [String]
recognitionOnlyAsWritten (Recognition terms) =
  (go `fmap` terms)
  where
  go :: Term -> String
  go Term{written} = written

  -- (terms <&> displayTerm)

--------------------------------------------------

{-|

Returns any 'spoken' (non-orthographic) pronounciations and/or 'category'(ies) present.



-}
doesRecognitionHaveMetadata :: Recognition -> ( [String], [WordCategory] )
doesRecognitionHaveMetadata (Recognition terms) =
  foldr go ([],[]) terms
  where
  go :: Term -> ([String], [WordCategory]) -> ([String], [WordCategory])
  go Term{..} (pronounciations, categories) =
    ( pronounciations ++ maybe2list spoken
    , categories      ++ maybe2list category
    )

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

displayRecognition :: Recognition -> String
displayRecognition (Recognition terms) =
  unwords (displayTerm `fmap` terms)
  where
  displayTerm :: Term -> String
  displayTerm Term{written} = written

  -- (terms <&> displayTerm)

--------------------------------------------------
--------------------------------------------------