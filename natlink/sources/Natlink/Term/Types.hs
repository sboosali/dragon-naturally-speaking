
--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Term.Types where

--------------------------------------------------
--------------------------------------------------

import qualified "aeson" Data.Aeson.Types as J

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-| A word, that:

* Sometimes includes a pronounciation and a word-sense. This can disambiguate homophones; e.g. @"A\\a\\letter"@ versus @"a\\a\\determiner"@.
* Sometimes includes whitespace (or significant punctuation). This comes from phrases that are spoken as single words, or from custom vocabulary (i.e. words the user has added).

When an utterance is recognized, Dragon NaturallySpeaking may return the additional metadata (listed above).

These are the "leaves" of the grammar.

Broadly speaking, given a @Term written spoken@, the speech recognition engine
recognizes @spoken@ and transcribes it as @written@.

e.g.

@
Term
 { 'written'  = \"a\"
 , 'spoken'   = Just \"A\"
 , 'category' = Just 'LetterCategory'
 }
@

-}
data Term = Term
  { written  :: String
  , spoken   :: Maybe String
  , category :: Maybe WordCategory
  }
  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)
  deriving anyclass (J.ToJSON,J.FromJSON)

--------------------------------------------------

{-| Includes both syntactic (e.g. 'DeterminerCategory'),
and semantic (e.g. 'LetterCategories') categories.

-}

data WordCategory

  = DeterminerCategory
  | LetterCategory
  | NumberCategory
  | UnknownCategory -- ^ TODO

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable)
  deriving anyclass (NFData,Hashable)
  deriving anyclass (J.ToJSON,J.FromJSON)

--------------------------------------------------

{-| only 'written'.

-}

defaultTerm :: String -> Term
defaultTerm written = Term{..}
  where
  category = Nothing
  spoken   = Nothing

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------