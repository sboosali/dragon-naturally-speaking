{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Term where

--------------------------------------------------

import Natlink.Term.Types

--------------------------------------------------

---import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-|

@

>>> parseTerm "the"
Term "the" Nothing Nothing

>>> parseTerm "A\\a\\letter"
Term "A" (Just "a") (Just LetterCategory)

@

-}

parseTerm :: String -> Term
parseTerm t = case ts of 
 [p]      -> Term p Nothing  Nothing -- NOTE the default and most frequent 
 [p,w]    -> Term p (Just w) Nothing 
 [p,w,c]  -> Term p (Just w) (parseCategory c)
 _        -> defaultTerm t
 where
 ts
  = fromString t
  & T.splitOn "\\"
  & fmap T.unpack

--------------------------------------------------

{-|

e.g.

@
>>> parseCategory "determiner"
DeterminerCategory
@


-}

parseCategory :: String -> Maybe WordCategory
parseCategory
  = go
  --TODO > maybe UnknownCategory id
  where
  go = \case
    "determiner" -> Just DeterminerCategory
    "letter"     -> Just LetterCategory
    "number"     -> Just NumberCategory
    _            -> Nothing

--------------------------------------------------
--------------------------------------------------