--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Recognition.Types where

--------------------------------------------------

import Natlink.Term.Types

--------------------------------------------------

---import qualified "aeson" Data.Aeson.Types as J

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Recognition = Recognition 

  [Term]

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)
  -- deriving newtype  (J.ToJSON,J.FromJSON)

instance IsList Recognition where
  type Item Recognition = Term
  fromList = coerce
  toList   = coerce

--------------------------------------------------

{-| 

-}



--------------------------------------------------
--------------------------------------------------