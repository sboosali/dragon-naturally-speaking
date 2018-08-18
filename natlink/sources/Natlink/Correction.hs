--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Correction where

--------------------------------------------------

import Natlink.Correction.Types
--import Natlink.Hypotheses.Types
--import Natlink.Recognition.Types

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

-- | 'True' is "good", 'False' is "bad".
isSuccessfulCorrectionStatus :: CorrectionStatus -> Bool
isSuccessfulCorrectionStatus = \case
  CorrectionSuccess      -> True
  CorrectionHeterophonic -> False
  CorrectionInvalidWord  -> False

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------