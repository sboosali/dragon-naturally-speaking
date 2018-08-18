--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Correction.Types where

--------------------------------------------------

-- import Natlink.Hypotheses.Types
-- import Natlink.Recognition.Types

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-|

Training (i.e. correcting misrecognitions) can fail if:

* 'CorrectionHeterophonic': the transcription is not close enough to the utterance;
* 'CorrectionInvalidWord': the transcription has unknown words (i.e. words that aren't in the current user's vocabulary).

-}

data CorrectionStatus

  = CorrectionSuccess
  | CorrectionHeterophonic
  | CorrectionInvalidWord
  
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------