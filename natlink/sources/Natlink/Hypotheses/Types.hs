
--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Hypotheses.Types where

--------------------------------------------------
--------------------------------------------------

import Prelude_natlink

--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

{-| The maximum number of hypotheses that Dragon NaturallySpeaking can detect.

-}

pattern MAXIMUM_RECOGNITION_HYPOTHESES :: Natural
pattern MAXIMUM_RECOGNITION_HYPOTHESES = 10

--------------------------------------------------

{-| Dragon NaturallySpeaking's hypotheses are the alternative recognitions,
with lower recognition confidence.

They can be presented to the speaker for:

* Correction: e.g. When they say "Correct That". By presenting the several most probable recognitions for some utterance, the speaker might simply select one, which takes one single key press or mouse click; rather than editing various words by typing multiple characters.
* Confirmation: e.g. When dictating into an input (e.g. form, buffer, etc) where the speaker wants to explicitly confirm

-}


--------------------------------------------------
--------------------------------------------------