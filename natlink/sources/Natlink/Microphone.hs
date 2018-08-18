--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Microphone
 ( module Natlink.Microphone
 , module Natlink.Microphone.Types
 ) where

--------------------------------------------------

import Natlink.Microphone.Types

--------------------------------------------------

import Prelude_natlink()

--------------------------------------------------
--------------------------------------------------

{-| Inverts 'MicrophoneOn' with 'MicrophoneOff'.
'MicrophoneAsleep' becomes 'MicrophoneOn' (i.e. "wakes up").

'toggleMicrophone' is not a simple <https://en.wikipedia.org/wiki/Involution_(mathematics) involution> like 'not',
but it does satisfy the weaker property:

@
'toggleMicrophone'
≡
('toggleMicrophone' '.' 'toggleMicrophone' '.' 'toggleMicrophone')
@

-}

toggleMicrophone :: MicrophoneState -> MicrophoneState
toggleMicrophone = \case
  
  MicrophoneOff      -> MicrophoneOn
  MicrophoneAsleep   -> MicrophoneOn
  MicrophoneOn       -> MicrophoneOff
  MicrophoneDisabled -> MicrophoneDisabled

--------------------------------------------------

{-| 

-}



--------------------------------------------------
--------------------------------------------------