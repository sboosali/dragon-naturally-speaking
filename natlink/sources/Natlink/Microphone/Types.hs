{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------
--------------------------------------------------

{-|

From @natlink.txt@:

@
getMicState()

    This function returns the microphone state, which can be one of 'off',
    'on', 'disabled' and 'sleeping'. 
@

-}

module Natlink.Microphone.Types where

--------------------------------------------------
--------------------------------------------------

import qualified "aeson" Data.Aeson.Types as J

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-|

-}
data MicrophoneState

  = MicrophoneOn
  | MicrophoneAsleep
  | MicrophoneOff
  | MicrophoneDisabled
  
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)
  -- deriving newtype  (J.ToJSON,J.FromJSON)

instance J.ToJSON   MicrophoneState where
  toJSON = printMicrophoneState > J.String

instance J.FromJSON MicrophoneState where
  parseJSON
  
    = J.withText "MicrophoneState" go
    
    where
    go = parseMicrophoneState
       > maybe (fail "unknown microphone state") return

--------------------------------------------------

{-| 

-}

printMicrophoneState :: MicrophoneState -> Text
printMicrophoneState = \case
  
  MicrophoneOn          -> "on"
  MicrophoneAsleep      -> "sleeping"
  MicrophoneOff         -> "off"
  MicrophoneDisabled    -> "disabled"

--------------------------------------------------

{-| 

-}

parseMicrophoneState :: Text -> Maybe MicrophoneState
parseMicrophoneState = \case
  
  "on"          -> Just MicrophoneOn
  "sleeping"    -> Just MicrophoneAsleep
  "off"         -> Just MicrophoneOff
  "disabled"    -> Just MicrophoneDisabled
  _             -> Nothing

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
--------------------------------------------------