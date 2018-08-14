--------------------------------------------------

{-|



-}

module Natlink.Mode.Types where

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------

import "enumerate" Enumerate

--------------------------------------------------

import "aeson" Data.Aeson

--------------------------------------------------

{-|

<http://www.nuance.com/naturallyspeaking/customer-portal/documentation/userguide/chapter7/ug_chapter7_switch_recognition_mode.asp>

-}
data Mode

 = NormalMode                    -- ^ Both Dictation and Commands
 | DictationMode                 -- ^ Just Dictation.
 | CommandMode                   -- ^ Just Commands.
 | NumbersMode                   -- ^ Like Dictation, but only Numbers.
 | SpellMode                     -- ^ Like Dictation, but only IPA Phonetic Alphabet.

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)
  deriving anyclass (Enumerable)
  deriving anyclass (ToJSON,FromJSON)

-- | @= 'defaultMode'@
instance Default Mode where
  def = defaultMode

-- | @= 'NormalMode'@
defaultMode :: Mode
defaultMode = NormalMode

--------------------------------------------------