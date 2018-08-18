{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, PatternSynonyms #-}

--------------------------------------------------
--------------------------------------------------

{-| The speech engine API, as exposed by
<https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L106 Natlink>

NOTE natlink is single-threaded, and so it can only act as a client, not a server

TODO websockets + setTimerCallback ?

'''gotBeginCallback() is called at start of every recognition. Recognizer will block until you return.
During callback, do bookkeeping:
Make sure text is synchronized with application
Get the location of the insertion point from the app.
Activate or deactivate grammars
Update select grammar from text
Update dictation context
Update “Resume With” word list'''

ADVANCED FORMATTING 
NatSpeak’s VDct uses a chart parser to format dates, time, numbers, currency, etc.
"one hundred dollars and two cents" -> $100.02

-}

module Natlink.Types
  ( module Natlink.Types
  , module Natlink.Audio.Types
  , module Natlink.Correction.Types
  , module Natlink.DragonScript.Types
  , module Natlink.Grammar.Types
  , module Natlink.Hypotheses.Types
  , module Natlink.Microphone.Types
  , module Natlink.Mode.Types
  , module Natlink.Python.Types
  , module Natlink.Recognition.Types
  , module Natlink.Term.Types
  ) where

--------------------------------------------------
--------------------------------------------------

import Natlink.Audio.Types
import Natlink.Correction.Types
import Natlink.DragonScript.Types
import Natlink.Grammar.Types
import Natlink.Hypotheses.Types
import Natlink.Microphone.Types
import Natlink.Mode.Types
import Natlink.Python.Types
import Natlink.Recognition.Types
import Natlink.Term.Types

--------------------------------------------------
--------------------------------------------------

{-
import           Data.Aeson (ToJSON,FromJSON) --TODO rm
import Data.Maybe (isJust)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Prelude_natlink

--------------------------------------------------------------------------------

{-|

-}
newtype DNSBuffer = DNSBuffer String



--------------------------------------------------------------------------------

type ForeignIdentifier = Int

data GrammarObject = GrammarObject_ ForeignIdentifier -- { getGrammarObject :: Int }
 deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   GrammarObject
instance Hashable GrammarObject

unsafeGrammarObject :: ForeignIdentifier -> GrammarObject
unsafeGrammarObject = GrammarObject_

newtype SelectionGrammarObject = SelectionGrammarObject_ ForeignIdentifier
 deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   SelectionGrammarObject
instance Hashable SelectionGrammarObject

unsafeSelectionGrammarObject :: ForeignIdentifier -> SelectionGrammarObject
unsafeSelectionGrammarObject = SelectionGrammarObject_

data ResultsObject = ResultsObject_ ForeignIdentifier -- { getResultsObject :: Int }
   deriving (Show,Eq,Ord,Generic) -- Show only for debugging
instance NFData   ResultsObject
instance Hashable ResultsObject

unsafeResultsObject :: ForeignIdentifier -> ResultsObject
unsafeResultsObject = ResultsObject_

data SelectionResultsObject = SelectionResultsObject_ ForeignIdentifier -- { getResultsObject :: Int }
unsafeSelectionResultsObject :: ForeignIdentifier -> SelectionResultsObject
unsafeSelectionResultsObject = SelectionResultsObject_

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

{-|

-}
data ControlGrammar = ControlGrammar
  { _controlProperties    :: GrammarProperties
  , _controlConfiguration :: DNSGrammar DNSInfo DNSText DNSName
  } deriving (Show,Eq,Ord,Generic)

-- data ControlConfiguration = ControlConfiguration
--   { _control ::
--   }

{-|

-}
data ControlResults = ControlResults
  { _controlRecognition :: Recognition
--  , _control ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)


--------------------------------------------------------------------------------

{-|

Whenever the selection buffer is updated, Dragon NaturallySpeaking
implicitly constructs a grammar with this production:

@
<'selectionSelectWords'> <subsequence> [ <'selectionThroughWords'> <subsequence> ]
@

where @<selectionSelectWords>@ and @<selectionThroughWords>@ are lists, and
@<subsequence>@ is any subsequence in the selection buffer.

Manually constructing @<subsequence>@, in a (non-selection) 'Grammar', would be
extremely inefficient, as it grows quadradically in the size of the buffer.

also see @MacroSystem/core/natlinkutils.py#L755@

https://github.com/sboosali/NatLink/blob/9545436181f23652224041afa2035f12fa60d949/MacroSystem/core/natlinkutils.py#L755

"A select XYZ grammar is a special grammar which
recognizes an utterance of the form "<verb> <text> [ through <text> ]"
where <verb> is specified and <text> is an arbitrary sequence of words in
a specified text buffer."

-}
data SelectionGrammar = SelectionGrammar
  -- { selectionStatus :: Status
  -- , selectionExclusivity :: Exclusivity
  -- , selectionShouldEavesdrop :: ShouldEavesdrop
  -- , selectionShouldHypothesize :: ShouldHypothesize
  { _selectionProperties    :: GrammarProperties
  , _selectionConfiguration :: SelectionConfiguration
--  , selection ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)

{-|

-}
defaultSelectionGrammar :: SelectionGrammar
defaultSelectionGrammar = SelectionGrammar{..}
 where
 _selectionProperties = defaultGrammarProperties
 _selectionConfiguration = defaultSelectionConfiguration

{-|

-}
data SelectionConfiguration = SelectionConfiguration
  { _selectionSelectWords  :: [String]
  , _selectionThroughWords :: [String]
--  , selection ::
  } deriving (Show,Read,Eq,Ord,Data,Generic)

defaultSelectionConfiguration :: SelectionConfiguration
defaultSelectionConfiguration = SelectionConfiguration{..}
 where
 _selectionSelectWords = ["select","correct","insert before","insert after","capitalize"] -- TODO Reproduce all NaturallySpeaking's Selection commands
 _selectionThroughWords = ["through","until"] -- TODO non-words, so they dont conflict with the buffer itself

data SelectionSettingStatus = SelectionSettingSuccess

{-|

see @ResultsObject.getSelectInfo()@

-}
data SelectionResults = SelectionResults
 { _sroRecognition :: Recognition -- ^ A subsequence of the selection buffer
 , _sroOffset      :: Natural     -- ^ Bounded by the length of the selection buffer -- TODO Necessary?
 } deriving (Show,Read,Eq,Ord,Data,Generic)


--------------------------------------------------------------------------------

data ChangeCallBack = MicrophoneCallBack PythonExpression | UserCallBack PythonExpression 
 
class MonadNatlink m where

 executeScript :: DragonScriptExpression -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 recognitionMimic :: Recognition -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)

 setMicrophoneState :: MicrophoneState -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)
 getMicrophoneState :: m (MicrophoneState)  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)

 setTimerCallback :: DragonScriptExpression -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)
 setChangeCallback :: PythonExpression -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/9545436181f23652224041afa2035f12fa60d949/NatlinkSource/natlink.txt#L520) 

 loadGrammarObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 activateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 dectivateGrammarRule :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 setExclusiveGrammar :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

-- getResultsObject :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 getResultsObjectAudio :: (ResultsObject) -> m (Utterance)  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)
 correctResultsObject :: (ResultsObject) -> Recognition -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

 setWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 getWordInfo :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 deleteWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 addWord :: () -> m ()  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 setSelection :: SelectionGrammarObject -> DNSBuffer -> m SelectionSettingStatus

{-| 
data NatlinkF k
 = RecognitionMimic  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)
 | ExecuteScript  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 | InputFromFile  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L249)

 | SetMicrophoneState  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)
 | GetMicrophoneState  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)

 | SetTimerCallback  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)
 | SetChangeCallback -- ^ [documentation](https://github.com/sboosali/NatLink/blob/9545436181f23652224041afa2035f12fa60d949/NatlinkSource/natlink.txt#L520) 

 | SetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 | GetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 | DeleteWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 | AddWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 | LoadGrammarObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 | ActivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 | DectivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 | SetExclusiveGrammar  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

 | GetResultsObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 | GetResultsObjectAudio  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)
 | CorrectResultsObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

-} 
 

{-| 
equivalent to @'NatlinkFunction' ()@.   

[JSON-RPC](http://ku-fpg.github.io/2016/02/09/remote-json/) 
[remote-json package](http://hackage.haskell.org/package/remote-json) 

-}
data NatlinkCommand
 = RecognitionMimic  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L209)
 | ExecuteScript  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L190)
 | InputFromFile  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L249)

 | SetMicrophoneState  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L178)

 | SetTimerCallback  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L279)
 | SetChangeCallback -- ^ [documentation](https://github.com/sboosali/NatLink/blob/9545436181f23652224041afa2035f12fa60d949/NatlinkSource/natlink.txt#L520) 

 | SetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L466)
 | GetWordInfo  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L373)
 | DeleteWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L421)
 | AddWord  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L428)

 | LoadGrammarObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L574)
 | ActivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L601)
 | DectivateGrammarRule  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L614)
 | SetExclusiveGrammar  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L619)

 | GetResultsObject  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L759)
 | GetResultsObjectAudio  -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L753)

{-|

[JSON-RPC](http://ku-fpg.github.io/2016/02/09/remote-json/) 
[remote-json package](http://hackage.haskell.org/package/remote-json) 

-}
data NatlinkFunction a where
 GetMicrophoneState    :: NatlinkFunction MicrophoneState -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L182)
 CorrectResultsObject  :: NatlinkFunction CorrectionStatus -- ^ [documentation](https://github.com/sboosali/NatLink/blob/master/NatlinkSource/natlink.txt#L740)

--------------------------------------------------------------------------------
-}