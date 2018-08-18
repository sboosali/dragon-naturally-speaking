--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Grammar.Types where

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------
--------------------------------------------------

{-| Initialization/configuration properties shared by all grammars.

-}

data GrammarProperties = GrammarProperties
  { status            :: Status
  , exclusivity       :: Exclusivity
  , shouldEavesdrop   :: ShouldEavesdrop
  , shouldHypothesize :: ShouldHypothesize
  }
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultGrammarProperties'@

instance Default GrammarProperties where
  def = defaultGrammarProperties

{-| Equals:

* @'status'               = 'Enabled'@
* @'exclusivity'          = 'Inclusive'@
* @'shouldEavesdrop'      = 'YesEavesdrop'@
* @'shouldHypothesize'    = 'YesHypothesize'@

-}

defaultGrammarProperties :: GrammarProperties
defaultGrammarProperties = GrammarProperties{..}
 where
 status               = Enabled
 exclusivity          = Inclusive
 shouldEavesdrop      = YesEavesdrop
 shouldHypothesize    = YesHypothesize

--------------------------------------------------

{- | A grammar can be both loaded and disabled.

'Bool'-like.

-}

data Status

  = Disabled
  | Enabled

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)

--------------------------------------------------

-- | @≡ 'All'@ w.r.t. @'Enabled' as 'True'@.

instance Semigroup Status where

  Disabled <> Disabled = Disabled
  Disabled <> Enabled  = Disabled
  Enabled  <> Disabled = Disabled

  Enabled  <> Enabled  = Enabled

--------------------------------------------------

{- | Whether the grammar is exclusive, i.e. it disables all other grammars.

'Bool'-like.

-}

data Exclusivity

  = Exclusive
  | Inclusive

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)

--------------------------------------------------

-- | @≡ 'Any'@ w.r.t. @'Exclusive' as 'False'@.

instance Semigroup Exclusivity where
  
  Exclusive <> Exclusive = Exclusive
  Exclusive <> Inclusive = Exclusive
  Inclusive <> Exclusive = Exclusive
  
  Inclusive <> Inclusive = Inclusive

--------------------------------------------------

{- | Whether the grammar should listen to other grammars' recognition-callbacks.

'Bool'-like.

-}

data ShouldEavesdrop

  = YesEavesdrop
  | NoEavesdrop
  
  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)

--------------------------------------------------

{- | hould the grammar listen to each hypothesis's callback

'Bool'-like.

-}

data ShouldHypothesize

  = YesHypothesize
  | NoHypothesize

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Enumerable,NFData,Hashable)

--------------------------------------------------

{-|

from @natlink.txt@:

@
        Can raise InvalidWord if the grammar contains an invalid word.
        Can raise BadGrammar if the grammar has syntax errors, or if it's is too complex
@

-}

data GrammarLoadStatus

  = GrammarLoadSuccess
  | GrammarLoadBadGrammar
  | GrammarLoadInvalidWord

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