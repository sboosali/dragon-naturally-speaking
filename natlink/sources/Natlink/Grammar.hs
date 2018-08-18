--------------------------------------------------
--------------------------------------------------

{-|



-}

module Natlink.Grammar where

--------------------------------------------------

import Natlink.Grammar.Types

--------------------------------------------------

import Prelude_natlink

--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

--------------------------------------------------

primaryGrammarProperties :: GrammarProperties
primaryGrammarProperties      = GrammarProperties{..}
 where
 status               = Enabled
 exclusivity          = Inclusive
 shouldEavesdrop      = YesEavesdrop
 shouldHypothesize    = YesHypothesize

--------------------------------------------------

{-|

-}
narcissisticGrammarProperties :: GrammarProperties
narcissisticGrammarProperties = GrammarProperties{..}
 where
 status               = Enabled
 exclusivity          = Exclusive
 shouldEavesdrop      = YesEavesdrop
 shouldHypothesize    = YesHypothesize

--------------------------------------------------

{-|

-}
fastGrammarProperties :: GrammarProperties
fastGrammarProperties         = GrammarProperties{..}
 where
 status               = Enabled
 exclusivity          = Inclusive
 shouldEavesdrop      = NoEavesdrop
 shouldHypothesize    = NoHypothesize

--------------------------------------------------

-- | 'True' is "good", 'False' is "bad"
isSuccessfulGrammarLoadStatus :: GrammarLoadStatus -> Bool
isSuccessfulGrammarLoadStatus = \case
  GrammarLoadSuccess      -> True
  GrammarLoadBadGrammar   -> False
  GrammarLoadInvalidWord  -> False

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