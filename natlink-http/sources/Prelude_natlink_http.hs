----------------------------------------

module Prelude_natlink_http
 ( module X
 ) where

----------------------------------------

import "spiros" Prelude.Spiros as X hiding (Text)

----------------------------------------

import "text" Data.Text as X (Text)

----------------------------------------

import "generic-lens" Data.Generics.Product as X hiding (IsList,list)
import "generic-lens" Data.Generics.Sum     as X

----------------------------------------