module Data.MsgPack 
    ( module Data.MsgPack
    , module X
    ) where

import Prelude

import Control.Monad.Except (Except, throwError)
import Control.Monad.Except (Except, throwError) as X
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.Types (ArrayBuffer) as X
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors)
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors) as X
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List.NonEmpty as NEL

-- | Encode a `Foreign` value into an `ArrayBuffer` using the MsgPack protocol.
foreign import encode :: Foreign -> ArrayBuffer 

-- | Decodes an `ArrayBuffer` into a `Foreign` value. Uses the `F` error monad
-- | from `purescript-foreign` to make conversions easier.
decode :: ArrayBuffer -> F Foreign
decode = runFn3 decodeImpl (throwError <<< NEL.singleton <<< ForeignError) pure

foreign import decodeImpl 
    :: Fn3 
        (forall a. String -> Except MultipleErrors a) 
        (forall a b. b -> Except a b) 
        ArrayBuffer 
        (F Foreign)
