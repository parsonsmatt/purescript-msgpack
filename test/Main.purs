module Test.Main where

import Prelude

import Control.Monad.Except
import Control.Monad.Eff          
import Control.Monad.Aff          
import Test.Spec                  
import Test.Spec.Runner           
import Test.Spec.Assertions       
import Test.Spec.Reporter.Console (consoleReporter)
import Test.QuickCheck 
import Test.Spec.QuickCheck (quickCheck)
import Data.Foreign
import Data.Generic
import Data.Foreign.Class
import Global.Unsafe

import Data.MsgPack

main :: Eff _ Unit
main = run [consoleReporter] do
    describe "Data.MsgPack" do
        it "that galois connection tho" do
            quickCheck \ (TestVal obj) -> do
                let value = TestVal obj
                (runExcept $ read =<< decode (encode (toForeign value)))
                    ===
                    (runExcept $ pure value)


newtype TestVal = TestVal { foo :: String, bar :: Int }

derive instance genericTestVal :: Generic TestVal

instance eqTestVal :: Eq TestVal where eq = gEq
instance showTestVal :: Show TestVal where show = gShow

instance isForeignTestVal :: IsForeign TestVal where
    read obj = do
        foo <- readProp "foo" obj
        bar <- readProp "bar" obj
        pure (TestVal {foo, bar})

instance arbTestVal :: Arbitrary TestVal where
    arbitrary = do
        foo <- arbitrary
        bar <- arbitrary
        pure (TestVal {foo, bar})
