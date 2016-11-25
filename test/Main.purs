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
            quickCheck \ (TestVal obj) ->
                encodeDecode (TestVal obj)

    describe "strings" do
        it "uhhh what" do
            quickCheck \ str -> 
                encodeDecode (str :: String)

    describe "copying specific strings" do
        it "ex1" do
            "衴᷃o䡔�ᐨ" `shouldEqual` "衴᷃o䡔�ᐨ"
        it "ex2" do
            "蕖寗䆿�ﶻ訶" `shouldEqual` "蕖寗䆿�ﶻ訶"
        it "ex3" do
            "멜⎬�阬闝" `shouldEqual` "멜⎬�阬闝"
        it "ex4" do
            "몝㹦�" `shouldEqual` "몝㹦�"
        it "ex5" do
            "ᠫ�" `shouldEqual` "ᠫ�"
        it "ex6" do
            "㐠姯䐸쪩倏쮇畓�௡⫋" `shouldEqual` "㐠姯䐸쪩倏쮇畓�௡⫋"
        it "ex7" do
            "埘ἢ螳�橠" `shouldEqual` "埘ἢ螳�橠"
        it "ex8" do
            "鍔͘嫥�" `shouldEqual` "鍔͘嫥�"
        it "ex9" do
            "⩡崒긭靗�" `shouldEqual` "⩡崒긭靗�"
        it "ex10" do
            "장轇⼧㌫�뎋" `shouldEqual` "장轇⼧㌫�뎋"
        it "ex11" do
            "�☔췔直嵹ᔹ࢑觲" `shouldEqual` "�☔췔直嵹ᔹ࢑觲"
        it "ex12" do
            "뮂쌲�" `shouldEqual` "뮂쌲�"
        it "ex13" do
            "┎潵菩�㸎�㭾傓⳿驲" `shouldEqual` "┎潵菩�㸎�㭾傓⳿驲"

encodeDecode val =
    runExcept (read =<< decode (encode (toForeign val)))
        ===
            (runExcept (pure val))

encodeToStr :: forall a. a -> ArrayBuffer
encodeToStr val = encode (toForeign val)

newtype TestVal = TestVal { foo :: String, bar :: Int, baz :: Array Number }

derive instance genericTestVal :: Generic TestVal

instance eqTestVal :: Eq TestVal where eq = gEq
instance showTestVal :: Show TestVal where show = gShow

instance isForeignTestVal :: IsForeign TestVal where
    read obj = do
        foo <- readProp "foo" obj
        bar <- readProp "bar" obj
        baz <- readProp "baz" obj
        pure (TestVal {foo, bar, baz})

instance arbTestVal :: Arbitrary TestVal where
    arbitrary = do
        foo <- arbitrary
        bar <- arbitrary
        baz <- arbitrary
        pure (TestVal {foo, bar, baz})
