{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Data.Hjq.Parser

main :: IO ()
main = hspec $ do
    describe "Data.Hjq.Parser.JqFilter" $
        it "read . show == id" $
            forAll jqFilterGen $ \jf ->
                read (show jf) == (jf :: JqFilter)
    describe "Data.Hjq.Parser.parseJqFilter" $ do
        it (show t1) $
            parseJqFilter t1 `shouldBe` Right JqNil
        it (show t2) $
            parseJqFilter t2 `shouldBe` Right (JqIndex 0 JqNil)
        it (show t3) $
            parseJqFilter t3 `shouldBe` Right (JqField "fieldName" JqNil)
        it (show t4) $
            parseJqFilter t4 `shouldBe`
                Right (JqIndex 0 (JqField "fieldName" JqNil))
        it (show t5) $
            parseJqFilter t5 `shouldBe`
                Right (JqField "fieldName" (JqIndex 0 JqNil))
      where
        t1 = "."
        t2 = ".[0]"
        t3 = ".fieldName"
        t4 = ".[0].fieldName"
        t5 = ".fieldName[0]"

jqFilterGen = oneof [ JqField <$> textGen <*> jqFilterGen
                    , JqIndex <$> (fmap abs arbitrary) <*> jqFilterGen
                    , pure JqNil
                    ]

textGen = fmap Text.pack $ listOf $ elements ['a'..'z']
