{-# LANGUAGE OverloadedStrings #-}

module AmountParserSpec (spec) where

import           Test.Hspec

import           Control.Monad.Trans.State.Strict
import           Data.Either (isLeft)
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Hledger as HL
import           Text.Megaparsec

import           AmountParser

spec :: Spec
spec = do
  describe "parseAmount" parseAmountSpec
  describe "parseAmountWithAssertion" parseAmountWithAssertionSpec

parseAmountSpec :: Spec
parseAmountSpec = do
  it "parses single amount" $
    parseAmount HL.nulljournal "42" `shouldBe` Right (amount "42")

  it "parses a positive number" $
    parseAmount HL.nulljournal "+42" `shouldBe` Right (amount "42")

  it "parses a negative number" $
    parseAmount HL.nulljournal "-42" `shouldBe` Right (amount "-42")

  it "parses a simple sum" $
    parseAmount HL.nulljournal "23 + 23 + 42 + 1" `shouldBe` Right (amount "89")

  it "parses a sum with negative values" $
    parseAmount HL.nulljournal "-42 + 23 + 42 - 23 + 1" `shouldBe` Right (amount "1")

  it "fails to parse a trailing plus" $
    parseAmount HL.nulljournal "23 +" `shouldSatisfy` isLeft

  it "rejects a balance assertion" $
    parseAmount HL.nulljournal "5 = 10" `shouldSatisfy` isLeft

parseAmountWithAssertionSpec :: Spec
parseAmountWithAssertionSpec = do
  it "parses an amount without assertion" $
    assertion "23 + 19" `shouldBe` Right (amount "42", Nothing)

  it "parses a sum followed by an assertion" $
    assertion "23 + 19 = 100" `shouldBe` Right (amount "42", Just (False, False))

  it "parses total and inclusive assertions" $ do
    fmap snd (assertion "1 ==100") `shouldBe` Right (Just (True, False))
    fmap snd (assertion "1 =* 100") `shouldBe` Right (Just (False, True))
    fmap snd (assertion "1 ==* 100") `shouldBe` Right (Just (True, True))

  it "fails on an assertion without amount" $
    assertion "1 =" `shouldSatisfy` isLeft

  where
    -- Keep only the amount and the (total, inclusive) flags of the assertion
    assertion t = fmap (fmap (\a -> (HL.batotal a, HL.bainclusive a))) <$> parseAmountWithAssertion HL.nulljournal t

amount :: Text -> HL.MixedAmount
amount = HL.mixed . Just . fromRight . runIdentity . runParserT (evalStateT HL.amountp HL.nulljournal) ""

fromRight :: Either a b -> b
fromRight = either (error "fromRight: Left value encountered") id
