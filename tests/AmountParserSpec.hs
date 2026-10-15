{-# LANGUAGE OverloadedStrings #-}

module AmountParserSpec (spec) where

import           Test.Hspec

import           Control.Monad.Trans.State.Strict
import           Data.Either (isLeft)
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Word (Word8)
import qualified Hledger as HL
import qualified Hledger.Read.JournalReader as JR
import           Text.Megaparsec

import           AmountParser

spec :: Spec
spec = describe "parseAmount" $ do
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

  it "parses amounts with digit groups" $
    parseAmount HL.nulljournal "1 000 + 1" `shouldBe` Right (amount "1001")

  it "parses amounts with commodities" $
    parseAmount HL.nulljournal "10 EUR + 5 EUR" `shouldBe` Right (amount "15 EUR")

  it "parses a negative amount with left-side commodity" $
    parseAmount HL.nulljournal "$-10 + $3" `shouldBe` Right (amount "$-7")

  it "parses amounts with exponent" $
    parseAmount HL.nulljournal "1E-3 + 1" `shouldBe` Right (amount "1.001")

  it "parses a quoted commodity containing operators" $
    parseAmount HL.nulljournal "3 \"a/b\" * 2" `shouldBe` Right (amount "6 \"a/b\"")

  it "parses a product" $
    parseAmount HL.nulljournal "2 * 3" `shouldBe` Right (amount "6")

  it "parses a quotient" $
    parseAmount HL.nulljournal "10 / 4" `shouldBe` Right (amount "2.5")

  it "parses a quotient without spaces" $
    parseAmount HL.nulljournal "10/2" `shouldBe` Right (amount "5")

  it "gives multiplication precedence over addition" $
    parseAmount HL.nulljournal "1 + 2 * 3" `shouldBe` Right (amount "7")

  it "parses parentheses" $
    parseAmount HL.nulljournal "(1 + 2) * 3" `shouldBe` Right (amount "9")

  it "parses nested parentheses" $
    parseAmount HL.nulljournal "((1 + 2) * (3 - 1)) / 2" `shouldBe` Right (amount "3")

  it "parses a negated product" $
    parseAmount HL.nulljournal "-2 * 3" `shouldBe` Right (amount "-6")

  it "parses subtraction left-associatively" $
    parseAmount HL.nulljournal "10 - 4 - 3" `shouldBe` Right (amount "3")

  it "parses division left-associatively" $
    parseAmount HL.nulljournal "24 / 4 / 2" `shouldBe` Right (amount "3")

  it "multiplies an amount with commodity" $ do
    parseAmount HL.nulljournal "10 EUR * 3" `shouldBe` Right (amount "30 EUR")
    parseAmount HL.nulljournal "3 * 10 EUR" `shouldBe` Right (amount "30 EUR")
    parseAmount HL.nulljournal "$10*3" `shouldBe` Right (amount "$30")

  it "divides an amount with commodity" $
    parseAmount HL.nulljournal "10 EUR / 4" `shouldBe` Right (amount "2.5 EUR")

  it "rounds non-terminating results to two decimal places" $
    parseAmount HL.nulljournal "10 / 3" `shouldBe` Right (amount "3.33")

  it "keeps the entered precision" $
    parseAmount HL.nulljournal "1.005 * 3" `shouldBe` Right (amount "3.015")

  it "only rounds the final result" $
    parseAmount HL.nulljournal "1/3 + 1/3 + 1/3" `shouldBe` Right (amount "1.00")

  it "rounds to the precision of the commodity inferred from the journal" $ do
    let journal = journalWithStyle "EUR" 2
    parseAmount journal "100 EUR / 3" `shouldBe` Right (amount "33.33 EUR")
    parseAmount journal "10 EUR * 1.2346" `shouldBe` Right (amount "12.35 EUR")

  it "applies the commodity style declared in the journal" $ do
    journal <- journalFromText "commodity 1.000,00 EUR\ncommodity $1,000.0000\n"
    showResult (parseAmount journal "100 EUR / 3") `shouldBe` Right "33,33 EUR"
    showResult (parseAmount journal "10000 EUR * 1.2346") `shouldBe` Right "12.346,00 EUR"
    showResult (parseAmount journal "$10 / 3") `shouldBe` Right "$3.3333"
    showResult (parseAmount journal "10 USD / 4") `shouldBe` Right "2.5 USD"

  it "fails to multiply two amounts with commodity" $
    parseAmount HL.nulljournal "10 EUR * 2 EUR" `shouldSatisfy` isLeft

  it "fails to divide by an amount with commodity" $ do
    parseAmount HL.nulljournal "2 / 10 EUR" `shouldSatisfy` isLeft
    parseAmount HL.nulljournal "10 EUR / 2 EUR" `shouldSatisfy` isLeft

  it "fails to divide by zero" $
    parseAmount HL.nulljournal "10 / (2 - 2)" `shouldSatisfy` isLeft

  it "fails to parse incomplete expressions" $ do
    parseAmount HL.nulljournal "2 *" `shouldSatisfy` isLeft
    parseAmount HL.nulljournal "* 2" `shouldSatisfy` isLeft
    parseAmount HL.nulljournal "2 * * 3" `shouldSatisfy` isLeft
    parseAmount HL.nulljournal "(1 + 2" `shouldSatisfy` isLeft
    parseAmount HL.nulljournal "1 + 2)" `shouldSatisfy` isLeft

amount :: Text -> HL.MixedAmount
amount = HL.mixed . Just . fromRight . runIdentity . runParserT (evalStateT HL.amountp HL.nulljournal) ""

journalWithStyle :: HL.CommoditySymbol -> Word8 -> HL.Journal
journalWithStyle c p = HL.nulljournal
  { HL.jinferredcommoditystyles = M.singleton c HL.amountstyle
      { HL.ascommodityside = HL.R, HL.ascommodityspaced = True, HL.asprecision = HL.Precision p } }

journalFromText :: Text -> IO HL.Journal
journalFromText t = do
  res <- HL.runExceptT $ HL.parseAndFinaliseJournal (JR.journalp HL.definputopts) HL.definputopts "" t
  either fail return res

showResult :: Either String HL.MixedAmount -> Either String String
showResult = fmap HL.showMixedAmount

fromRight :: Either a b -> b
fromRight = either (error "fromRight: Left value encountered") id
