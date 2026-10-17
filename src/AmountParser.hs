module AmountParser (parseAmount, parseAmountWithAssertion) where

import           Data.Text (Text)
import qualified Hledger as HL
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Strict
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser a = HL.JournalParser Identity a

parseAmount :: HL.Journal -> Text -> Either String HL.MixedAmount
parseAmount = runAmountParser mixed

-- | Parse an amount (possibly a sum) optionally followed by a balance
-- assertion, using the same syntax as journal files, e.g. @5 EUR = 100 EUR@.
parseAmountWithAssertion :: HL.Journal -> Text -> Either String (HL.MixedAmount, Maybe HL.BalanceAssertion)
parseAmountWithAssertion = runAmountParser amountWithAssertion

runAmountParser :: Parser a -> HL.Journal -> Text -> Either String a
runAmountParser p journal t =
  case runIdentity $ runParserT (evalStateT (p <* optional space <* eof) journal) "" t of
    Left err -> Left (errorBundlePretty err)
    Right res -> Right res

amountWithAssertion :: Parser (HL.MixedAmount, Maybe HL.BalanceAssertion)
amountWithAssertion = (,) <$> mixed <*> optional (try $ lexeme HL.balanceassertionp)

mixed :: Parser HL.MixedAmount
mixed = HL.mixed <$> expr

expr :: Parser [HL.Amount]
expr = some (try $ lexeme factor)

factor :: Parser HL.Amount
factor =  (char '+' >> lexeme HL.amountp)
      <|> (char '-' >> HL.divideAmount (-1) <$> lexeme HL.amountp)
      <|> HL.amountp

lexeme :: Parser a -> Parser a
lexeme p = space >> p
