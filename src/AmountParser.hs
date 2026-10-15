module AmountParser (parseAmount) where

import           Control.Monad (void, when)
import           Data.Char (isDigit)
import           Data.Decimal (decimalPlaces, normalizeDecimal)
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Hledger as HL
import           Control.Monad.Trans.State.Strict
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser a = HL.JournalParser Identity a

-- | Intermediate result of an arithmetic expression.
--
-- 'valScaled' records whether a multiplication or division was involved. In
-- that case, the result may have (practically) infinite precision and has to be
-- rounded before it can be used.
data Value = Value
  { valAmount :: HL.MixedAmount
  , valScaled :: Bool
  }

-- | Parse an arithmetic expression of amounts.
--
-- Supported are @+@, @-@, @*@, @/@ (with the usual precedence) and
-- parentheses. Multiplication and division require one side (the right side
-- for division) to be a plain number without commodity.
parseAmount :: HL.Journal -> Text -> Either String HL.MixedAmount
parseAmount journal t = case runIdentity $ runParserT (evalStateT (space *> expr <* eof) journal) "" t of
  Left err -> Left (errorBundlePretty err)
  Right res -> Right (roundValue (HL.journalCommodityStyles journal) res)

expr :: Parser Value
expr = term >>= rest
  where rest acc = (do op <- addOp
                       t <- term
                       rest (op acc t))
                   <|> pure acc

term :: Parser Value
term = factor >>= rest
  where rest acc = (do op <- mulOp
                       f <- factor
                       op acc f >>= rest)
                   <|> pure acc

factor :: Parser Value
factor =  (symbol '-' *> (negateValue <$> factor))
      <|> (symbol '+' *> factor)
      <|> between (symbol '(') (symbol ')') expr
      <|> operand

-- | A single hledger amount like @42@, @5 EUR@ or @$-3.50@.
--
-- 'HL.amountp' happily accepts characters like @/@ or @(@ as commodity
-- symbols, so we first cut the input at the next operator and then let
-- 'HL.amountp' parse only that part.
operand :: Parser Value
operand = do
  input <- getInput
  let (amountText, rest) = T.splitAt (operandLength (T.unpack input)) input
  when (T.null (T.strip amountText)) $
    void (satisfy (const False) <?> "amount")
  setInput amountText
  amount <- HL.amountp <* space <* eof
  setInput rest
  return $ Value (HL.mixedAmount amount) False

-- | Number of characters at the beginning of the input that belong to the
-- next amount.
--
-- The amount ends at the first operator or parenthesis outside of a quoted
-- commodity symbol. Signs are only treated as operators after the first digit
-- (to allow @$-5@) and if they are not part of an exponent (like in @1E-3@).
operandLength :: String -> Int
operandLength = go 0 False ' ' ' '
  where
    -- n: consumed characters, digit: seen a digit yet,
    -- p1, p2: the last two consumed characters
    go :: Int -> Bool -> Char -> Char -> String -> Int
    go n _ _ _ [] = n
    go n digit p1 _ ('"':cs) =
      let (quoted, rest) = break (== '"') cs
          n' = n + 1 + length quoted
      in case rest of
           [] -> n'
           (_:rest') -> go (n' + 1) digit '"' p1 rest'
    go n digit p1 p2 (c:cs)
      | c `elem` ("*/()" :: String) = n
      | c `elem` ("+-" :: String) && not signAllowed = n
      | otherwise = go (n + 1) (digit || isDigit c) c p1 cs
      where signAllowed = not digit || (p1 `elem` ("eE" :: String) && isDigit p2)

addOp :: Parser (Value -> Value -> Value)
addOp =  (symbol '+' >> return (combine HL.maPlus))
     <|> (symbol '-' >> return (combine HL.maMinus))
  where combine f (Value a sa) (Value b sb) = Value (f a b) (sa || sb)

mulOp :: Parser (Value -> Value -> Parser Value)
mulOp =  (symbol '*' >> return multiply)
     <|> (symbol '/' >> return divide)

multiply :: Value -> Value -> Parser Value
multiply l r = case (plainNumber r, plainNumber l) of
  (Just q, _) -> return $ Value (HL.multiplyMixedAmount q (valAmount l)) True
  (_, Just q) -> return $ Value (HL.multiplyMixedAmount q (valAmount r)) True
  _ -> fail "Can only multiply by a plain number without commodity"

divide :: Value -> Value -> Parser Value
divide l r = case plainNumber r of
  Just 0 -> fail "Division by zero"
  Just q -> return $ Value (HL.divideMixedAmount q (valAmount l)) True
  Nothing -> fail "Can only divide by a plain number without commodity"

negateValue :: Value -> Value
negateValue v = v { valAmount = HL.maNegate (valAmount v) }

-- | The quantity of a value, if it is a single amount without commodity and
-- cost.
plainNumber :: Value -> Maybe HL.Quantity
plainNumber v = case HL.amountsRaw (valAmount v) of
  [a] | T.null (HL.acommodity a) && isNothing (HL.acost a) -> Just (HL.aquantity a)
  _ -> Nothing

-- | Round the result of multiplications and divisions.
--
-- Amounts are rounded to the precision of their commodity in the journal. If
-- the journal doesn't define one, the precision of the entered amount is
-- used, extended to up to two decimal places if the result needs them.
roundValue :: M.Map HL.CommoditySymbol HL.AmountStyle -> Value -> HL.MixedAmount
roundValue styles v
  | valScaled v = HL.mapMixedAmount roundAmount (valAmount v)
  | otherwise = valAmount v
  where
    roundAmount a = HL.setAmountInternalPrecision (precision a) a
    precision a = case HL.asprecision <$> M.lookup (HL.acommodity a) styles of
      Just (HL.Precision p) -> p
      _ -> max (enteredPrecision a) (min 2 (decimalPlaces (normalizeDecimal (HL.aquantity a))))
    enteredPrecision a = case HL.asprecision (HL.astyle a) of
      HL.Precision p -> p
      HL.NaturalPrecision -> 0

symbol :: Char -> Parser ()
symbol c = void (char c) <* space
