--------------------------------------------------------------------------------
-- | Parser utilities
module Hakyll.Core.Util.Parser
    ( metadataKey
    , reservedKeys
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Monad       (mzero)
import qualified Text.Parsec         as P
import           Text.Parsec.String  (Parser)


--------------------------------------------------------------------------------
metadataKey :: Parser String
metadataKey = do
    i <- (:) <$> P.letter <*> (P.many $ P.alphaNum <|> P.oneOf "_." <|> notEndingMinus)
    if i `elem` reservedKeys then mzero else return i

notEndingMinus :: Parser Char
notEndingMinus = P.try $ do
    P.char '-'
    P.notFollowedBy (P.char '$')
    return '-'

--------------------------------------------------------------------------------
reservedKeys :: [String]
reservedKeys = ["if", "else", "endif", "for", "sep", "endfor", "partial"]
