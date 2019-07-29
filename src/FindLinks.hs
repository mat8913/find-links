{-# LANGUAGE FlexibleContexts #-}

module FindLinks (findLinks) where

import Text.Megaparsec
import Control.Monad
import Data.Bifunctor (first)
import Data.Void
import Control.Exception (throw)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Text.URI as URI


search :: MonadPlus m => m step -> m end -> m ([step], end)
search step end = first ($ []) <$> go id
  where
    go f = do
      done <- (Just <$> end) `mplus` pure Nothing
      case done of
        Just result -> pure (f, result)
        Nothing -> do
          x <- step
          go (f . (x:))

uriParser :: MonadParsec e Text m => m URI.URI
uriParser = do
    uri <- URI.parser
    case URI.uriScheme uri of
        Nothing -> failure Nothing mempty
        _ -> pure ()
    case URI.uriAuthority uri of
        Left _ -> failure Nothing mempty
        _ -> pure ()
    pure $ uri

theParser :: MonadParsec e Text m => m ([(String, URI.URI)], String)
theParser = (,)
    <$> (many $ try $ search anySingle $ try uriParser)
    <*> fmap T.unpack takeRest

findLinks :: String -> [Either String URI.URI]
findLinks str = case parse theParser "" (T.pack str) of
    Right (x, rest) -> collapse x ++ [Left rest]
    Left e -> throw (e :: ParseErrorBundle Text Void)

collapse :: [(a, b)] -> [Either a b]
collapse [] = []
collapse ((x, y):xs) = Left x : Right y : collapse xs
