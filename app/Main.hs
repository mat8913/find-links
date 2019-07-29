module Main where

import FindLinks
import qualified Html as H
import qualified Html.Attribute as A
import qualified Text.URI as URI
import qualified Data.ByteString.Lazy as B


main :: IO ()
main = do
    withLinks <- findLinks <$> getContents
    let doc = fmap (fmap encodeUri) withLinks
    B.putStr $ H.renderByteString doc

encodeUri :: URI.URI -> _
encodeUri uri = H.a_A (A.href_ uri') uri'
  where
    uri' = URI.render uri
