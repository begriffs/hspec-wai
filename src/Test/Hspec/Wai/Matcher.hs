{-# LANGUAGE CPP, TypeFamilies #-}
module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, match
) where


import           Control.Monad
import           Data.Monoid
import           Data.Functor
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Util

#if MIN_VERSION_base(4,4,0)
import           GHC.Exts

instance IsList MatchHeaders where
  type Item MatchHeaders = Header
  fromList xs = case xs of
    [] -> AnyHeaders
    _ -> foldr1 (:&&:) (map HasHeader xs)
  toList = error "Test.Hspec.Wai.Matcher.toList: toList is not supported fo MatchHeaders"
#endif

data ResponseMatcher = ResponseMatcher {
  matchStatus :: Int
, matchHeaders :: MatchHeaders
, matchBody :: Maybe LB.ByteString
}

data MatchHeaders = AnyHeaders | HasHeader Header | MatchHeaders :&&: MatchHeaders | MatchHeaders :||: MatchHeaders

instance IsString ResponseMatcher where
  fromString s = ResponseMatcher 200 AnyHeaders (Just . encodeUtf8 . fromString $ s)

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher (fromInteger n) AnyHeaders Nothing
  (+) =    error "ResponseMatcher does not support (+)"
  (-) =    error "ResponseMatcher does not support (-)"
  (*) =    error "ResponseMatcher does not support (*)"
  abs =    error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

match :: SResponse -> ResponseMatcher -> Maybe String
match (SResponse (Status status _) headers body) (ResponseMatcher expectedStatus expectedHeaders expectedBody) = mconcat [
    actualExpected "status mismatch:" (show status) (show expectedStatus) <$ guard (status /= expectedStatus)
  , checkHeaders headers expectedHeaders
  , expectedBody >>= matchBody_ body
  ]
  where
    matchBody_ actual expected = actualExpected "body mismatch:" actual_ expected_ <$ guard (actual /= expected)
      where
        (actual_, expected_) = case (safeToString $ LB.toStrict actual, safeToString $ LB.toStrict expected) of
          (Just x, Just y) -> (x, y)
          _ -> (show actual, show expected)

    actualExpected :: String -> String -> String -> String
    actualExpected message actual expected = unlines [
        message
      , "  expected: " ++ expected
      , "  but got:  " ++ actual
      ]

checkHeaders :: [Header] -> MatchHeaders -> Maybe String
checkHeaders actual expected = undefined {- case filter (`notElem` actual) expected of
  [] -> Nothing
  missing ->
    let msg
          | length missing == 1 = "missing header:"
          | otherwise = "missing headers:"
    in Just $ unlines (msg : formatHeaders missing ++ "the actual headers were:" : formatHeaders actual)
  where
    formatHeaders = map (("  " ++) . formatHeader)
    -}
