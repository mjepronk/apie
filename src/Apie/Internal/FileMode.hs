module Apie.Internal.FileMode
    ( toSymbolicRep
    , fromSymbolicRep
    , parseFileMode
    )
where

import RIO
import qualified RIO.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (zip3)
import System.Posix.Types (FileMode)
import qualified System.Posix.Files as F

{- | Return the symbolic representation of a file mode.

> toSymbolicRep nullFileMode == "---------"
> toSymbolicRep stdFileMode == "rw-rw-rw-"
> toSymbolicRep accessModes == "rwxrwxrwx"

-}
toSymbolicRep :: FileMode -> String
toSymbolicRep mode = zipWith f strModes allModes
  where
    f c m
      | hasMode m mode = c
      | otherwise = '-'

hasMode :: FileMode -> FileMode -> Bool
hasMode a b = F.intersectFileModes a b == a

{- | Parse a symbolic representation of a file mode.

> fromSymbolicRep "---------" == Just nullFileMode
> fromSymbolicRep "rw-rw-rw-" == Just stdFileMode
> fromSymbolicRep "rwxrwxrwx" == Just accessModes

-}
fromSymbolicRep :: String -> Maybe FileMode
fromSymbolicRep str
    | length str == length strModes = foldM f F.nullFileMode xs
    | otherwise = Nothing
  where
    f acc x = F.unionFileModes acc <$> uncurry3 parseMode x
    xs = zip3 allModes strModes str

-- | Aeson Parser for symbolic representation of a file mode.
parseFileMode :: A.Value -> A.Parser FileMode
parseFileMode =
  A.withText "FileMode" $ \text ->
    case fromSymbolicRep (T.unpack text) of
      Just mode -> pure mode
      Nothing -> fail ("Invalid FileMode specified: " <> T.unpack text)

parseMode :: FileMode -> Char -> Char -> Maybe FileMode
parseMode m exp' c
    | c == exp' = Just m
    | c == '-'  = Just F.nullFileMode
    | otherwise = Nothing

strModes :: String
strModes = "rwxrwxrwx"

allModes, ownerModes, groupModes, otherModes :: [FileMode]
allModes = ownerModes <> groupModes <> otherModes
ownerModes = [F.ownerReadMode, F.ownerWriteMode, F.ownerExecuteMode]
groupModes = [F.groupReadMode, F.groupWriteMode, F.groupExecuteMode]
otherModes = [F.otherReadMode, F.otherWriteMode, F.otherExecuteMode]

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
