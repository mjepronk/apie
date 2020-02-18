{-# LANGUAGE RecordWildCards #-}
module Apie.Internal.User
    ( User(..)
    , authenticate
    , createUser
    )
where

import RIO
import qualified RIO.Text as T
import RIO.Directory (doesFileExist)
import RIO.List (headMaybe)
import RIO.ByteString (ByteString)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)

data User a = User
  { email    :: !T.Text
  , password :: !a
  , info     :: !T.Text
  }
  deriving Show

data ValidationError
  = EmailAlreadyTaken
  | InvalidInfo
  | InvalidPassword
  deriving Show

data UserException
  = PasswdFileCorrupted
  | PasswdIncorrectPermissions
  deriving Show

instance Exception UserException

authenticate :: (MonadThrow m, MonadUnliftIO m)
             => T.Text -> T.Text -> m (Maybe (User ()))
authenticate u p = do
    users <- listUsers
    case findUser users u of
        Just user ->
            if validatePassword (encodeUtf8 p) (password user)
            then pure (Just (user { password=() }))
            else pure Nothing
        Nothing -> pure Nothing

createUser :: (MonadThrow m, MonadUnliftIO m)
           => User T.Text -> m (Either ValidationError (User ByteString))
createUser user = do
    -- TODO: lock passwd file
    users <- listUsers
    case findUser users (email user) of
        Just _ -> pure (Left EmailAlreadyTaken)
        Nothing -> do
            p <- liftIO $ hashPassword cost (encodeUtf8 (password user))
            let new = User
                    { email = email user
                    , password = p
                    , info = info user
                    }
            writeUsers (users <> [new])
            pure (Right new)
  where
    -- TODO: validate that email and info do not contain `sepChar`
    -- validateUser :: User T.Text -> Either ValidationError (User T.Text)
    -- validateUser = undefined

    cost = 5

-- TODO: ensure chmod 700
listUsers :: (MonadThrow m, MonadUnliftIO m) => m [User ByteString]
listUsers = do
    exists <- doesFileExist passwdFile
    if exists
    then do
        contents <- readFileUtf8 passwdFile
        let ls = filter (not . T.null) . fmap T.strip $ T.lines contents
        let result = sequence (decodeUser <$> ls)
        case result of
            Just users -> pure users
            Nothing -> throwM PasswdFileCorrupted
    else pure []
  where
    decodeUser :: T.Text -> Maybe (User ByteString)
    decodeUser line =
        case T.split (== sepChar) line of
            [email, p, info] ->
                let password = encodeUtf8 p
                in  pure (User {..})
            _ -> Nothing

-- TODO: ensure chmod 700
writeUsers :: MonadUnliftIO m => [User ByteString] -> m ()
writeUsers users = do
    let contents = T.unlines (encodeUser <$> users)
    writeFileUtf8 passwdFile contents
  where
    encodeUser :: User ByteString -> T.Text
    encodeUser User {..} =
        T.intercalate (T.singleton sepChar) [email, decodeUtf8Lenient password, info]

findUser :: [User a] -> T.Text -> Maybe (User a)
findUser users e = headMaybe (filter emailMatches users)
  where
    emailMatches x = email x == e

passwdFile :: FilePath
passwdFile = "passwd"

sepChar :: Char
sepChar = ':'
