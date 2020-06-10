{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
module Apie.Internal.User
    ( User(..)
    , authenticate
    , createUser
    , updateUser
    )
where

import RIO
import qualified RIO.Text as T
import RIO.Directory (doesFileExist)
import RIO.List (find)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)

data User a = User
  { email    :: !T.Text
  , password :: !a
  , info     :: !T.Text
  }
  deriving (Show, Functor)

data ValidationError
  = EmailAlreadyTaken
  | UserDoesNotExist
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
    users <- getUsers
    case findUser users u of
        Just user ->
            if validatePassword (encodeUtf8 p) (password user)
            then pure (Just (void user))
            else pure Nothing
        Nothing -> pure Nothing

createUser :: (MonadThrow m, MonadUnliftIO m)
           => User T.Text -> m (Either ValidationError (User ByteString))
createUser user = do
    -- TODO: lock passwd file
    users <- getUsers
    case findUser users (email user) of
        Just _ -> pure (Left EmailAlreadyTaken)
        Nothing -> do
            p <- hashPassword' (password user)
            let new = fmap (const p) user
            writeUsers (users <> [new])
            pure (Right new)

updateUser :: (MonadThrow m, MonadUnliftIO m)
           => User (Maybe T.Text) -> m (Either ValidationError (User ByteString))
updateUser newUser = do
    -- TODO: lock passwd file
    users <- getUsers
    case findUser users (email newUser) of
        Just oldUser -> do
            new <- case password newUser of
                Just p -> do
                    p' <- hashPassword' p
                    pure (fmap (const p') newUser)
                Nothing ->
                    pure (fmap (const (password oldUser)) newUser)
            let users' = updateUser' new <$> users
            writeUsers users'
            pure (Right new)
        Nothing -> pure (Left UserDoesNotExist)
  where
    updateUser' new u
      | email u == email new = new
      | otherwise = u

-- TODO: validate that email and info do not contain `sepChar`
-- validateUser :: User T.Text -> Either ValidationError (User T.Text)
-- validateUser = undefined

-- TODO: ensure chmod 700
getUsers :: (MonadThrow m, MonadUnliftIO m) => m [User ByteString]
getUsers = do
    exists <- doesFileExist passwdFile
    if exists
    then do
        contents <- readFileUtf8 passwdFile
        let ls = filter (not . T.null) . fmap T.strip $ T.lines contents
        let result = sequence (decodeUser <$> ls)
        case result of
            Just users -> pure users
            Nothing -> throwM PasswdFileCorrupted
    else fail "No passwd file found."
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
findUser users e = find emailMatches users
  where
    emailMatches x = email x == e

hashPassword' :: MonadUnliftIO m => Text -> m ByteString
hashPassword' p = liftIO $ hashPassword cost (encodeUtf8 p)
  where cost = 5

passwdFile :: FilePath
passwdFile = "passwd"

sepChar :: Char
sepChar = ':'
