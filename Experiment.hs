{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module Experiment where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Text

import           Ldap.Client

data LdapEffect r where
  SearchLdap        :: Dn -> Mod Search -> Filter -> [Attr] -> LdapEffect [SearchEntry]
  ModifyLdap        :: Dn -> [Operation]-> LdapEffect ()

searchLdap' :: Member LdapEffect effs => Dn -> Mod Search -> Filter -> [Attr] -> Eff effs [SearchEntry]
searchLdap' d m f a = send $ SearchLdap d m f a

modifyLdap' :: Member LdapEffect effs => Dn -> [Operation] -> Eff effs ()
modifyLdap' d o = send $ ModifyLdap d o



data FacebookEffect r where
  GetUserEmail    :: Text -> FacebookEffect (Either Int Text)
  SendHelp        :: FacebookEffect ()
  PostMessage     :: Text -> FacebookEffect ()

getUserEmail' :: Member FacebookEffect effs => Text -> Eff effs (Either Int Text)
getUserEmail' e = send $ GetUserEmail e

postMessage' :: Member FacebookEffect effs => Text -> Eff effs ()
postMessage' m = send $ PostMessage m

sendHelp' :: Member FacebookEffect effs => Eff effs ()
sendHelp' = send $ SendHelp



(<?>) :: Member (Error e) effs => Either a1 a2 -> e -> Eff effs a2
ei <?> message = do
  case ei of
    Left _  -> throwError message
    Right v -> return v

program :: (Member LdapEffect effs, Member (Error String) effs, Member FacebookEffect effs) => Eff effs ()
program = do
  emailEither <- getUserEmail' "username"
  email <- emailEither <?> "errror"

  entries <- searchLdap' (Dn email) undefined undefined undefined

  modifyLdap' (Dn "") undefined

  postMessage' $ "Welcome"

-- runLdap :: Eff '[LdapEffect, IO] a -> IO a
-- runLdap = runM . interpretM (\case
--   SearchLdap d m f a -> return []
--   ModifyLdap d o -> return ())

runLdap :: LastMember IO effs => Eff (LdapEffect ': effs) ~> Eff effs
runLdap = interpretM (\case
  SearchLdap d m f a -> return []
  ModifyLdap d o -> putStrLn "")

runFacebook :: LastMember IO effs => Eff (FacebookEffect ': effs) ~> Eff effs
runFacebook = interpret (\case
  GetUserEmail msg -> return $ Right "email"
  PostMessage message -> return ()
  SendHelp -> return ())

-- r :: Eff '[Error e, FacebookEffect, LdapEffect] a -> IO (Either e a)
r :: LastMember IO effs => Eff (FacebookEffect : LdapEffect : effs) ~> Eff effs
r p = runLdap $ runFacebook p

f :: Eff '[IO] a -> IO a
f p = runM p

-- g :: (Member LdapEffect effs, Member (Error String) effs, Member FacebookEffect effs, Member IO effs) => Eff effs a -> Eff '[] (Either String a)
g :: Eff '[Error String, LdapEffect, FacebookEffect, IO] a -> IO (Either String a)
g = runM . runFacebook . runLdap . runError


main :: IO ()
main = do
  a <- g program
  putStrLn $ show a

-- runFacebook :: Eff '[FacebookEffect, IO] a -> IO a
-- runFacebook = runM . interpretM (\case
--   GetUserEmail msg -> return $ Right "email"
--   PostMessage message -> return ()
--   SendHelp -> return ())



-- -- import           Control.Monad.Except       ( ExceptT, MonadError, runExceptT, throwError )
-- -- import           Control.Monad.Reader
-- -- import           Control.Monad.Trans.Class
-- -- import           Control.Monad.Trans.Except
-- -- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Freer.Error
-- import           Control.Monad.Freer.State
-- import           Control.Monad.Freer.Writer
-- -- import           Control.Monad.Trans.Writer
-- -- import           Control.Monad.Writer.Class
-- -- -- import           Control.Monad.Trans.Writer.CPS
-- -- import           Data.Functor.Identity
-- -- import           Data.Text
-- -- import           Env
-- -- -- import           Ldap.Client                hiding ( modify )
-- -- import           Data.List                  ( delete )
-- -- import           Prelude
-- -- import           Server.LDAP
-- -- import qualified System.IO                  as S
-- -- import           System.IO.Unsafe
-- import           Data.List
-- import           Test.Hspec                 ( Spec, context, describe, it, shouldBe, shouldReturn )

-- import           Control.Monad.Freer
-- import           Data.Function
-- import           Data.Text

-- -- mkFixture "Fixture" [ts| MonadLdap |]


-- data FileSystem r where
--   ReadFile :: FilePath -> FileSystem String
--   WriteFile :: FilePath -> String -> FileSystem ()

-- readFile1 :: Member FileSystem effs => FilePath -> Eff effs String
-- readFile1 path = send (ReadFile path)

-- writeFile1 :: Member FileSystem effs => FilePath -> String -> Eff effs ()
-- writeFile1 path contents = send (WriteFile path contents)


-- runFileSystemIO :: LastMember IO r => Eff (FileSystem ': r) ~> Eff r
-- runFileSystemIO = interpretM $ \case
--   ReadFile a -> do
--     -- tell
--     Prelude.readFile a
--   WriteFile a b -> Prelude.writeFile a b

-- data Config = Config String

-- runFileSystemIOWithConfig :: LastMember IO r => Config -> Eff (FileSystem ': r) ~> Eff r
-- runFileSystemIOWithConfig (Config contents) = interpretM $ \case
--   ReadFile a -> Prelude.readFile contents
--   WriteFile a b -> Prelude.writeFile a b

-- runFileSystemPredefined :: Eff (FileSystem ': r) ~> Eff r
-- runFileSystemPredefined = interpret $ \case
--   ReadFile a -> do
--     -- tell ""
--     return "1111"
--   WriteFile a b -> return ()

-- runInMemoryFileSystem :: [(FilePath, String)] -> Eff (FileSystem ': effs) ~> Eff effs
-- runInMemoryFileSystem initVfs = evalState initVfs . fsToState where
--   fsToState :: Eff (FileSystem ': effs) ~> Eff (State [(FilePath, String)] ': effs)
--   fsToState = reinterpret $ \case
--     ReadFile path -> get >>= \vfs -> case lookup path vfs of
--       Just contents -> pure contents
--       Nothing       -> error ("readFile: no such file " ++ path)
--     WriteFile path contents -> modify $ \vfs ->
--       (path, contents) : deleteBy ((==) `on` fst) (path, contents) vfs

-- a2 :: (Member FileSystem effs, Member (Writer Text) effs) => Eff effs String
-- a2 = do
--   writeFile1 "2" "!"
--   tell $ pack "ttt"
--   return ""

-- -- a :: Eff '[Writer Text, FileSystem] String
-- a :: (Member (Error String) effs, Member FileSystem effs, Member (Writer Text) effs) => Eff effs String
-- a = do
--   writeFile1 "1" "2"
--   a2
--   throwError "ee"
--   f <- readFile1 "1"
--   readFile1 f

-- -- do
-- --   (v, logs) <- runWriter a
-- --   return v

-- -- t :: Eff '[IO] String
-- -- t = runFileSystemIO a

-- g :: Either String (String, Text)
-- g =
--   (run $ runError $ runWriter $ runInMemoryFileSystem [] a)
--   -- in w

-- spec :: Spec
-- spec =
--   describe "Verify endpoint" $ do
--     it "sdfsd" $ do
--       -- runIdentity f
--       -- return ()


--       g `shouldBe` Right( ("1", "1"))
--       -- (runIdentity f) `shouldBe` Left ""
--       -- f `shouldBe`  Left ""


-- -- newtype TestM m a = TestM (ReaderT Config (ExceptT Text m) a)
-- --   deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadError Text)

-- -- unTestM (TestM (x)) = runIdentity $ runExceptT $ runReaderT x (Config {})

-- -- instance MonadLdap (TestM Identity) where
-- --   config = ask -- default
-- --   searchLdap base mod filter atts = throwError "2"
-- --   modifyLdap base operations = throwError "1"
-- --   loginLdap user password = pure $ \l -> return ()

-- -- -- f :: Identity (Either Text SearchEntry)
-- -- -- f :: Either Text [SearchEntry]
-- -- f = unTestM $ do
-- --   -- Config {_activeUsersContainer} <- config
-- --   getUserByUsername (Dn "") "123"


