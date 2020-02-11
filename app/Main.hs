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
module Main where

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
  PostMessage     :: Text -> FacebookEffect ()

getUserEmail' :: Member FacebookEffect effs => Text -> Eff effs (Either Int Text)
getUserEmail' e = send $ GetUserEmail e

postMessage' :: Member FacebookEffect effs => Text -> Eff effs ()
postMessage' m = send $ PostMessage m


(<?>) :: Member (Error e) effs => Either a1 a2 -> e -> Eff effs a2
ei <?> message = do
  case ei of
    Left _  -> throwError message
    Right v -> return v

program :: (Member LdapEffect effs, Member (Error Text) effs, Member FacebookEffect effs) => Eff effs ()
program = do
  emailEither <- getUserEmail' "username"
  emailEither <- getUserEmail' "username"
  email <- emailEither <?> (pack "error")
  entries <- searchLdap' (Dn email) undefined undefined undefined
  entries <- searchLdap' (Dn email) undefined undefined undefined
  postMessage' $ formatEntries entries
  emailEither <- getUserEmail' "username"
  postMessage' $ formatEntries entries
  where
    formatEntries e = ""

real :: Eff '[FacebookEffect, LdapEffect, Error Text, IO] a -> IO (Either Text a)
real = runM . runError . runLdap . runFacebook
  where
    runLdap :: LastMember IO effs => Eff (LdapEffect ': effs) ~> Eff effs
    runLdap = interpretM $ \case
      SearchLdap d m f a -> return []
      ModifyLdap d o -> putStrLn "modifying ldap"
    runFacebook :: LastMember IO effs => Eff (FacebookEffect ': effs) ~> Eff effs
    runFacebook = interpretM $ \case
      GetUserEmail msg -> return $ Right "email"
      PostMessage message -> putStrLn "posting fb message"

-- f1 :: (LastMember (Writer [Text]) effs ) =>  Eff (FacebookEffect : LdapEffect : Error e : effs) a -> Eff effs (Either e a)
-- f1 = runError . fakeLdap . fakeFacebook
--   where


fakeLdap :: (Member (Writer [Text]) effs ) => Eff (LdapEffect : effs) ~> Eff (effs)
fakeLdap = interpret $ \case
  SearchLdap d m g a -> do
    tell [pack "searching in ldap"]
    return []
  ModifyLdap d o     -> do
    tell [pack "modifying ldap"]
    return ()
fakeFacebook :: (Member (Writer [Text]) effs ) => Eff (FacebookEffect : effs) ~> Eff (effs)
fakeFacebook = interpret $ \case
  GetUserEmail msg -> do
    tell [pack "getting user"]
    return $ Right "email"
  PostMessage message -> do
    tell [pack "posting message"]
    return ()

-- f2 :: Eff '[Writer [Text]] (Either Text a) -> Eff '[] ((Either Text a), [Text])
-- f2 = runWriter

-- f3 :: Eff '[Writer [Text]] (Either Text a) -> (Either Text a, [Text])
-- f3 = run . runWriter


fake :: Eff '[FacebookEffect, LdapEffect, Error Text, Writer [Text]] a -> (Either Text a, [Text])
fake = run . runWriter . runError . fakeLdap . fakeFacebook

main :: IO ()
main = do
  r <- real program
  putStrLn $ show r

  putStrLn "----"

  let a = fake program
  putStrLn $ show a
