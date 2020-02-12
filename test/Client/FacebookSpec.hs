module Client.FacebookSpec (
  spec
) where

import           Test.Hspec ( Spec )

spec :: Spec
spec = return ()
-- fake :: Config -> Eff '[FacebookEffect, Error Text, Reader Config] a -> Either Text (a, [Text])
-- fake config = run . runReader config . runError . runWriter . fakeFacebook


-- fakeFacebook :: Eff (FacebookEffect ': effs) ~> Eff (Writer [Text] ': effs)
-- fakeFacebook = reinterpret $ \case
--   SendText r -> do
--     tell [pack "send text"]
--     return $ SendTextMessageResponse " " ""
--   GetInfo a -> do
--     tell [pack "getting info"]
--     return $ GetUserInfoMessageResponse ""
--   SendHelp p -> do
--     tell [pack "sending help"]
--     return $ SendTextMessageResponse " " ""
