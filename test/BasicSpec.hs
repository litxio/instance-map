{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module BasicSpec where

import Test.Hspec
import Type.InstanceMap
import Data.Typeable
import Data.Either (isLeft)
import Control.Monad.Fail (MonadFail(..))
import Data.Proxy
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy

class (Binary a) => X a where
  foo :: a -> String

instance X Int where
  foo x = "I am an Int!"

instance X Bool where
  foo x = "I am a Bool!"


decodeOrFailEasy :: (Binary a) => ByteString -> Maybe a
decodeOrFailEasy = fmap third . eitherToMaybe . decodeOrFail
  where 
    third (a,b,c) = c
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Left x) = Nothing
    eitherToMaybe (Right y) = Just y

-- Declares a type instance Some X and variable called mapOfX of type
-- Map TypeRep (ByteString -> Maybe (Some X))
$(mkMap ''X ''ByteString ''Maybe [|decodeOrFailEasy|])


typeTaggedData = [
  (encode (typeRep (Proxy :: Proxy Int)), encode (6 :: Int)),
  (encode (typeRep (Proxy :: Proxy Bool)), encode (True)),
  (encode (typeRep (Proxy :: Proxy Double)), encode (6.4 :: Double))
 ]

spec = do
  describe "test decoding" $ do
    it "should decode Int" $ do
      let (typeTag, dta) = typeTaggedData !! 0
      runFoo <$> getSomeX (decode typeTag) dta `shouldBe` Just "I am an Int!"
    it "should decode Bool" $ do
      let (typeTag, dta) = typeTaggedData !! 1
      runFoo <$> getSomeX (decode typeTag) dta `shouldBe` Just "I am a Bool!"
    it "should NOT decode Double" $ do
      let (typeTag, dta) = typeTaggedData !! 2
      runFoo <$> getSomeX (decode typeTag) dta `shouldBe` Nothing
  where runFoo (SomeX s) = foo s


