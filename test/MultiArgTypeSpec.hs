{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module MultiArgTypeSpec where

import Test.Hspec
import Type.InstanceMap
import GHC.Generics
import Data.Typeable
import Data.Either (isLeft)
import qualified Data.Binary as Binary
import Control.Monad.Fail (MonadFail(..))
import Data.Proxy
import Data.Aeson hiding (defaultOptions)
import Data.ByteString.Lazy

data Baz a b = Baz a b deriving (Eq, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (Baz a b)
instance (FromJSON a, FromJSON b) => FromJSON (Baz a b)

class (ToJSON a, FromJSON a) => Z a where
  foo :: a -> String

instance Z Int where
  foo x = "Int!"

instance Z Bool where
  foo x = "Bool!"

instance (Z a, Z b) => Z (Baz a b) where
  foo (Baz a b) = "Wrapped " ++ foo a ++ " and " ++ foo b

-- Declares a type instance Some Z and variable called mapOfZ of type
-- Map TypeRep (ByteString -> Maybe (Some Z))
$(mkMap ''Z ''Value ''Result [|fromJSON|])

typeTaggedData = [
  (Binary.encode (typeRep (Proxy :: Proxy (Baz Int Bool))), toJSON (Baz 6 False :: Baz Int Bool)),
  (Binary.encode (typeRep (Proxy :: Proxy (Baz Int Char))), toJSON (Baz 6 'd' :: Baz Int Char))
 ]

spec = do
  describe "test decoding" $ do
    it "should decode Baz Int Bool" $ do
      let (typeTag, dta) = typeTaggedData !! 0
      runFoo <$> getSomeZ (Binary.decode typeTag) dta `shouldBe` Success "Wrapped Int! and Bool!"
    it "should NOT decode Baz Int Char" $ do
      let (typeTag, dta) = typeTaggedData !! 1
      runFoo <$> getSomeZ (Binary.decode typeTag) dta `shouldSatisfy` isError
  where runFoo (SomeZ s) = foo s
        isError (Error s) = True
        isError _         = False


