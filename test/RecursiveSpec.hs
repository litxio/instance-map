{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module RecursiveSpec where

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

data Bar a = Bar a deriving (Eq, Generic)
instance (ToJSON a) => ToJSON (Bar a)
instance (FromJSON a) => FromJSON (Bar a)

class (ToJSON a, FromJSON a) => Y a where
  foo :: a -> String

instance Y Int where
  foo x = "Int!"

instance (Y a) => Y (Bar a) where
  foo (Bar a) = "Wrapped " ++ foo a

-- Declares a type instance Some Y and variable called mapOfY of type
-- Map TypeRep (ByteString -> Maybe (Some Y))
$(mkMapWithOpts defaultOptions {maxDepth = 2} ''Y ''Value ''Result [|fromJSON|])

typeTaggedData = [
  (Binary.encode (typeRep (Proxy :: Proxy Int)), toJSON (6 :: Int)),
  (Binary.encode (typeRep (Proxy :: Proxy (Bar Int))), toJSON (Bar 6 :: Bar Int)),
  (Binary.encode (typeRep (Proxy :: Proxy (Bar (Bar Int)))), toJSON (Bar (Bar 6) :: Bar (Bar Int))),
  (Binary.encode (typeRep (Proxy :: Proxy (Bar (Bar (Bar Int))))), toJSON (Bar (Bar (Bar 6)) :: Bar (Bar (Bar Int))))
 ]

spec = do
  describe "test decoding" $ do
    it "should decode Int" $ do
      let (typeTag, dta) = typeTaggedData !! 0
      runFoo <$> getSomeY (Binary.decode typeTag) dta `shouldBe` Success "Int!"
    it "should decode once-wrapped int" $ do
      let (typeTag, dta) = typeTaggedData !! 1
      runFoo <$> getSomeY (Binary.decode typeTag) dta `shouldBe` Success "Wrapped Int!"
    it "should decode twice-wrapped int" $ do
      let (typeTag, dta) = typeTaggedData !! 2
      runFoo <$> getSomeY (Binary.decode typeTag) dta `shouldBe` Success "Wrapped Wrapped Int!"
    it "should NOT decode thrice-wrapped int" $ do
      let (typeTag, dta) = typeTaggedData !! 3
      runFoo <$> getSomeY (Binary.decode typeTag) dta `shouldSatisfy` isError
  where runFoo (SomeY s) = foo s
        isError (Error s) = True
        isError _         = False


