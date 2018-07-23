# instance-map

`instance-map` provides Template Haskell functions that help go from serialized values with value-level type witnesses (i.e. TypeRep values) to existential types containing type-level evidence of membership in a type class.  It is useful for dealing with serialized values when only membership in a certain class (and not the monomorphic type) is known at the site of deserialization.

Because Haskell is not a dependently typed language, there is ordinarily no way to "lift" information at the value level to the type level.  This restriction makes dealing with serialized data for which the monomorphic type (or at least a list of the possible types) is not known at the site of use very difficult.  

This library works by using Template Haskell to try to generate a map from TypeReps of the the monomorphic types belonging to a given type class to their decoder functions.  It does this by recursively looking for instances of the given type class, then instances of any type class mentioned in the instance context, and so on.  For example if we have

```haskell
class Foo a
class Bar a
instance Foo a => Bar (Maybe a))
```

then `mkMap` will try to generate instances of Bar by looking for instances of Foo and replacing the `a` in the instance head.

Because the monomorphic members of a type class are potentially infinite in number (e.g. Maybe (Maybe (Maybe ...))), this library will only attempt to generate monomorphic types up to a given level of nesting (default 2).

Currently only univariate type classes are supported, and type synonyms are not supported.  The library operates on a "best-efforts" basis, so if it encounters a multiparameter type class or other constraint it doesn't know how to satisfy, it will give up on that branch but continue to try other ways of finding the monomorphic type class members.

## Example: Deserialization of JSON to value with evidence of type class membership
```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

import Type.InstanceMap

import Data.Aeson
import Data.Proxy
import Data.Typeable
import qualified Data.Binary as Binary
import Data.ByteString.Lazy hiding (putStrLn)

class (FromJSON a) => MyClass a where
  foo :: a -> String

instance MyClass Int where
  foo x = "Fooing Int " ++ show x

instance MyClass Bool where
  foo x = "Fooing Bool " ++ show x

-- Creates a function:
-- getSomeMyClass :: TypeRep -> Value -> Result (Some MyClass)
$(mkMap ''MyClass ''Value ''Result [|fromJSON|])

-- Pretend we have read the below values from the network or disk. 
-- There is usually no way to lift the TypeRep evidence to the value level.

mysteryValue :: Value
mysteryValue = toJSON (6 :: Int)

mysteryValueTypeData :: ByteString
mysteryValueTypeData = Binary.encode (typeRep (Proxy :: Proxy Int))

main = do
                  
  let someVal :: Some MyClass  -- Existential type with a single constructor, SomeMyClass
      someVal = getSomeMyClass (Binary.decode mysteryValueTypeData :: TypeRep) mysteryValue
  putStrLn $ case  of
    Error e -> "Whoops"
    Success (SomeMyClass v) -> foo v  -- Inside the pattern match I know v has a MyClass instance
```
