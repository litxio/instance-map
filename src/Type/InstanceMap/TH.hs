{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Type.InstanceMap.TH (
  mkMap,
  mkMapWithOpts,
  defaultOptions,
  ClassName, InputTypeName, OutputWrapperName,
  Some(..),
  Options(..)) where 

import Data.Typeable
import qualified Data.Set as S
import Data.List (nub, intersect)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict (StateT(..), put, get, evalStateT, modify, withStateT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (for)
import qualified Debug.Trace as DBG
import Control.Monad (join, foldM, when)
import Language.Haskell.TH
import GHC.Exts

data family Some (c :: * -> Constraint)

data Options = Options {
-- | How deep to traverse the tree of instance contexts when attempting to find 
-- monomorphic types that are members of the class.  Default is 2.
--
-- For instance, if we have declarations like:
--
-- > data NumWithS n = NumWithS String n
-- > instance (Num a) => Num (NumWithS a)
--
-- with maxDepth set to 2, then NumWithS Int and NumWithS (NumWithS Int) would
-- both be found but NumWithS (NumWithS (NumWithS Int)) would not be.
 maxDepth :: Int,
-- | Whether to produce lots of debugging output.  Default False.
 verbose :: Bool,
 witnessGenerator :: ExpQ,
 witnessTypeName :: Name
}
defaultOptions = Options 2 False [|typeRep|] ''TypeRep

type ConcreteType = Type
type InstanceHead = Type

type ClassName = Name

type InputTypeName = Name

type OutputWrapperName = Name

data InstanceTraversalState = InstanceTraversalState {
  classesInProgress :: [Name],
  classesDone :: M.Map Name [ConcreteType]
 }
initialTraversalState = InstanceTraversalState [] M.empty

type StateQ a = StateT InstanceTraversalState Q a 

-- get a list of instances
getInstances :: Name -> Q [InstanceDec]
getInstances typ = do
  ClassI _ instances <- reify typ
  return instances

showInstances :: Name -> Q Exp
showInstances typ = do
  ins <- getInstances typ
  return . LitE . stringL $ show ins

-- | When used as a declaration splice, this function will create three declarations:
--
--   * A data instance 'Some' /ClassName/ with a single constructor Some/ClassName/
--
--   * A value named mapOf/ClassName/, which has type
-- 
-- @
--   'M.Map' 'TypeRep' /InputType/ -> 'Some' /Class/
-- @
--
--   * A helper function getSome/ClassName/ with signature
--
-- @
--   'TypeRep' -> /InputType/ -> 'Maybe' ('Some' /Class/)
-- @
--
-- The four arguments to this function are:
--
--   1. A TypeRep indicating what monomorphic type we should try to decode
--
--   2. The name of the type that is the argument to the decoder function.  
--     Often ''ByteString or ''Aeson.Value.
--
--   3. The name of a monad type with an instance of MonadFail that the functional 
--     expression uses to wrap the return value.
--     Often ''Maybe.
--
--   4. A quasiquoted expression for a function that takes an argument of the type
--      named by (2) and has a polymorphic return type wrapped in the monad (3).
--
--  For example, for JSON the call generally be:
--
-- > $(mkMap ''MyClass ''Value ''Result [|fromJSON|])
mkMap :: ClassName -> InputTypeName -> OutputWrapperName -> ExpQ -> Q [Dec]
mkMap = mkMapWithOpts defaultOptions

-- | Like 'mkMap' but with user-provided 'Options'
mkMapWithOpts :: Options -> ClassName -> InputTypeName -> OutputWrapperName -> ExpQ -> Q [Dec]
mkMapWithOpts opts className inType outWrap fExp = do
  typs <- knownConcreteInstances opts className
  lst <- mapM mkExp typs
  let witnesses = traverse mkWitness typs
  typMap <- [|M.fromList $ zip ($(ListE <$> witnesses)) $(return $ ListE lst)|]
  decoderE <- [| case M.lookup trep $(varE mapName) of
                   Nothing -> fail $ "No instance found for " ++ show trep
                   Just f  -> f v |]

  a <- newName "a"
  let dataInstD = DataInstD []
                            ''Some
                            [ConT className]
                            Nothing
                            [ForallC [PlainTV a] 
                                     [AppT (ConT ''Typeable) (VarT a),AppT (ConT className) (VarT a)]
                                     (NormalC someName [(Bang NoSourceUnpackedness NoSourceStrictness,
                                                        (VarT a))])]
                            []
      mapDefinitionD = ValD (VarP mapName) (NormalB typMap) []
      decoderSigD = SigD getterName (AppT (AppT ArrowT (ConT (witnessTypeName opts)))
                                          (AppT (AppT ArrowT (ConT inType))
                                                (AppT (ConT outWrap)
                                                (AppT (ConT ''Some) (ConT className)))))
      decoderD = FunD getterName [Clause [VarP (mkName "trep"), VarP (mkName "v")] (NormalB decoderE) []]

  return $ [dataInstD, mapDefinitionD, decoderSigD, decoderD]
    
    where
      mkExp :: Type -> Q Exp
      mkExp t = [| fmap $(conE someName) . ($(fExp) :: $(conT inType) -> $(appT (conT outWrap) (return t))) |]
      someName = mkName ("Some" ++ nameBase className)
      mapName  = mkName ("mapOf" ++ nameBase className)
      getterName  = mkName ("getSome" ++ nameBase className)
      mkWitness :: ConcreteType -> ExpQ
      mkWitness t = [|$(witnessGenerator opts) (Proxy :: Proxy $(return t))|]



-- Get all the variables in constraints; get the instances for each constraint; intersect
-- over the constraints; replace variables in the instance type
knownConcreteInstances :: Options -> Name -> Q [ConcreteType]
knownConcreteInstances opts className = evalStateT (knownConcreteInstances' opts className) initialTraversalState


knownConcreteInstances' :: Options -> Name -> StateQ [ConcreteType]
knownConcreteInstances' opts className = do
  InstanceTraversalState {..} <- get
  -- warn ("Depth " ++ show (S.size classesInProgress) ++ "; stack is " ++ show (S.toList classesInProgress))
  case M.lookup className classesDone of
    Just types -> return types
    Nothing ->
      if length classesInProgress > maxDepth opts
         then warn opts ("Cutting off recursion at " ++ show className) >> return []
         else do
           info opts $ "Looking for instances of " ++ show className
           trInsts <- lift $ getInstances className :: StateQ [InstanceDec]
           concreteInsts <- for trInsts $ \(InstanceD _ ctx (AppT _ head) _) -> do
             s <- get
             lift $ evalStateT (deepReplaceVars opts head ctx)
                               (s { classesInProgress = className : classesInProgress })
           let retVal = join concreteInsts
           info opts $ "Returning instances for " ++ show className ++ ": " ++ show retVal 
           modify $ \s -> s { classesDone = M.insert className retVal classesDone }
           return retVal -- Flatten the list of lists

-- Ctx is a list of univariate constraints.  Return a list of the types we can find that
-- satisfy those constraints
-- If it's a simple declaration like "instance Eq MyType" then we're done
-- but if there is a context with variables e.g. instance (XYZ a) => Eq a
-- then we need to recurse and find instances of XYZ and replace the a
deepReplaceVars :: Options -> InstanceHead -> Cxt -> StateQ [ConcreteType]
deepReplaceVars opts t constraints
  | monomorphic t = return [t]
  | otherwise        = do
    InstanceTraversalState {..} <- get
    -- Get type variables and their candidate types for each constraint
    if (not (all univariate constraints))
     then do 
       warn opts $ "Only simple univariate constraints (like 'C a') are supported (skipping "
                   ++ show t ++ " due to " ++ show (filter univariate constraints) ++ ")"
       return []
     else do
        constraintCandidates <- traverse getVarsAndCandidates constraints :: StateQ [(Name, [ConcreteType])]
        -- Group by variable
        let mc :: M.Map Name [[Type]]
            mc = M.fromListWith (flip mappend) $ fmap (\(n, typs) -> (n, [typs])) constraintCandidates
            -- Possible values for a variable are those that satisfy every constraint
            possibleVals = foldl1 intersect <$> mc
        return $ allReplacements possibleVals t

    where 
      univariate (AppT (ConT cls) (VarT v)) = True
      univariate _                          = False
      getVarsAndCandidates :: Type -> StateQ (Name, [ConcreteType])
      getVarsAndCandidates (AppT (ConT cls) (VarT v)) = (v,) <$> knownConcreteInstances' opts cls
      getVarsAndCandidates t = lift $ (,[]) <$> newName ""-- error ("Unexpected constraint in instance context: " ++ show t)

warn opts s = if verbose opts 
                 then lift $ reportWarning s
                 else return ()

info opts s = if verbose opts 
                 then lift $ runIO $ putStrLn s
                 else return ()

allReplacements :: M.Map Name [ConcreteType] -> InstanceHead -> [ConcreteType]
allReplacements var2candidates instHead = foldM substitute instHead (assocs var2candidates)
  where 
    assocs hm = zip (M.keys hm) (M.elems hm)
    substitute :: Type -> (Name, [ConcreteType]) -> [Type]
    substitute (VarT v1) (v2, substs)
      | v1 == v2         = substs
      | otherwise        = [VarT v1]
    substitute (AppT t1 t2) s = AppT <$> (substitute t1 s) <*> (substitute t2 s)
    substitute (SigT t1 k) s = SigT <$> (substitute t1 s) <*> [k]
    substitute (InfixT t1 n t2) s = InfixT <$> (substitute t1 s) <*> [n] <*> (substitute t2 s)
    substitute (UInfixT t1 n t2) s = UInfixT <$> (substitute t1 s) <*> [n] <*> (substitute t2 s)
    substitute (ParensT t1) s = ParensT <$> (substitute t1 s)
    substitute t _ = [t]

monomorphic :: Type -> Bool
monomorphic (VarT v) = False
monomorphic (AppT t1 t2) = monomorphic t1 && monomorphic t2
monomorphic (SigT t1 _) = monomorphic t1
monomorphic (InfixT t1 n t2) = monomorphic t1 && monomorphic t2
monomorphic (UInfixT t1 n t2) = monomorphic t1 && monomorphic t2
monomorphic (ParensT t1) = monomorphic t1
monomorphic _ = True

intersectInstances :: [Name] -> Q [InstanceDec]
intersectInstances classNames = foldl1 intersect <$> (traverse getInstances classNames)
-- indexMapRegistry = fromList [
--   ("Key", fromList [
--     ("Security", fromJSON :: Result (Key Security)),
--     ("Snapshot", fromJSON :: Result (Key Snapshot)),
--     ...
