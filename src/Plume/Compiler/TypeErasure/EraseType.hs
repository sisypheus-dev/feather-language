{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.TypeErasure.EraseType where

import Data.Bitraversable
import Data.List qualified as List
import GHC.IO hiding (liftIO)
import Plume.Compiler.TypeErasure.Syntax qualified as Post
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Concrete (Position)
import Plume.Syntax.Common.Literal
import Plume.TypeChecker.Monad
import Plume.TypeChecker.TLIR qualified as Pre
import Control.Monad.Exception (compilerError)

-- | TYPE ERASURE
-- | Type erasure is a simple process that aims at removing all type information
-- | from the program as we don't need it anymore. This is done by converting
-- | all typed expressions to untyped expressions.
-- | 
-- | But, we should take care of some things:
-- |
-- |  - We should transform ADTs in order just to extract functions and 
-- |    declarations.
-- |
-- |  - We should transform extensions in order to remove every Instance
-- |    expression
-- |
-- | Conventionnaly, we transform ADTs with type arguments into functions that
-- | take the same number of arguments as the type arguments and return a
-- | list of untyped expressions. To recognize ADTs, we look for a `special`
-- | expression in the first position of the list. If it's not there, we
-- | consider it as a variable.
-- | 
-- | Special values should not appear by default in the program, they should be
-- | only available from that point.

{-# NOINLINE dispatched #-}
dispatched :: IORef [((PlumeType, Text), Text)]
dispatched = unsafePerformIO $ newIORef mempty

{-# NOINLINE currentPosition #-}
currentPosition :: IORef (Maybe Position)
currentPosition = unsafePerformIO $ newIORef Nothing

{-# NOINLINE program #-}
program
  :: IORef [Post.UntypedProgram]
program = unsafePerformIO $ newIORef []

isNotDecl :: Pre.Expression -> Bool
isNotDecl (Pre.EDeclaration {}) = False
isNotDecl (Pre.EMutUpdate {}) = False
isNotDecl _ = True

arity :: Identity PlumeType -> Int
arity (Identity (a :->: _)) = length a
arity _ = -1

eraseType :: [Pre.TypedExpression] -> IO [Post.UntypedProgram]
eraseType (Pre.EDeclaration _ (Annotation name _ _) (Pre.EClosure args _ body isAsync) Nothing : xs) = do
  let args' = map (\(Annotation n _ _) -> n.identifier) args
  b' <- eraseStatement body
  let fun = Post.UPFunction name.identifier args' b' isAsync
  modifyIORef' program (<> [fun])
  eraseType xs
eraseType (Pre.EVariableDeclare _ name t : xs) = do
  modifyIORef' program (<> [Post.UPDeclare name (arity t)])
  eraseType xs
eraseType (Pre.ENativeFunction fp n _ (args :->: _) isStd : xs) = do
  modifyIORef'
    program
    (<> [Post.UPNativeFunction fp n (length args) isStd])
  eraseType xs
eraseType (Pre.EType tyName ts : xs) = do
  tys <- mapM createFunction ts
  modifyIORef' program (<> tys)
  eraseType xs
 where
  createVariant n = "a" <> show n
  ts' =
    map
      ( \case
          Pre.TVariable n ->
            ( n
            , const $
                Post.UEList
                  [ Post.UESpecial
                  , Post.UELiteral (LString tyName.annotationName.identifier)
                  , Post.UELiteral (LString n)
                  ]
            )
          Pre.TConstructor n _ ->
            ( n
            , \vars ->
                Post.UEList
                  ( [ Post.UESpecial
                    , Post.UELiteral (LString tyName.annotationName.identifier)
                    , Post.UELiteral (LString n)
                    ]
                      <> map Post.UEVar vars
                  )
            )
      )
      ts
  createFunction :: Pre.TypeConstructor PlumeType -> IO Post.UntypedProgram
  createFunction (Pre.TVariable n) = case List.lookup n ts' of
    Just f -> return $ Post.UPDeclaration n (f [])
    Nothing -> compilerError "Type constructor not found"
  createFunction (Pre.TConstructor n vars) = case List.lookup n ts' of
    Just f -> return $ Post.UPADTFunction n args (Post.USReturn (f args))
     where
      args = map createVariant [0 .. length vars - 1]
    Nothing -> compilerError "Type constructor not found"
eraseType (Pre.EDeclaration _ (Annotation name _ True) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPMutDeclaration name.identifier e'])
  eraseType xs
eraseType (Pre.EDeclaration _ (Annotation name _ _) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPDeclaration name.identifier e'])
  eraseType xs
eraseType (Pre.EMutUpdate (Annotation name _ _) e Nothing : xs) = do
  e' <- eraseExpr e
  modifyIORef' program (<> [Post.UPMutUpdate name.identifier e'])
  eraseType xs
eraseType (Pre.EInstanceDeclare {} : xs) = eraseType xs
-- eraseType (Pre.EEmpty : xs) = eraseType xs
eraseType (x : xs) = do
  x' <- eraseStatement x
  modifyIORef' program (<> [Post.UPStatement x'])
  ys <- eraseType xs
  return (Post.UPStatement x' : ys)
eraseType [] = return []

eraseStatement :: Pre.TypedExpression -> IO Post.UntypedStatement
eraseStatement (Pre.EReturn e) =
  transformReturnE . Post.USReturn <$> eraseExpr e
eraseStatement (Pre.EDeclaration _ (Annotation n _ True) e1 Nothing) =
  Post.USMutDeclaration n.identifier <$> eraseExpr e1
eraseStatement (Pre.EDeclaration _ (Annotation n _ _) e Nothing) = Post.USDeclaration n.identifier <$> eraseExpr e
eraseStatement (Pre.EMutUpdate (Annotation n _ _) e1 Nothing) =
  Post.USMutUpdate n.identifier <$> eraseExpr e1
eraseStatement (Pre.EConditionBranch e1 e2 e3) = do
  e3' <- case e3 of
    Just e3' -> eraseStatement e3'
    Nothing -> pure $ Post.USExpr (Post.UEBlock [])

  Post.USConditionBranch 
    <$> eraseExpr e1
    <*> eraseStatement e2
    <*> pure e3'
eraseStatement (Pre.EWhile c b) = do
  c' <- eraseExpr c
  b' <- eraseStatement b
  return $ Post.USWhile c' b'
eraseStatement e = Post.USExpr <$> eraseExpr e

eraseExpr :: Pre.TypedExpression -> IO Post.UntypedExpr
eraseExpr (Pre.EUnMut e) = Post.UEUnMut <$> eraseExpr e
eraseExpr (Pre.EDeclaration _ (Annotation n _ True) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutDeclaration n.identifier e1' e2''
    Nothing -> compilerError "Mut declaration without a body"
eraseExpr (Pre.EMutUpdate (Annotation n _ _) e1 e2) = do
  e1' <- eraseExpr e1
  e2' <- maybeM e2 eraseExpr
  case e2' of
    Just e2'' -> return $ Post.UEMutUpdate n.identifier e1' e2''
    Nothing -> compilerError "Mut update without a body"
eraseExpr (Pre.EVariable x _) = pure $ Post.UEVar x.identifier
eraseExpr (Pre.EApplication f args) =
  Post.UEApplication <$> eraseExpr f <*> mapM eraseExpr args
eraseExpr (Pre.ELiteral l) = pure $ Post.UELiteral l
eraseExpr (Pre.EList es) = Post.UEList <$> mapM eraseExpr es
eraseExpr (Pre.EDeclaration _ (Annotation n _ _) e1 e2) = case e2 of
  Just e2' -> Post.UEDeclaration n.identifier <$> eraseExpr e1 <*> eraseExpr e2'
  Nothing -> compilerError "Declaration without a body"
eraseExpr (Pre.EConditionBranch e1 e2 e3) = do
  e3' <- case e3 of
    Just e3' -> eraseExpr e3'
    Nothing -> pure $ Post.UEBlock []

  Post.UEConditionBranch
    <$> eraseExpr e1
    <*> eraseExpr e2
    <*> pure e3'
eraseExpr (Pre.ESwitch e cases) = do
  e' <- eraseExpr e
  cases' <- mapM (bimapM erasePattern eraseExpr) cases
  return $ Post.UESwitch e' cases'
eraseExpr (Pre.EBlock es) = Post.UEBlock <$> mapM eraseStatement es
eraseExpr (Pre.EClosure args _ body isAsync) = do
  b <- eraseExpr body
  return $
    Post.UEClosure
      (map (\(Annotation n _ _) -> n.identifier) args)
      (Post.USExpr b)
      isAsync
eraseExpr (Pre.EEqualsType e t) = Post.UEEqualsType <$> eraseExpr e <*> pure t
eraseExpr (Pre.ENativeFunction {}) = compilerError "Native functions aren't expressions"
eraseExpr (Pre.EAnd e1 e2) = Post.UEAnd <$> eraseExpr e1 <*> eraseExpr e2
eraseExpr (Pre.EIndex e i) = Post.UEIndex <$> eraseExpr e <*> eraseExpr i
eraseExpr (Pre.EReturn e) = eraseExpr e
eraseExpr (Pre.EType {}) = compilerError "Type isn't an expression"
eraseExpr (Pre.EInstanceVariable name _) = pure $ Post.UEVar name.identifier
eraseExpr (Pre.EInstanceAccess expr i) = do
  expr' <- eraseExpr expr
  pure (Post.UEIndex expr' (Post.UELiteral (LInt (toInteger i))))
-- eraseExpr Pre.EEmpty = error "Should not encounter empty expressions"
eraseExpr (Pre.ESpreadable _) = error "Should not encounter spreadable expressions"
eraseExpr (Pre.EInstanceDict _ _ exprs) = do
  exprs' <- mapM eraseExpr exprs
  return (Post.UEList exprs')
eraseExpr _ = compilerError "Unknown expression"

erasePattern :: Pre.Pattern -> IO Post.UntypedPattern
erasePattern (Pre.PVariable x _) = pure $ Post.UPVariable x
erasePattern (Pre.PLiteral l) = pure $ Post.UPLiteral l
erasePattern (Pre.PConstructor (n, _) ps) = Post.UPConstructor n <$> mapM erasePattern ps
erasePattern (Pre.PWildcard _) = pure Post.UPWildcard
erasePattern (Pre.PSpecialVar x _) = pure $ Post.UPSpecialVariable x
erasePattern (Pre.PList _ ps t) =
  Post.UPList <$> mapM erasePattern ps <*> maybeM t erasePattern
erasePattern _ = compilerError "Unknown pattern"

erase :: [Pre.TypedExpression] -> IO [Post.UntypedProgram]
erase xs = do
  writeIORef program []
  void $ eraseType xs
  readIORef program

findWithKey :: (k -> Bool) -> [(k, a)] -> Maybe (k, a)
findWithKey f = List.find (f . fst)

findWithKeyM :: Monad m => (k -> m Bool) -> [(k, a)] -> m (Maybe (k, a))
findWithKeyM f = foldr go (pure Nothing)
 where
  go (k, a) acc = do
    b <- f k
    if b then return $ Just (k, a) else acc

decompose :: [a] -> ([a], Maybe a)
decompose [] = ([], Nothing)
decompose [x] = ([], Just x)
decompose xs = case (viaNonEmpty init xs, viaNonEmpty last xs) of
  (Just init', Just last') -> (init', Just last')
  (_, _) -> ([], Nothing)

transformReturn :: Post.UntypedExpr -> Post.UntypedExpr
transformReturn (Post.UEBlock es) = case decompose es of
  (init', Just (Post.USExpr l)) -> Post.UEBlock (init' <> [transformReturnE (Post.USReturn l)])
  _ -> Post.UEBlock es
transformReturn e = Post.UEBlock [transformReturnE (Post.USReturn e)]

transformReturnE :: Post.UntypedStatement -> Post.UntypedStatement
transformReturnE (Post.USReturn (Post.UESwitch e cases)) = do
  let cases' = map (second transformReturn) cases
  Post.USExpr (Post.UESwitch e cases')
transformReturnE (Post.USReturn e) = Post.USReturn e
transformReturnE e = e
