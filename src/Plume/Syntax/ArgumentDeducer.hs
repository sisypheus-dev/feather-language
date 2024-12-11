module Plume.Syntax.ArgumentDeducer where

import Plume.Syntax.Abstract as AST
import qualified Plume.Syntax.Common as AST
import qualified Data.List as List

deduceArgument :: AST.Expression -> AST.Expression
deduceArgument (AST.EApplication x xs) | containsDeducable xs = do
  let xs' = map deduceArgument xs
  let count = sum $ map countDeducable xs
  let args :: [Text] = ["?" <> show i | i <- [1..count]]
  
  let (x', rest) = substitute x args
  let (xs'', _) = List.foldl' (\(acc, ys) e -> do
        let (e', ys') = substitute e ys
        (acc <> [e'], ys')
        ) ([], rest) xs'

  let annotatedArgs = map (\a -> AST.Annotation (AST.MkIdentifier a False) Nothing False) args

  AST.EClosure annotatedArgs Nothing (AST.EApplication x' xs'') False
deduceArgument (AST.EBlock xs) = AST.EBlock $ map deduceArgument xs
deduceArgument (AST.EList xs) = AST.EList $ map deduceArgument xs
deduceArgument (AST.EApplication x xs) = AST.EApplication (deduceArgument x) (map deduceArgument xs)
deduceArgument (AST.ELiteral l) = AST.ELiteral l
deduceArgument (AST.EVariable e t) = AST.EVariable e t
deduceArgument (AST.EInstanceAccess x i) = AST.EInstanceAccess (deduceArgument x) i
deduceArgument (AST.EInstanceDict x t xs) = AST.EInstanceDict x t (map deduceArgument xs)
deduceArgument (AST.EInstanceVariable x t) = AST.EInstanceVariable x t
deduceArgument (AST.ETypeAlias x t) = AST.ETypeAlias x t
deduceArgument (AST.EType x xs) = AST.EType x xs
deduceArgument (AST.ENativeFunction x y xs z isStd) = AST.ENativeFunction x y xs z isStd
deduceArgument (AST.EInterface x xs ys d) = AST.EInterface x xs ys d
deduceArgument (AST.EVariableDeclare x y z) = AST.EVariableDeclare x y z
deduceArgument (AST.ERequire x) = AST.ERequire x
deduceArgument (AST.ETypeExtension xs x t ys) = AST.ETypeExtension xs x t (map deduceArgumentE ys)
  where 
    deduceArgumentE (AST.ExtDeclaration xs' x' y) = AST.ExtDeclaration xs' x' (deduceArgument y)
deduceArgument (AST.ELocated x p) = AST.ELocated (deduceArgument x) p
deduceArgument (AST.ESwitch x xs) = AST.ESwitch (deduceArgument x) (map (second deduceArgument) xs)
deduceArgument (AST.EReturn x) = AST.EReturn (deduceArgument x)
deduceArgument (AST.EConditionBranch x y z) = AST.EConditionBranch (deduceArgument x) (deduceArgument y) (fmap deduceArgument z)
deduceArgument (AST.EWhile x y) = AST.EWhile (deduceArgument x) (deduceArgument y)
deduceArgument (AST.EAwait x) = AST.EAwait (deduceArgument x)
deduceArgument (AST.EMutUpdate x y z) = AST.EMutUpdate x (deduceArgument y) (fmap deduceArgument z)
deduceArgument (AST.EClosure x y z a) = AST.EClosure x y (deduceArgument z) a
deduceArgument (AST.EDeclaration x y z zs) = AST.EDeclaration x y (deduceArgument z) (fmap deduceArgument zs)
deduceArgument (AST.EInstanceDeclare x xs ys) = AST.EInstanceDeclare x xs ys
deduceArgument (AST.ELetMatch p e) = AST.ELetMatch p (deduceArgument e)
deduceArgument (AST.EDirectExtension xs t ys) = AST.EDirectExtension xs t (map deduceArgumentE ys)
  where 
    deduceArgumentE (AST.ExtDeclaration xs' x' y) = AST.ExtDeclaration xs' x' (deduceArgument y)
deduceArgument (AST.EMonadicBind x e) = AST.EMonadicBind x (deduceArgument e)

substitute :: AST.Expression -> [Text] -> (AST.Expression, [Text])
substitute (AST.EVariable "?" _) (x:xs) = (AST.EVariable (AST.MkIdentifier x False) Nothing, xs)
substitute (AST.EBlock xs) ys = (AST.EBlock $ map (fst . flip substitute ys) xs, ys)
substitute (AST.EList xs) ys = (AST.EList $ map (fst . flip substitute ys) xs, ys)
substitute (AST.EApplication x xs) ys = (AST.EApplication x xs, ys)
substitute (AST.ELiteral l) ys = (AST.ELiteral l, ys)
substitute (AST.EVariable e t) ys = (AST.EVariable e t, ys)
substitute (AST.EInstanceAccess x i) ys = (AST.EInstanceAccess (fst $ substitute x ys) i, ys)
substitute (AST.EInstanceDict x t xs) ys = do
  let (xs', ys') = unzip $ map (substituteE ys) xs
  (AST.EInstanceDict x t xs', concat ys')
  where 
    substituteE ys' x' = (fst $ substitute x' ys', ys')
substitute (AST.EInstanceVariable x t) ys = (AST.EInstanceVariable x t, ys)
substitute (AST.ETypeAlias x t) ys = (AST.ETypeAlias x t, ys)
substitute (AST.EType x xs) ys = (AST.EType x xs, ys)
substitute (AST.ENativeFunction x y xs z isStd) ys = (AST.ENativeFunction x y xs z isStd, ys)
substitute (AST.EInterface x xs ys d) ys' = (AST.EInterface x xs ys d, ys')
substitute (AST.EVariableDeclare x y z) ys = (AST.EVariableDeclare x y z, ys)
substitute (AST.ERequire x) ys = (AST.ERequire x, ys)
substitute (AST.ETypeExtension xs x t ys) ys' = (AST.ETypeExtension xs x t (map (fst . flip substituteE ys') ys), ys')
  where 
    substituteE (AST.ExtDeclaration xs' x' y) ys'' = (AST.ExtDeclaration xs' x' (fst $ substitute y ys''), ys'')
substitute (AST.ELocated x p) ys = do
  let (x', ys') = substitute x ys
  (AST.ELocated x' p, ys')
substitute (AST.ESwitch x xs) ys = do
  let (x', ys') = substitute x ys
  let xs' = map (second (fst . flip substitute ys')) xs
  (AST.ESwitch x' xs', ys')
substitute (AST.EReturn x) ys = (AST.EReturn (fst $ substitute x ys), ys)
substitute (AST.EConditionBranch x y z) ys = (AST.EConditionBranch (fst $ substitute x ys) (fst $ substitute y ys) (fmap (fst . flip substitute ys) z), ys)
substitute (AST.EWhile x y) ys = (AST.EWhile (fst $ substitute x ys) (fst $ substitute y ys), ys)
substitute (AST.EAwait x) ys = (AST.EAwait (fst $ substitute x ys), ys)
substitute (AST.EMutUpdate x y z) ys = (AST.EMutUpdate x (fst $ substitute y ys) (fmap (fst . flip substitute ys) z), ys)
substitute (AST.EClosure x y z a) ys = (AST.EClosure x y (fst $ substitute z ys) a, ys)
substitute (AST.EDeclaration x y z zs) ys = (AST.EDeclaration x y (fst $ substitute z ys) (fmap (fst . flip substitute ys) zs), ys)
substitute (AST.EInstanceDeclare x xs ys) ys' = (AST.EInstanceDeclare x xs ys, ys')
substitute (AST.ELetMatch p e) ys = (AST.ELetMatch p (fst $ substitute e ys), ys)
substitute (AST.EDirectExtension xs t ys) ys' = (AST.EDirectExtension xs t (map (fst . flip substituteE ys') ys), ys')
  where 
    substituteE (AST.ExtDeclaration xs' x' y) ys'' = (AST.ExtDeclaration xs' x' (fst $ substitute y ys''), ys'')
substitute (AST.EMonadicBind x e) ys = (AST.EMonadicBind x (fst $ substitute e ys), ys)

countDeducable :: AST.Expression -> Int
countDeducable (AST.EVariable "?" _) = 1
countDeducable (AST.EBlock _) = 0
countDeducable (AST.EList _) = 0
countDeducable (AST.EApplication _ _) = 0
countDeducable (AST.ELiteral _) = 0
countDeducable (AST.EVariable _ _) = 0
countDeducable (AST.EInstanceAccess x _) = countDeducable x
countDeducable (AST.EInstanceDict _ _ xs) = sum $ map countDeducable xs
countDeducable (AST.EInstanceVariable _ _) = 0
countDeducable (AST.ETypeAlias _ _) = 0
countDeducable (AST.EType _ _) = 0
countDeducable (AST.ENativeFunction {}) = 0
countDeducable (AST.EInterface {}) = 0
countDeducable (AST.EVariableDeclare {}) = 0
countDeducable (AST.ERequire _) = 0
countDeducable (AST.ETypeExtension _ _ _ xs) = sum $ map countDeducableE xs
  where 
    countDeducableE (AST.ExtDeclaration _ _ x) = countDeducable x
countDeducable (AST.ELocated x _) = countDeducable x
countDeducable (AST.ESwitch x xs) = countDeducable x + sum (map (countDeducable . snd) xs)
countDeducable (AST.EReturn x) = countDeducable x
countDeducable (AST.EConditionBranch x y z) = countDeducable x + countDeducable y + maybe 0 countDeducable z
countDeducable (AST.EWhile x y) = countDeducable x + countDeducable y
countDeducable (AST.EAwait x) = countDeducable x
countDeducable (AST.EMutUpdate _ x y) = countDeducable x + maybe 0 countDeducable y
countDeducable (AST.EClosure _ _ x _) = countDeducable x
countDeducable (AST.EDeclaration _ _ x xs) = countDeducable x + maybe 0 countDeducable xs
countDeducable (AST.EInstanceDeclare {}) = 0
countDeducable (AST.ELetMatch _ e) = countDeducable e
countDeducable (AST.EDirectExtension _ _ xs) = sum $ map countDeducableE xs
  where 
    countDeducableE (AST.ExtDeclaration _ _ x) = countDeducable x
countDeducable (AST.EMonadicBind _ e) = countDeducable e

containsDeducable :: [AST.Expression] -> Bool
containsDeducable (AST.EVariable "?" _ : _) = True
containsDeducable (AST.ELocated x _ : xs) = containsDeducable (x:xs)
containsDeducable (_:xs) = containsDeducable xs
containsDeducable [] = False