module Plume.Syntax.MatchRemoval where

import Plume.Syntax.Abstract as AST
import qualified Plume.Syntax.Common as AST

manageLetMatches :: [AST.Expression] -> [AST.Expression]
manageLetMatches (AST.ELetMatch p e : es) = [AST.ESwitch (manageLetMatch e) [(p, AST.EBlock $ manageLetMatches es)]]
manageLetMatches (AST.EMonadicBind x e : es) = do
  let es' = manageLetMatches es
  let e' = case es' of
        [x'] -> x'
        xs -> AST.EBlock xs
  
  let arg = AST.Annotation x Nothing False
  let bind = AST.EApplication (AST.EVariable ">>=" Nothing) [manageLetMatch e, AST.EClosure [arg] Nothing e' False]

  [bind]
manageLetMatches (AST.EBlock xs : es) | any containsMonadicBind xs = do
  let xs' = manageLetMatches xs
  let es' = manageLetMatches es

  xs' ++ es'
manageLetMatches (AST.EBlock xs : es) = AST.EBlock (manageLetMatches xs) : manageLetMatches es
manageLetMatches (AST.ETypeExtension xs x t ys : es) = AST.ETypeExtension xs x t (manageLetMatchesE ys) : manageLetMatches es
manageLetMatches (AST.ELocated e p : es) = AST.ELocated <$> manageLetMatches (e : es) <*> pure p
manageLetMatches (AST.ESwitch x xs : es) = AST.ESwitch (manageLetMatch x) (map (second manageLetMatch) xs) : manageLetMatches es
manageLetMatches (AST.EReturn x : es) = AST.EReturn (manageLetMatch x) : manageLetMatches es
manageLetMatches (AST.EConditionBranch x y z : es) = AST.EConditionBranch (manageLetMatch x) (manageLetMatch y) (fmap manageLetMatch z) : manageLetMatches es
manageLetMatches (AST.EWhile x y : es) = AST.EWhile (manageLetMatch x) (manageLetMatch y) : manageLetMatches es
manageLetMatches (AST.EInterface x xs ys d : es) = AST.EInterface x xs ys d : manageLetMatches es
manageLetMatches (AST.ENativeFunction x y xs z isStd : es) = AST.ENativeFunction x y xs z isStd : manageLetMatches es
manageLetMatches (AST.EType x xs : es) = AST.EType x xs : manageLetMatches es
manageLetMatches (AST.EVariableDeclare x y z : es) = AST.EVariableDeclare x y z : manageLetMatches es
manageLetMatches (AST.ERequire x : es) = AST.ERequire x : manageLetMatches es
manageLetMatches (AST.EInstanceAccess x i : es) = AST.EInstanceAccess (manageLetMatch x) i : manageLetMatches es
manageLetMatches (AST.EInstanceDict x t xs : es) = AST.EInstanceDict x t (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EInstanceVariable x t : es) = AST.EInstanceVariable x t : manageLetMatches es
manageLetMatches (AST.ETypeAlias x t : es) = AST.ETypeAlias x t : manageLetMatches es
manageLetMatches (AST.EAwait x : es) = AST.EAwait (manageLetMatch x) : manageLetMatches es
manageLetMatches (AST.EMutUpdate x y z : es) = AST.EMutUpdate x (manageLetMatch y) (fmap manageLetMatch z) : manageLetMatches es
manageLetMatches (AST.EList xs : es) = AST.EList (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EApplication x xs : es) = AST.EApplication (manageLetMatch x) (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EVariable x y : es) = AST.EVariable x y : manageLetMatches es
manageLetMatches (AST.ELiteral x : es) = AST.ELiteral x : manageLetMatches es
manageLetMatches (AST.EClosure x y z a : es) = AST.EClosure x y (manageLetMatch z) a : manageLetMatches es
manageLetMatches (AST.EDeclaration x y z zs : es) = AST.EDeclaration x y (manageLetMatch z) (fmap manageLetMatch zs) : manageLetMatches es
manageLetMatches (e : es) = e : manageLetMatches es
manageLetMatches [] = []

manageLetMatchesE :: [AST.ExtensionMember] -> [AST.ExtensionMember]
manageLetMatchesE (AST.ExtDeclaration xs x y : es) = AST.ExtDeclaration xs x (manageLetMatch y) : manageLetMatchesE es
manageLetMatchesE [] = []

manageLetMatch :: AST.Expression -> AST.Expression
manageLetMatch e = case manageLetMatches [e] of
  [x] -> x
  xs -> AST.EBlock xs

containsMonadicBind :: AST.Expression -> Bool
containsMonadicBind (AST.EMonadicBind _ _) = True
containsMonadicBind (AST.EBlock xs) = any containsMonadicBind xs
containsMonadicBind (AST.ELocated x _) = containsMonadicBind x
containsMonadicBind (AST.ESwitch x xs) = containsMonadicBind x || any (containsMonadicBind . snd) xs
containsMonadicBind (AST.EReturn x) = containsMonadicBind x
containsMonadicBind (AST.EConditionBranch x y z) = containsMonadicBind x || containsMonadicBind y || maybe False containsMonadicBind z
containsMonadicBind (AST.EWhile x y) = containsMonadicBind x || containsMonadicBind y
containsMonadicBind (AST.EInterface  {}) = False
containsMonadicBind (AST.ENativeFunction {}) = False
containsMonadicBind (AST.EType _ _) = False
containsMonadicBind (AST.EVariableDeclare {}) = False
containsMonadicBind (AST.ERequire _) = False
containsMonadicBind (AST.EInstanceAccess x _) = containsMonadicBind x
containsMonadicBind (AST.EInstanceDict _ _ xs) = any containsMonadicBind xs
containsMonadicBind (AST.EInstanceVariable _ _) = False
containsMonadicBind (AST.ETypeAlias _ _) = False
containsMonadicBind (AST.EAwait x) = containsMonadicBind x
containsMonadicBind (AST.EMutUpdate _ x y) = containsMonadicBind x || maybe False containsMonadicBind y
containsMonadicBind (AST.EList xs) = any containsMonadicBind xs
containsMonadicBind (AST.EApplication x xs) = containsMonadicBind x || any containsMonadicBind xs
containsMonadicBind (AST.EVariable _ _) = False
containsMonadicBind (AST.ELiteral _) = False
containsMonadicBind (AST.EClosure _ _ x _) = containsMonadicBind x
containsMonadicBind (AST.EDeclaration _ _ x xs) = containsMonadicBind x || maybe False containsMonadicBind xs
containsMonadicBind (AST.EInstanceDeclare {}) = False
containsMonadicBind (AST.ELetMatch _ e) = containsMonadicBind e
containsMonadicBind (AST.EDirectExtension _ _ xs) = any containsMonadicBindE xs
  where 
    containsMonadicBindE (AST.ExtDeclaration _ _ x) = containsMonadicBind x
containsMonadicBind (AST.ETypeExtension _ _ _ xs) = any containsMonadicBindE xs
  where 
    containsMonadicBindE (AST.ExtDeclaration _ _ x) = containsMonadicBind x
