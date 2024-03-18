module Plume.Compiler.Desugaring.Modules.ANF where

import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Compiler.TypeErasure.Syntax qualified as Pre

desugarANF :: DesugarModule Pre.UntypedExpr (ANFResult Post.DesugaredExpr)
desugarANF f (Pre.UEApplication x xs) = do
  x' <- f x
  xs' <- mapM f xs
  let x'' = fst x'
  let xs'' = map fst xs'

  fresh <- freshName
  let stmts' = concatMap snd xs' <> snd x' <> [Post.DSDeclaration fresh x'']

  return (Post.DEApplication fresh xs'', stmts')
desugarANF f (Pre.UEDeclaration name expr body) = do
  (expr', stmt1) <- f expr
  (body', stmts2) <- desugarANF f body

  fresh <- freshName

  let stmts =
        stmt1
          <> stmts2
          <> [Post.DSDeclaration name expr', Post.DSDeclaration fresh body']

  return (Post.DEVar fresh, stmts)
desugarANF _ _ = error "test"