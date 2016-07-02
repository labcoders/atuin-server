{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Atuin.Lua.Pretty where

import Prelude hiding ( break )

import Atuin.Lua hiding ( (<$$>) )

import Text.PrettyPrint.ANSI.Leijen

data Result :: Node -> * where
  PrettyExpr :: Doc -> Result ('Expr a)
  PrettyStmt :: Doc -> Result 'Stmt

result :: Result a -> Doc
result e = case e of
  PrettyExpr x -> x
  PrettyStmt x -> x

ind :: Doc -> Doc
ind = indent 2

prettyNat :: HFix LuaF :~> Result
prettyNat = hcata phi where
  phi :: LuaF Result :~> Result
  phi = Nat $ \e -> case e of
    Done -> PrettyStmt ""
    Break -> PrettyStmt "break"

    Block code next -> PrettyStmt $
      "do" <$$> ind (result code) <$$> "end" <$$> result next

pprint :: HFix LuaF a -> Doc
pprint = result . unNat prettyNat

test = pprint $
  block (
    break
  )
