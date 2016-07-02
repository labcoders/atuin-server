{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Atuin.Lua where

import Prelude hiding (id, (.))
import Control.Category
import Data.Text ( Text )
-- import Control.Monad.Free

type LuaText = Text

data Node
  = Stmt
  | Expr ValueSide

data LuaF :: (Node -> *) -> Node -> * where
  -- statements
  Assignment
    :: Locality -- ^ Whether the assignment is local or global
    -> [next ('Expr 'LValue)] -- ^ The variables to assign to
    -> [next ('Expr a)] -- ^ The expressions to assign
    -> next 'Stmt -- ^ What to do next
    -> LuaF next 'Stmt

  Block
    :: next 'Stmt -- ^ The contents of the block
    -> next 'Stmt -- ^ What to do next
    -> LuaF next 'Stmt

  While
    :: next ('Expr a) -- ^ The condition of the while loop
    -> next 'Stmt -- ^ The body of the while loop
    -> next 'Stmt -- ^ What to do after the loop
    -> LuaF next 'Stmt

  ForNumeric
    :: Identifier -- ^ The index of the loop
    -> next ('Expr a) -- ^ The initial value of the index
    -> next ('Expr b) -- ^ The final value of the index
    -> Maybe (next ('Expr c)) -- ^ The step value of the index
    -> next 'Stmt -- ^ The body of the for loop
    -> next 'Stmt -- ^ What to do after the loop
    -> LuaF next 'Stmt

  ForGeneric
    :: Identifier -- ^ The index of the loop
    -> next ('Expr a) -- ^ The iterator of the loop
    -> next 'Stmt -- ^ The body of the loop
    -> next 'Stmt -- ^ What to do after the loop
    -> LuaF next 'Stmt

  IfThenElse
    :: next ('Expr a) -- ^ The condition of the if-statement
    -> next 'Stmt -- ^ The then clause
    -> Maybe (next 'Stmt) -- ^ The else clause
    -> next 'Stmt -- ^ What to do after the if-statement
    -> LuaF next 'Stmt

  -- Not allowed to do anything after a break in lua.
  Break :: LuaF next 'Stmt

  Return
    :: Maybe (next ('Expr a)) -- ^ The value to return, if any
    -> LuaF next 'Stmt

  -- expressions
  Addition
    :: next ('Expr a)
    -> next ('Expr b)
    -> LuaF next ('Expr 'RValue)

  Subtraction
    :: next ('Expr a)
    -> next ('Expr b)
    -> LuaF next ('Expr 'RValue)

  Multiplication
    :: next ('Expr a)
    -> next ('Expr b)
    -> LuaF next ('Expr 'RValue)

  Division
    :: next ('Expr a)
    -> next ('Expr b)
    -> LuaF next ('Expr 'RValue)

  -- preserve the binding status of expressions if you index into it; binders
  -- remain binds, and non-binding expressions remain non-binding
  Index
    :: next ('Expr a)
    -> Text
    -> LuaF next ('Expr a)

  Function
    :: [Identifier]
    -> Ellipsis
    -> next 'Stmt
    -> LuaF next ('Expr 'RValue)

  Done :: LuaF next 'Stmt

  -- atomic values

  Ref :: Text -> LuaF next ('Expr 'LValue)

  Number :: Double -> LuaF next ('Expr 'RValue)

  String :: Text -> LuaF next ('Expr 'RValue)

  Bool :: Bool -> LuaF next ('Expr 'RValue)

  Nil :: LuaF next ('Expr 'RValue)

  Array :: [next ('Expr a)] -> LuaF next ('Expr 'RValue)

  Object :: [(Identifier, next ('Expr a))] -> LuaF next ('Expr 'RValue)

data ValueSide
  = LValue
  | RValue

data Locality
  = Local
  | Global

data Ellipsis
  = Ellipsis
  | NoEllipsis

type Identifier = Text

newtype HFix h a = HFix { unHFix :: h (HFix h) a }

-- | Natural transformation.
newtype f :~> g = Nat { unNat :: forall a. f a -> g a }

instance Category (:~>) where
  id = Nat $ \x -> x
  f . g = Nat (unNat f . unNat g)

-- | Higher-order functors parameterized by a category.
class HFunctor hom h where
  hfmap :: a `hom` b -> h a `hom` h b

infixl 4 <$$>

-- | Operator form of 'hfmap'.
(<$$>) :: HFunctor hom h => a `hom` b -> h a `hom` h b
(<$$>) = hfmap

-- | Higher-order applicative functors parametrized by a category.
class HFunctor hom h => HApplicative hom h where
  hpure :: a `hom` h a
  hap :: h (a `hom` b) -> h a `hom` h b

infixl 4 <**>

-- | Operator form of 'hap'.
(<**>) :: HApplicative hom h => h (a `hom` b) -> h a `hom` h b
(<**>) = hap

-- | Higher-order monads parameterized by a category.
class HFunctor hom h => HMonad hom h where
  hbind :: (f `hom` h g) -> (h f `hom` h g)

infixl 1 ==<<

-- | Operator form of 'hbind'.
(==<<) :: HMonad hom h => (a `hom` h b) -> (h a `hom` h b)
(==<<) = hbind

instance HFunctor (:~>) LuaF where
  hfmap (Nat f) = Nat $ \e -> case e of
    Assignment l vars exprs next ->
      Assignment l (f <$> vars) (f <$> exprs) (f next)

    Done -> Done

    Break -> Break

    Block code next -> Block (f code) (f next)

    _ -> error "blah"

type Lua = HFix LuaF

type LuaProgram = Lua 'Stmt
type LuaExpr a = Lua ('Expr a)

-- | Sequence Lua statements.
--
-- /O(n)/ in the number of statements in the left operand.
--
-- /Remark:/ use @RebindableSyntax@ setting @(>>) = (&)@ to take advantage of
-- @do@-notation.
--
-- /Warning:/ in Lua, it is syntactically invalid to put statements after a
-- @break@ or @return@ statement. Trying to append a statement to a sequence
-- ending in such a statement is a no-op in this function! (Indeed, it is
-- impossible to implement given the definition of 'Break' and 'Return'.)
(&) :: LuaProgram -> LuaProgram -> LuaProgram
(HFix e) & code = HFix $ case e of
  Done -> unHFix code
  Assignment l x y next -> Assignment l x y (next & code)
  Block inner next -> Block inner (next & code)
  While expr inner next -> While expr inner (next & code)
  ForNumeric i a b c inner next -> ForNumeric i a b c inner (next & code)
  ForGeneric i x inner next -> ForGeneric i x inner (next & code)
  IfThenElse expr inner other next -> IfThenElse expr inner other (next & code)
  Break -> Break
  Return x -> Return x
infixr 1 &

-- Now the actual EDSL commands

-- | Helper for manufacturing statement primitives.
stmt :: (LuaProgram -> h (HFix h) a) -> HFix h a
stmt = HFix . ($ done)

-- | End of a chunk of code.
done :: LuaProgram
done = HFix Done

-- | General assignment.
assign :: Locality -> [LuaExpr 'LValue] -> [LuaExpr a] -> LuaProgram
assign l vars exprs = stmt $ Assignment l vars exprs

-- | Multiple local assignment.
(.==) :: [LuaExpr 'LValue] -> [LuaExpr a] -> LuaProgram
vars .== exprs = assign Local vars exprs
infixl 2 .==

-- | Shortcut for single local assignment
(.=) :: LuaExpr 'LValue -> LuaExpr a -> LuaProgram
v .= expr = [v] .== [expr]
infixl 2 .=

-- | A block of code.
block :: LuaProgram -> LuaProgram
block code = stmt $ Block code

-- | A while loop.
while :: LuaExpr a -> LuaProgram -> LuaProgram
while expr code = stmt $ While expr code

-- | Numeric for loop.
forN
  :: Identifier
  -> LuaExpr a
  -> LuaExpr b
  -> Maybe (LuaExpr c)
  -> LuaProgram
  -> LuaProgram
forN i e1 e2 e3 code = stmt $ ForNumeric i e1 e2 e3 code

-- | Generic for loop.
forG :: Identifier -> LuaExpr a -> LuaProgram -> LuaProgram
forG i e code = stmt $ ForGeneric i e code

break :: LuaProgram
break = HFix Break

ret :: Maybe (LuaExpr a) -> LuaProgram
ret x = HFix $ Return x

-- EDSL expression combinators

(!+) :: LuaExpr a -> LuaExpr b -> LuaExpr 'RValue
x !+ y = HFix $ Addition x y
infixl 6 !+

(!-) :: LuaExpr a -> LuaExpr b -> LuaExpr 'RValue
x !- y = HFix $ Subtraction x y
infixl 6 !-

(!*) :: LuaExpr a -> LuaExpr b -> LuaExpr 'RValue
x !* y = HFix $ Multiplication x y
infixl 7 !*

(!/) :: LuaExpr a -> LuaExpr b -> LuaExpr 'RValue
x !/ y = HFix $ Division x y
infixl 7 !/

(!) :: LuaExpr a -> Text -> LuaExpr a
e ! i = HFix $ Index e i

function :: [Identifier] -> Ellipsis -> LuaProgram -> LuaExpr 'RValue
function is ell code = HFix $ Function is ell code

function' :: [Identifier] -> LuaProgram -> LuaExpr 'RValue
function' is code = function is NoEllipsis code

functionV :: [Identifier] -> LuaProgram -> LuaExpr 'RValue
functionV is code = function is Ellipsis code

functionDecl :: Identifier -> [Identifier] -> LuaProgram -> LuaProgram
functionDecl name is code = var name .= function' is code

var :: Text -> LuaExpr 'LValue
var t = HFix $ Ref t

str :: Text -> LuaExpr 'RValue
str t = HFix $ String t

num :: Double -> LuaExpr 'RValue
num x = HFix $ Number x

bool :: Bool -> LuaExpr 'RValue
bool x = HFix $ Bool x

true :: LuaExpr 'RValue
true = bool True

false :: LuaExpr 'RValue
false = bool False

nil :: LuaExpr 'RValue
nil = HFix Nil

array :: [LuaExpr a] -> LuaExpr 'RValue
array xs = HFix $ Array xs

table :: [(Identifier, LuaExpr a)] -> LuaExpr 'RValue
table xs = HFix $ Object xs

-- instance Functor f => Functor (Free' f) where
--   fmap f e = case e of
--     Pure x -> Pure (f x)
--     Free' free -> Free' $ fmap (fmap f) free

-- Now let's try higher-order free monads

-- Free :: (* -> *) -> (* -> *)
-- so HFree blows (* -> *) up to ((* -> *) -> * -> *)
-- giving ((* -> *) -> * -> *) -> ((* -> *) -> * -> *)
-- whew

hcata
  :: HFunctor (:~>) f
  => f t :~> t
  -> HFix f :~> t
hcata hphi@(Nat phi) = Nat $ phi . unNat (hfmap (hcata hphi)) . unHFix

-- pretty :: HFix LuaF a -> Doc
