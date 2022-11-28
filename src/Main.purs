module Main where

import Prelude

import Data.Exists (Exists, mkExists)
import Data.List (List(..))
import Type.Proxy (Proxy(..))

-- Kind for type level natural numbers
data Nat
foreign import data N0 :: Nat
foreign import data NSucc :: Nat -> Nat

type N1 = NSucc N0
type N2 = NSucc N1
type N3 = NSucc N2
type N4 = NSucc N3
type N5 = NSucc N4

class Add (n :: Nat) (m :: Nat) (o :: Nat) | n m -> o
instance addZero :: Add N0 m N0
instance addSucc :: Add n m o => Add (NSucc n) m (NSucc o)

-- Term lists track the number of terms to match with predicate arity
data TermList (arity :: Nat) = TermList (List Term)

tNil :: TermList N0
tNil = TermList Nil

tCons :: forall (n :: Nat). Term -> TermList n -> TermList (NSucc n)
tCons term (TermList termlist) = TermList $ Cons term termlist

infixr 6 tCons as :

someTermList :: TermList N2
someTermList = Constant "a" : Variable "b" : tNil

type Time = Number

data Interval = Interval Time Time

data Term = Variable String | Constant String

data Predicate (arity :: Nat) = Predicate String

-- We need an existential type to match arity
data PredicateFormula (n :: Nat) = PredicateFormula (Predicate n) (TermList n)
type PredicateFormulaF = Exists PredicateFormula

data Formula = Formula PredicateFormulaF | Top | Bottom | BoxPlus Interval Formula

predicate :: forall (n :: Nat). Proxy n -> String -> Predicate n
predicate _ name = Predicate name

formula :: forall (n :: Nat). Predicate n -> TermList n -> Formula
formula pred termlist = Formula $ mkExists $ PredicateFormula pred termlist

p = predicate Proxy "p"

exFormula :: Formula
exFormula = formula p tNil

ex2Formula :: Formula
ex2Formula = formula p (Constant "t" : tNil)