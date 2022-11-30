module Main where

import Prelude

import Data.Exists (Exists, mkExists)
import Data.List (List(..))
import Data.Natural (Natural, intToNat)
import Effect (Effect)
import Effect.Console (logShow)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

predicate :: forall (n :: Nat). Natural -> String -> Predicate n
predicate arity name = Predicate name

formula :: forall (n :: Nat). Predicate n -> TermList n -> Formula
formula pred termlist = Formula $ mkExists $ PredicateFormula pred termlist

p = predicate (intToNat 3) "p"


class NatToNatural :: Nat -> Constraint
class NatToNatural nat where
  reflectNat :: Proxy nat -> Natural

instance n0ToNatural :: NatToNatural N0 where
  reflectNat _ = intToNat 0

instance nsuccToNatural :: NatToNatural n => NatToNatural (NSucc n) where
  reflectNat _ = intToNat 1

test :: forall (n :: Nat). Proxy n -> Effect Unit
test proxy = logShow $ reflectNat proxy

main :: Effect Unit
main = do
  test (Proxy :: Proxy N1)

--class Reifies s a | s -> a where
--  reflect :: Proxy s -> a
--
--reify :: forall a r. a -> (forall s. Reifies s a => Proxy s -> r) -> r
--reify a f = coerce f { reflect: \_ -> a } Proxy where
--  coerce :: (forall s. Reifies s a => Proxy s -> r) -> { reflect :: Proxy Unit -> a } -> Proxy Unit -> r
--  coerce = unsafeCoerce
--
--instance reifiesN0Natural :: Reifies N0 Natural where
--  reflect Proxy = intToNat 0
--instance reifiesN1Natural :: Reifies N1 Natural where
--  reflect Proxy = intToNat 1

exFormula :: Formula
exFormula = formula p tNil

ex2Formula :: Formula
ex2Formula = formula p (Constant "t" : tNil)