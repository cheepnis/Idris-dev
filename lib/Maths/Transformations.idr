module Maths.Transformations

import Prelude.Vect

-- A transformation on a finite set can be defined by giving the
-- images of the points in an array
-- We make the number of points we act on implicit 
--
-- The type system makes sure that our images are in range.
--
-- XXX: Would it make sense to make the number of points acted on
--      explicit?

data Transformation : {k : Nat} -> Type where
    MkTrans : {k : Nat} -> Vect k (Fin k) -> Transformation {k}

-- Transformations act
apply : Fin k -> Transformation {k=k} -> Fin k
apply x (MkTrans t) = index x t

multiply : {k : Nat} 
        -> Transformation {k=k}
        -> Transformation {k=k}
        -> Transformation {k=k}
multiply {k = S m} (MkTrans t1) t2 = MkTrans (map (\x => apply x t2) t1)
multiply {k = Z} (MkTrans []) (MkTrans []) = MkTrans []

instance Semigroup (Transformation {k}) where
    l <+> r = multiply l r

multiplyIsAssoc : (a,b,c : Transformation {k}) -> a <+> (b <+> c) = (a <+> b) <+> c




-- For any given k the values of type Transformation k form
-- the full transformation monoid on k points


-- This seems like a convenient thing to do, don't know how to make it
-- nicer
syntax "(" [t] ")" = MkTrans t
