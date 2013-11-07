module Prelude.Float

import Builtins
import Prelude.Nat

%access public
%default total

%include C "math.h"
%lib C "m"

pow : (Num a) => a -> Nat -> a
pow x Z = 1
pow x (S n) = x * (pow x n)

exp : Float -> Float
exp x = prim__floatExp x

log : Float -> Float
log x = prim__floatLog x

sin : Float -> Float
sin x = prim__floatSin x

cos : Float -> Float
cos x = prim__floatCos x

tan : Float -> Float
tan x = prim__floatTan x

asin : Float -> Float
asin x = prim__floatASin x

acos : Float -> Float
acos x = prim__floatACos x

atan : Float -> Float
atan x = prim__floatATan x

atan2 : Float -> Float -> Float
atan2 y x = atan (y/x)

sinh : Float -> Float
sinh x = (exp x - exp (-x)) / 2

cosh : Float -> Float
cosh x = (exp x + exp (-x)) / 2

tanh : Float -> Float
tanh x = sinh x / cosh x

sqrt : Float -> Float
sqrt x = prim__floatSqrt x

floor : Float -> Float
floor x = prim__floatFloor x

ceiling : Float -> Float
ceiling x = prim__floatCeil x

