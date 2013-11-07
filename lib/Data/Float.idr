module Data.Float

import Prelude
-- Hide some constants in seperate namespace

%public
infinity : Float
infinity = 1/0

nan : Float
nan = 0 / 0

-- machine epsilon
epsilon : Float
epsilon = 2.22e-16 

-- smallest value greater zero that can be represented
float_min : Float
float_min = 2.2250738585072014e-308

e : Float
e = 2.7182818284590452354

pi : Float
pi = 3.14159265358979323846


