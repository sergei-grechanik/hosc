data Number = Z | S Number;

mul (mul x y) z where

add = \x y ->
    case x of {
      Z -> y;
      S x1 -> S (add x1 y);
    };

mul = \x y ->
    case x of {
      Z -> Z;
      S x1 -> add y (mul x1 y);
    };

