data Number = Z | S Number;

deepseq x x where

add = \x y ->
    case x of {
      Z -> y;
      S x1 -> S (add x1 y);
    };

force = \x -> deepseq x x;

deepseq = \x y -> case x of {
 Z -> y;
 S x1 -> deepseq x1 y;
};

