data List a = Nil | Cons a (List a);

x where

app = \xs ys ->
    case xs of {
      Nil -> ys;
      Cons z z -> Cons z ys;
    };