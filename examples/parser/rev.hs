data List a = Nil | Cons a (List a);

rev x 

where


rev = \xs -> case xs of {
    Nil -> Nil;
    Cons z  zs -> app (rev zs) (Cons z Nil);
  };

app = \xs ys -> case xs of {
      Nil -> ys;
      Cons z zs -> Cons z (app zs ys);
    };