-- generated by hosc0 from debug/add_machine.hs

data Term a = Lit a | Add (Term a) (Term a);
data Red a = Sum a a;
data Context a = Hole (Red a) | C1 (Context a) (Term a) | C2 a (Context a);
data Dec a = Val a | Ctx (Context a);

(letrec
  g=(\u24->
    case 
    (letrec
      h=(\v24->
        case  v24  of {
          Lit s3 -> (Val s3);
          Add y23 r13 ->
            case  (h y23)  of {
              Ctx x18 -> (Ctx (C1 x18 r13));
              Val t9 -> case  (h r13)  of { Ctx u19 -> (Ctx (C2 t9 u19)); Val v -> (Ctx (Hole (Sum t9 v))); };
            };
        })
    in
      (h u24))
     of {
      Ctx p4 ->
        case  p4  of {
          Hole s11 -> (Lit case  s11  of { Sum u2 p20 -> ((f u2) p20); });
          C1 u5 z2 ->
            (g
              (Add
                (letrec
                  f1=(\w24->
                    case  w24  of {
                      Hole v15 ->
                        (Lit
                          (letrec
                            g1=(\p24->
                              case  p24  of {
                                Hole p22 -> case  p22  of { Sum y3 z20 -> ((f y3) z20); };
                                C1 r16 u10 -> (g1 r16);
                                C2 y5 r21 -> (g1 r21);
                              })
                          in
                            (g1 u5)));
                      C1 t11 y6 -> (Add (f1 t11) y6);
                      C2 r12 y17 -> (Add (Lit r12) (f1 y17));
                    })
                in
                  (f1 u5))
                z2));
          C2 y18 p7 ->
            (g
              (Add
                (Lit y18)
                (letrec
                  h1=(\r24->
                    case  r24  of {
                      Hole v19 ->
                        (Lit
                          (letrec
                            f2=(\s24->
                              case  s24  of {
                                Hole s1 -> case  s1  of { Sum s4 y20 -> ((f s4) y20); };
                                C1 z24 p3 -> (f2 z24);
                                C2 s9 p21 -> (f2 p21);
                              })
                          in
                            (f2 p7)));
                      C1 y13 p -> (Add (h1 y13) p);
                      C2 p19 z15 -> (Add (Lit p19) (h1 z15));
                    })
                in
                  (h1 p7))));
        };
      Val r -> (Lit r);
    })
in
  (g t))