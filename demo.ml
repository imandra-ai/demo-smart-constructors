
let pp_hr() = Format.printf "@.%s@.@." (String.make 60 '#');;

pp_hr();;
print_endline "e2:";;
e2;;
print_endline "derive e2 with simplification:";;
derive "x" e2;;
print_endline "derive e2 without simplification:";;
simpl := false;;
derive "x" e2;;

pp_hr();;
simpl := true;;
print_endline "e3:";;
e3;;
print_endline "free vars of e3:";;
Aexpr.vars e3 |> Var_set.elements;;
print_endline "derive e3 with simplification:";;
derive "x" e3;;
print_endline "derive e3 without simplification:";;
simpl := false;;
derive "x" e3;;

pp_hr();;
simpl := true;;
Format.printf "eval e2 with substitution %a@." Subst.pp s1;;
Aexpr.eval s1 e2;;
print_endline "and now without simpl:";;
simpl := false;;
Aexpr.eval s1 e2;;

Format.printf "eval e3 with substitution %a@." Subst.pp s2;;
simpl := true;;
Aexpr.eval s2 e3;;
print_endline "and now without simpl:";;
simpl := false;;
Aexpr.eval s2 e3;;


pp_hr();;
simpl := true;;
print_endline "b1:";;
b1;;
Format.printf "eval b1 with substitution %a@." Subst.pp s2;;
simpl := true;;
Bexpr.eval s2 b1;;
print_endline "and now without simpl:";;
simpl := false;;
Bexpr.eval s2 b1;;


simpl := true;;
