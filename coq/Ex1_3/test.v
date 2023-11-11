Check true.
Check false.


Definition et a b :=
    match a with 
    | true => b
    | false => false
    end.


Print et.


Set Printing All.

Print et.

Unset Printing All.

Eval compute in et true true.

(* Definition et_non_exhaustive a b :=
    match a with 
    | true => b
    end. *)

Check andb false true.
Check orb false true.
Check negb false.

Eval compute in andb true true.

Print nat_ind.

Set Printing All.
Check 16.
Unset Printing All.
Check 17.