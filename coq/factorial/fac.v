Fixpoint factorial n :=
  match n with
  | 0 => 1
  | S m => n * factorial m
  end.

Eval compute in factorial 8.