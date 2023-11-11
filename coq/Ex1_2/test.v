Definition g x y := x + 2 * y.

Definition f := fun n => n + 1.

Definition g2 := fun x y => x + 2 * y.
Definition g3 := fun x => (fun y => x + 2 * y).

Check g.

Check g 1.

Eval compute in (g 1) 3.

Eval compute in g 1 3.


Definition repeat_twice f (x: nat) := f (f x).

Check repeat_twice.

Print f.


Eval compute in repeat_twice f 2.

Eval compute in repeat_twice f 2.