Definition f := fun n => n + 1.
Check f.

Definition f2: nat -> nat := fun n => n + 1.

Fail Definition f2 := fun n => n + 2.

Check f2. 

Eval compute in f 2.

Check f 2.