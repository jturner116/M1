set I := { 1..24 };
set J := { 1..24 };
param C := 9; #capacité

param t[I] := <1> 6, <2> 6, <3> 5, <4> 5, <5> 5, <6> 4, <7> 4, <8> 4, <9> 4,
        <10> 2, <11> 2, <12> 2, <13> 2, <14> 3, <15> 3, <16> 7, <17> 7, <18> 5,
        <19> 5, <20> 8, <21> 8, <22> 4, <23> 4, <24> 5 ;

var x[I*J] binary;
var y[J] binary;

minimize numbox: sum <j> in J: y[j];

subto box : forall <i> in I: sum <j> in J: x[i,j] <= 1;

subto dep1 : forall <j> in J:
     vif x[j] > 0 then y<j> == 1 end ;

subto dep2: forall <j> in J: sum <i> in I: t[i] * x[i,j] <= C;
