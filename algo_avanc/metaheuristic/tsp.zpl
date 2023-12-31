set V := { read "tsp5.txt" as "<1n>" skip 1, <0>};
set E := { <i, j> in V * V with i < j };
set P[] := powerset(V);
set K := indexset(P);

param px[V] := read "tsp5.txt" as "<1n> 2n" skip 1, <0> 0;
param py[V] := read "tsp5.txt" as "<1n> 3n" skip 1, <0> 0;

defnumb dist(a, b) := sqrt((px[a]-px[b])^2 + (py[a]-py[b])^2);

var x[E] binary;

minimize cost: sum <i, j> in E: dist(i, j) * x[i, j];

subto two_connected: forall <v> in V do
    (sum <v, j> in E: x[v, j]) + (sum <i, v> in E: x[i, v]) == 2;

subto no_subtour:
    forall <k> in K with card(P[k]) > 2 and card(P[k]) < card(V) - 2 do
        sum <i, j> in E with <i> in P[k] and <j> in P[k]: x[i, j]
        <= card(P[k]) - 1;
