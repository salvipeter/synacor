coins(X) :-
  permutation([2-red,3-corroded,5-shiny,7-concave,9-blue], X),
  X = [X1-_,X2-_,X3-_,X4-_,X5-_],
  X1 + X2 * X3^2 + X4^3 - X5 =:= 399, !.
