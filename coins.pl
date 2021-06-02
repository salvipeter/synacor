coins(X) :-
  permutation([2,3,5,7,9], X),
  X = [X1,X2,X3,X4,X5],
  X1 + X2 * X3^2 + X4^3 - X5 =:= 399.

