map([[22,  -,  9,  *],
     [ +,  4,  -, 18],
     [ 4,  *, 11,  *],
     [ *,  8,  -,  1]]).

pos(X-Y, V) :- map(M), nth1(Y, M, R), nth1(X, R, V).

adjacent(X-Y, X1-Y) :- X > 1, X1 is X - 1.
adjacent(X-Y, X1-Y) :- X < 4, X1 is X + 1.
adjacent(X-Y, X-Y1) :- Y > 1, Y1 is Y - 1.
adjacent(X-Y, X-Y1) :- Y < 4, Y1 is Y + 1.

path(N, P) :- path(N, 1-1, [22], P).

path(0, 4-4, P, P) :- !.
path(N, L, A, P) :-
    N > 0, N1 is N - 1,
    adjacent(L, L1),
    L \= 4-4,  % cannot enter the vault before the end
    L1 \= 1-1, % cannot return to the entrance
    pos(L1, V),
    path(N1, L1, [V|A], P).

value([22], 22).
value([X,O|P], V) :-
    value(P, V1),
    E =.. [O, V1, X],
    V is E.

vault(X) :-
    between(3, 100, N),
    N1 is N * 2,
    path(N1, P),
    value(P, 30), !,
    reverse(P, X).
