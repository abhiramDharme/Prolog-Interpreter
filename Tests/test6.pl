%add(X, Y, Z) :- Z = X / Y.
/* not(X, Y) :- X \= Y.
mem(a, b). */
add(X, Y, Z) :- Z = X + Y.
sub(X, Y, Z) :- Z = X - Y.
mul(X, Y, Z) :- Z = X * Y.
div(X, Y, Z) :- Z = X / Y.
mem(alice, bob).
mem(blue, bob).


edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).
edge(c, a).
path(X, X).
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

greater(X, Y) :- X >= Y.
lesser(X, Y) :- X <= Y.
pritesh(X, ret(X)).

fact(0, 1).
fact(X, Y) :-
        X > 0,
        Z = X - 1,
        fact(Z, W),
        Y =  W * X.