# graph

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).

path(X, X).
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

greater(X, Y) :- X >= Y.
lesser(X, Y) :- X <= Y.
