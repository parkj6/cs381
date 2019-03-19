% Illustrate the effect of a cut.
foo(1).
foo(2).
bar(X,Y) :- foo(X), !, foo(Y).
bar(3,3).

% Green cut.
max(X,Y,Y) :- X < Y, !.
max(X,Y,X) :- X >= Y.

% Red cut.
find(X,[X|_]) :- !.
find(X,[_|L]) :- find(X,L).

member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

% Negation as failure.
not(P) :- P, !, fail.
not(_).

% Illustrating negation
hobbit(frodo).
hobbit(sam).
hobbit(merry).
hobbit(pippin).

likes(frodo,ring).
likes(X,beer) :- hobbit(X), not(likes(X,ring)).
