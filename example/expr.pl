% A big-step operational semantics for the integer arithmetic langauge.

% Define the set of terms.
expr(N)        :- number(N).
expr(neg(E))   :- expr(E).
expr(add(L,R)) :- expr(L), expr(R).
expr(mul(L,R)) :- expr(L), expr(R).

% Big-step operational semantics.
reduce(N,N) :- number(N).
reduce(neg(E),Result) :- reduce(E,N), Result is -N.
reduce(add(L,R),Result) :- reduce(L,M), reduce(R,N), Result is M+N.
reduce(mul(L,R),Result) :- reduce(L,M), reduce(R,N), Result is M*N.
