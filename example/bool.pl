% A big-step operational semantics for a boolean expression language.

% Define the set of terms (i.e. syntax).
%
% Note: this is not really necessary, but nice for clarifying exactly
% what you expect to be in the language.
expr(tru).
expr(fls).
expr(not(E))   :- expr(E).
expr(and(L,R)) :- expr(L), expr(R).
expr(or(L,R))  :- expr(L), expr(R).

% Big-step operational semantics.
reduce(tru,tru).
reduce(fls,fls).
reduce(not(E),tru) :- reduce(E,fls).
reduce(not(E),fls) :- reduce(E,tru).
reduce(and(L,R),tru) :- reduce(L,tru), reduce(R,tru).
reduce(and(L,R),fls) :- reduce(L,fls), expr(R).
reduce(and(L,R),fls) :- reduce(R,fls), expr(L).
reduce(or(L,R),fls) :- reduce(L,fls), reduce(R,fls).
reduce(or(L,R),tru) :- reduce(L,tru), expr(R).
reduce(or(L,R),tru) :- reduce(R,tru), expr(L).
