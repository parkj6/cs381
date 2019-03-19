% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).


% Part 1. Family relations.

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y)  :- female(X), sibling(X,Y).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).

brotherInLaw(X,Y) :- male(X), siblingInLaw(X,Y).
sisterInLaw(X,Y)  :- female(X), siblingInLaw(X,Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- parent(Z,Y), sister(X,Z).
aunt(X,Y) :- parent(Z,Y), sisterInLaw(X,Z).
uncle(X,Y) :- parent(Z,Y), brother(X,Z).
uncle(X,Y) :- parent(Z,Y), brotherInLaw(X,Z).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(W,X), parent(Z,Y), sibling(W,Z).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

% Extra credit: Define the predicate `related/2`.
%
% This approach will not produce an infinite loop (since married/2 and
% by_blood/2 are both terminating), but it will also never stop finding
% solutions since the constraints don't enforce the absence of cycles
% of people who are related...

by_blood(X,Y) :- ancestor(X,Y).
by_blood(X,Y) :- ancestor(Y,X).
by_blood(X,Y) :- ancestor(Z,X), ancestor(Z,Y), X \= Y.

related(X,Y) :- by_blood(X,Y).
related(X,Y) :- married(Y,Z), by_blood(X,Z).
related(X,Y) :- married(X,Z), by_blood(Y,Z).
related(X,Y) :- married(A,B), by_blood(A,X), related(B,Y), X \= Y.
related(X,Y) :- married(A,B), by_blood(B,Y), related(A,X), X \= Y.


% Part 2. Language implementation

% 1. Define the predicate `cmd/3`.
cmd(t,S,[t|S]).
cmd(f,S,[f|S]).
cmd(X,S,[X|S]) :- number(X).
cmd(X,S,[X|S]) :- string(X).
cmd(add,[X,Y|S],[Z|S]) :- Z is X+Y.
cmd(lte,[X,Y|S],[t|S]) :- X =< Y.
cmd(lte,[X,Y|S],[f|S]) :- X > Y.
cmd(if(P,_),[t|S],T)   :- prog(P,S,T).
cmd(if(_,P),[f|S],T)   :- prog(P,S,T).

% 2. Define the predicate `prog/3`.
prog([],S,S).
prog([C|P],S,U) :- cmd(C,S,T), prog(P,T,U).
