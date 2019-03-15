%% Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

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



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(Y,X) :- parent(X,Y).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- child(X,Z), child(Y,Z), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y), male(X).
sister(X,Y) :- sibling(X,Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.


% 8. Define the predicate `cousin/2`.


% 9. Define the predicate `ancestor/2`.


% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation (see course web page)
%%
