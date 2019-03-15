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
male(test).


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

%%%%%%%%%%%%%%%%%%%
parent(herb, test).




%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
 child(X,Y):- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
 isMother(X):- female(X), parent(X,_).
 isFather(X):- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
 grandparent(X,Y):- parent(X,N), parent(N,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
 sibling(A,B):- parent(P,A), parent(P,B), A\=B.

% 5. Define two predicates `brother/2` and `sister/2`.
 brother(X,Y):- sibling(X,Y), male(X).
 sister(X,Y):- sibling(X,Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%   a sibling or the sibling of a spouse.
 siblingInLaw(X,Y):- married(X,A), sibling(A,Y) ; sibling(X, B), married(B, Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
 aunt(X,Y):- sister(X,A), parent(A,Y) ; parent(A,Y), siblingInLaw(X,A), female(X).
 uncle(X,Y):- brother(X,A), parent(A,Y); parent(A,Y), siblingInLaw(X,A), male(X).

% 8. Define the predicate `cousin/2`.
 cousin(X,Y):- parent(A,X), sibling(A,B), parent(B,Y); parent(A,X), sibling(A,B), married(B, C), parent(C, Y).

% 9. Define the predicate `ancestor/2`.
 ancestor(X,Y):- parent(X,Y) ; parent(X,A), ancestor(A,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation (see course web page)
%%
