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
siblingInLaw(X,Z) :- married(X,Y), sibling(Y,Z); sibling(X,Y), married(Y,Z).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.


% 8. Define the predicate `cousin/2`.


% 9. Define the predicate `ancestor/2`.


% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation (see course web page)
%%


% num	::=	(any number literal)	
% str	::=	(any string literal)	
% bool	::=	t   |   f	boolean literals
% prog	::=	cmd∗	sequence of commands
% cmd	::=	num   |   str   |   bool	push a literal onto the stack
%        |	add   |   lte	number addition/comparison
%        |	if(prog,prog)	conditional branching

% A literal number (e.g. 3), string (e.g. "Hi there!"), or boolean (i.e. t or f) is a command that simply pushes the corresponding value onto the stack.
% The add command pops two numbers off the stack and pushes their sum.
% The lte command pops two numbers off the stack and pushes t if the first number is the less than or equal to the second, otherwise it pushes f.
% An if(P1,P2) command pops a boolean value off the stack and executes either P1 if the value is t or P2 if the value is f.

% 1. Define the predicate cmd/3, which describes the effect of a command on the stack. That is, the predicate cmd(C,S1,S2) means that executing command C with stack S1 produces stack S2.


cmd(t,S,[t|S]).
cmd(f,S,[f|S]).
cmd(X,S,[X|S]) :- number(X).
cmd(X,S,[X|S]) :- string(X).

cmd(add,[X,Y|S], [Z|S]) :- Z is X+Y.
cmd(lte,[X,Y|S], [t|S]) :- X =< Y.
cmd(lte,[X,Y|S], [f|S]) :- X > Y.

cmd(if(P,_),[t|S],T)    :- prog(P,S,T).
cmd(if(_,P),[f|S],T)    :- prog(P,S,T).

% ?- cmd("hello",[4],S).
% S = ["hello", 4].

% ?- cmd(4,S,[4,"goodbye"]).
% S = ["goodbye"].

% ?- cmd(add,[2,3,4],S).
% S = [5, 4].

% ?- cmd(lte,[2,3,4],S).
% S = [t, 4].

% ?- cmd(lte,[5,3,t],S).
% S = [f, t].
% Note that I have not provided a test case for if yet since it depends on the prog/3 predicate below.

% 2. Define the predicate prog/3, which describes the effect of a program on the stack. That is, the predicate prog(P,S1,S2) means that executing program P with stack S1 produces stack S2.

prog([],S,S).
prog([A|B],S1,S2) :- cmd(A,S1,S), prog(B,S,S2).

% ?- prog([3,4,add],[],S).
% S = [7] .

% ?- prog([3,4,add,6,lte,9],[],S).
% S = [9, t] .

% ?- prog([if(["foo"],[3]),4],[t,5],S).
% S = [4, "foo", 5] .

% ?- prog([2,lte,if(["foo"],[3]),4],[1],S).
% S = [4, 3] .


