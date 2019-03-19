%% Practice Problem for the final exam

% Here is a Prolog database that describes where different animals live and what they eat.

lives(zebra, savanna).     eats(zebra, grass).
lives(lion, savanna).      eats(lion, meat).
lives(sloth, forest).      eats(sloth, leaves).
lives(deer, forest).       eats(deer, grass).
lives(shark, ocean).       eats(shark, meat).

% 1. Write a query that returns the name and location of any animal that eats grass. This query should find two solutions: zebras in the savanna and deer in the forest.

% ?- lives(X,Y), eats(X,grass).
% X = zebra,
% Y = savanna ;
% X = deer,
% Y = forest ;
% false.

% 2. Define a predicate neighbor/2 that relates whether two animals live in the same place. For example, the query neighbor(deer,Y) should find Y=sloth as a solution.

neighbor(X,Y) :- lives(X,Z), lives(Y,Z), X\=Y.

% 3. Define a predicate prey/1 that indicates whether an animal is prey. An animal is prey if it is neighbors with an animal that eats meat.

prey(H,F) :- eats(H,meat), neighbor(H,F).

% Here is a Prolog database that describes a directed and color-coded teleportation network in terms of a predicate portal/3. 
% For example, the fact portal(2,red,4) means that we can move from position 2 to position 4 via a red portal, but not necessarily vice versa.

portal(1,blue,2).      portal(3,red,4).      portal(2,green,1).
portal(2,blue,3).      portal(4,red,1).      portal(3,green,2).
portal(3,blue,4).      portal(4,red,2).      portal(4,green,2).

% 4. Define a recursive predicate path/3 that relates whether two positions are connected by portals of a particular color. 
% For example, the query path(1,blue,4) should return true.

path(S,C,D) :- portal(S,C,D); portal(S,C,X), path(X,C,D).
% path(X,C,Y) :- X = Y.
% path(X,C,Y) :- portal(X,C,Z), path(Z,C,Y).

% 5. Using path/3, write a query that finds all of the colors for which a path exists from position 3 to position 1. Then list all solutions to this query.

% ?- path(3,Color,1).
% Color = red ;
% Color = green ;
% false.

% 6. Using path/3, write a query that finds all of the positions that can be reached from position 2 using only blue portals. Then list all solutions to this query.

% ?- path(2,blue,Dest).
% Dest = 3 ;
% Dest = 4 ;
% false.