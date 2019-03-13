% Some facts about Hobbits

hobbit(bilbo, rivendell).
hobbit(frodo, hobbiton).
hobbit(sam, hobbiton).
hobbit(merry, buckland).
hobbit(pippin, tookland).

hobbit(X) :- hobbit(X,_).

likes(sam,frodo).
likes(frodo,sam).
likes(merry,pippin).
likes(pippin,merry).

likes(bilbo,ring).
likes(frodo,ring).
likes(gollum,ring).

likes(X,beer) :- hobbit(X).
likes(X,boats) :- hobbit(X,buckland).

likesEachOther(X,Y) :- likes(X,Y), likes(Y,X).

danger(X) :- likes(X,ring).
danger(X) :- likes(X,boats), likes(X,beer).
