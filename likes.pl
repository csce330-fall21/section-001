%  This is a program about who likes what kinds of food.
likes(john,pizza).                   % John likes pizza.
likes(john,sushi).                   % John likes sushi.
likes(mary,sushi).                   % Mary likes sushi.
likes(paul,X) :- likes(john,X).      % Paul likes what John likes.
likes(_,icecream).                   % Everybody likes ice cream.
likes(X,calzone) :- likes(X,pizza). 
likes(X,cilantro) :- person(X), \+ X = mary.

person(john).
person(paul).
person(mary).

food(pizza). food(calzone).
food(sushi). food(icecream).

dislikes(P,F) :- person(P),food(F),\+ likes(P,F). 
