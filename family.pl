% This is the Prolog version of the family example

child(john,sue).   child(john,sam).   
child(jane,sue).   child(jane,sam).   
child(sue,george).   child(sue,gina). 

male(john).   male(sam).     male(george). 
female(sue).  female(jane).  %female(june). 
female(gina).

parent(Y,X) :- child(X,Y).
father(Y,X) :- child(X,Y), male(Y).
opp_sex(X,Y) :- male(X), female(Y). 
opp_sex(Y,X) :- male(X), female(Y). 
grand_father(X,Z) :- father(X,Y), parent(Y,Z).

mother(M,C) :- female(M),parent(M,C).

person(P) :- child(P,_).
person(P) :- child(_,P).

all_gendered :- \+ ungendered(_).

%ungendered(P):-person(P),\+male(P), \+female(P).
ungendered(P):-person(P),\+(male(P);female(P)).

gender_but_not_person(Name):-male(Name),\+person(Name).
gender_but_not_person(Name):-female(Name),\+person(Name).
