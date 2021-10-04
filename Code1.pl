% basic comparisons : < , > , >= , =< , =:= (negate last)

strictlyIncreasing(A,B,C):- A < B, B < C.

nonDecreasing(A,B,C) :- A =< B, B =< C.

% Y is 2, X is Y, \+ X =:= Y.


% try 30/4 , note floating point returned

%other ops available
%div(), also //, integer div., %mod
% note, basically a return (unusual)

% is vs =:= ( left hand of is can be unset for unificaiton)

quotient_remainder(X,Y,Q,R) :- Q is div(X,Y), R is mod(X,Y). 

quotient_remainder2(X,Y,Q,R) :- Q is X//Y , R is mod(X,Y).

%square function?
square(B,S) :- S is B^2.

%sqrt(N). Kind of pointless
square_root(S,SR) :- SR is sqrt(S).

%factorial
factorial(0,1).
factorial(N,F):-Nm1 is N-1,N>0,factorial(Nm1,Fm1),F is N*Fm1.

%so, what's power of a pow(A,P,A_to_the_P) ?
%yes ^ works in Prolog, but this is recursive...
pow(_,0,1).
pow(A,P,A2P):- P>0, Pm1 is P-1, pow(A,Pm1,A2Pm1),A2P is A*A2Pm1 .


%define with power(A,0,1) ?

%so, what's blocks world?

%loc(b1,x,y) screen-coordinate order
loc(b1,0,2).
loc(b2,0,3).
loc(b3,1,0).
loc(b4,1,1).
loc(b5,1,2).
loc(b6,1,3).
loc(b7,2,3).

%generator for blocks?
block(B):-loc(B,_,_).

on(Bt,Bb):-loc(Bt,X,Yt),Ytp1 is Yt+1,loc(Bb,X,Ytp1).

above(Bt,Bb):-loc(Bt,X,Yt),loc(Bb,X,Yb),Yb>Yt.

below(Bb,Bt):-above(Bt,Bb).

left(Bl,Br) :- loc(Bl,Xl,_),loc(Br,Xr,_),Xl<Xr.


%really important - Lists!

%note these examples:
% head_tail([1,2,3,4,5,6], H,T). , H is not list (in general), T is list
% head_tail([1], H,T). , T is empty list
% head_tail([], H,T). ,FAILS, H _must_ match at item and there's no
%     items...T
head_tail( [H|T],H,T).

%real predicate is length/2
len([],0).
len([_|T],Len):- len(T,LT), Len is LT +1.



%sum items in a list
sum([],0).
sum([N|Ns],S):-sum(Ns,SNs),S is N+SNs.


%adjacent equals?
adjacent_equals([H,H|_]).
adjacent_equals([_,H|T]):- adjacent_equals([H|T]).


%class is_sorted (non-decreasing) order, multiple base cases

is_sorted([]).
is_sorted([_]).
is_sorted([G,H|T]):-G=<H,is_sorted([H|T]).

%elem (real: member) -- test with M both set and unset
elem(M,[M|_]).
elem(M,[_|T]):- elem(M,T).

elem2(M,L) :- append( _, [M|_], L).

%concat (real, append)
%append([1,2,3], [4,5,6],C).
%append(A,B,[1,2,3,4,5,6]).
concat([],X,X).
concat( [H|C] , D , [H|T]) :- concat(C,D,T).



%real predicate is reverse
rev([],[]).
rev([H|T], RevTH) :- rev(T,RevT),append(RevT,[H],RevTH).


% rev2 (library, example of accumulator pattern, also define helper rule
% (always okay)


%subset -- multiple cases
subset1([],[]).
subset1(ST, [_|T] ):-subset1(ST,T).
subset1([H|ST],[H|T]):- subset1(ST,T).


%disjoint
disjoint1([], _).
disjoint1([_|_],[]).
disjoint1( [H|T],[G|S]) :- \+ H=G, disjoint1([H|T],S), disjoint1(T,S).


%disjoint2 using member, NO RECURSION (explicitly)
disjoint(A,B) :- \+ (member(M,A),member(M,B)).

%intersects
intersect(A,B) :- member(M,A),member(M,B).

intersection( [] , _ , []).
intersection([H|T],B,[H|I]):- member(H,B), intersection(T,B,I).
intersection([H|T],B,I):- \+ member(H,B), intersection(T,B,I).

%max rule
max(A,B,A):-A>B.
max(A,B,B):-A=<B.

%max of LIST
maxL([X],X).
maxL([H|T],Max) :- \+ [_]=[H|T],maxL(T,TMax),max(H,TMax,Max).

maxL2( [H|T] , Max):- maxL2Helper(T,H,Max).

maxL2Helper([X],Curr,Max) :- max(X,Curr,Max).
maxL2Helper([H,H2|T],Curr,Max):-H>Curr, maxL2Helper([H2|T],H,   Max) ; 
                                H=<Curr,maxL2Helper([H2|T],Curr,Max).

n_copies(_,0,[]).
n_copies(X,N,[X|Nm1_Xs] ):-N>0,Nm1 is N-1, n_copies(X,Nm1,Nm1_Xs).

%list_evil
list_evil([e,v,i,l|_]).
list_evil([_|T]):-list_evil(T).
%call following line:
% set_prolog_flag(answer_write_options,[max_depth(0)]).


%take
take(0, _ , []).
take(N,[H|T], [H|TT] ):- N>0, Nm1 is N-1,take(Nm1,T,TT). 

%drop
drop(0,L, L).
drop(N,[_|T], DT):-N>0, Nm1 is N-1, drop(Nm1,T,DT).

% split less efficient with take and drop
split(AB,A,B):-
    length(AB,Len),
    Half is Len//2,
    take(Half,AB,A),
    drop(Half,AB,B).

% more efficient version (take, in particular, computes and then
% discards the drop answer).
splitEff(AB,A,B):-
    length(AB,Len),
    Half is Len//2,
    takeDrop(Half,AB,A,B).

splitEff2(AB,A,B):-
    length(AB,Len),
    Half is Len // 2,
    length(A,Half),
    append(A,B,AB).



%note use of helper predicate, below
%split
takeDrop(0, L , [], L).
takeDrop(N,[H|T], [H|TT] , DT ):- N>0, Nm1 is N-1,takeDrop(Nm1,T,TT,DT).


%merge_lists

merge([],L,L).
merge(L,[],L).
merge([G|S],[H|T],[G|MergeSHT]):-
    G=<H, merge(S,[H|T] ,MergeSHT).
merge([G|S],[H|T],[H|MergeGST]):-
    G>H, merge([G|S], T ,MergeGST).

%what happens if I do <, >= (sort NOT stable - 350)

merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(L,SL):-
    L=[_,_|_],
    splitEff(L,L1,L2),
    merge_sort(L1,SL1),
    merge_sort(L2,SL2),
    merge(SL1,SL2,SL).
    

