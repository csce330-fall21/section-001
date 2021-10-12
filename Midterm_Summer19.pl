
%q2
l(c1,c2). l(c2,c3). l(c3,c4).
l(c4,c5). l(c5,c6). l(c6,c7).

left(X,Y):-l(X,Y).
left(X,Z):-l(X,Y),left(Y,Z).

pass_to(c7,c1).
pass_to(X,Y):-l(X,Y). % immediately left is the same as pass_to, excepting c7 to c1

n(c1,1). n(c2,2). n(c3,3). 
n(c4,4). n(c5,5). n(c6,6). n(c7,7).

left1(X,Y):-n(X,NX),n(Y,NY),NX<NY.

pass_to2(c7,c1).
pass_to2(X,Y) :- n(X,NX),n(Y,NY), NY =:= N+1.

%q3
color(red). color(blue). color(green). color(yellow).

colorCA(M,G,B,E,H,N,CR,P,C):-
    color(M),  C=M,
    color(B),  B \= M,
    color(G),  G \= M, G \= B,
    color(H),  H \= M, H \= G,
    color(E),  E \= M, E \= H, E \= G,
    color(N),  N \= M, N \= H,   
    color(CR), CR \= M, CR \= N,
    color(P),  P \= M, P \= CR.

%q4
%EXAMPLE tree
node(a,b,c).
node(b,d,e).
node(c,f,nil).
node(d,nil,nil).
node(e,nil,g).

parent(P,C) :- node(P,C,_).
parent(P,C) :- node(P,_,C).

descendant(D,A):-parent(A,D),\+ D=nil.
descendant(D,A):-parent(A,X), descendant(D,X),\+ D=nil.

%q5
flatten1( [], []).
flatten1( [L|Ls] , Out) :- 
    flatten1(Ls,FLs),
    append(L,FLs,Out).