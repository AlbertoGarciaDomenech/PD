% Alberto Garcia Domenech Laboratorio sesion 6 2020/2021
cima(d).
sobre(d,c).
sobre(c,b).
sobre(b,a).


cima(g).
sobre(g,f).
sobre(f,e).


cima(i).
sobre(i,h).


izquierda(c,g).

izquierda(b,f).
izquierda(f,i).

izquierda(a,e).
izquierda(e,h).

por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

por_encima_de_ERROR(X,Y) :- sobre(X,Y).
por_encima_de_ERROR(X,Y) :- por_encima_de_ERROR(X,Z), sobre(Z,Y).

pila_izquierda(X,Y) :- izquierda(X,Y).				% misma altura
pila_izquierda(X,Y) :- izquierda(Z,Y), por_encima_de(X,Z).	% X mas arriba que Y
pila_izquierda(X,Y) :- izquierda(X,Z), por_encima_de(Y,Z).	% X mas abajo que Y

por_arriba(X,[]) :- cima(X).
por_arriba(X,[Y|L]) :- sobre(Y,X), por_arriba(Y,L).

poner_encima(X,Y) :- cima(X), cima(Y), pilas_contiguas(X,Y).

pilas_contiguas(X,Y) :- pila_izquierda(X,Y).
pilas_contiguas(X,Y) :- pila_izquierda(Y,X).



% ESTADO DE LAS PILAS

%   d
%   c   g
%   b   f   i
%   a   e   h

% 1)
% ?- por_encima_de(X,c).       =>  X=d
% ?- por_encima_de(c,X).       => X=b  X=a
% ?- por_arriba(b,X).          => X=[c,d]
% ?- por_arriba(X,Y).          => X = d,Y = []  X = g,Y = []    X = i,Y = []    X = c,Y = [d]   X = b,Y = [c, d]    X = a,Y = [b, c, d] X = f,Y = [g]   X = e,Y = [f, g]    X = h,Y = [i]
% ?- poner_encima(X,f).        => false
% ?- por_encima_de(X,Y), cima(Y)    => false    (no hay nada que este por encima de la cima)
% ?- cima(Y), pila_izquierda(X,Y), cima(X). => X=d,Y=g  X=g Y=i (las cimas de las pilas izq y der)
% ?- por_arriba(a,X), member(Y,X), por_encima_de(Z,Y).  => X=[b,c,d],Y=b,Z=c    X=[b,c,d],Y=b,Z=d   X=[b,c,d],Y=c,Z=d

% Escribe un predicado mas_por_encima_que(X,Y) para expresar que X tiene mas fichas por encima que Y

longitud([],0).
longitud([_|Xs, N]) :- longitud(Xs,N1), N is N1+1.

mas_por_encima_que(X,Y) :- por_arriba(X,Xlist), por_arriba(Y,Ylist), longitud(Xlist,N1), longitud(Ylist,N2), (N1>N2).


% 2)
% Define un predicado Prolog mezcla(L1,L2,L) <-> L1 y L2 son listas y L es resultado de intercalar L1 y L2 y cuya longitud es el de la menor lista

mezcla([],[_],[]).
mezcla([_],[],[]).
mezcla([L1|L1s],[L2|L2s],[L1,L2|Z]) :- mezcla(L1s,L2s,Z).


% 3) Sean L1 y L2 listas
% a) Define un predicado para determinar si L1 es una sublista de L2. Es decir, todos los elementos de L1 aparecen consecutivamente en L2 y en el mismo orden.
sublista(L1,L2):- append(L1,X,L2).      %Si existe una lista X que concatenada a L1 es L2
% b)Define un predicado para determinar si L1 est´a contenida en L2. Es decir, cada elemento de L1 es un elemento de L2.
contenido([],_).
contenido([L1|Ls],L2) :- member(L1,L2), contenido(Ls,L2).

% 4)Define un predicado que calcule el n´umero de nodos de un ´abol binario, con aritm´etica de Peano
% definicion nodo: tiene un arbol con el hijo izq, der y su valor de nodo (X) y tiene un entero representando la cantidad de nodos de sus hijos más la suya

nodo(void,0).
nodo(arbol(X,I,D),1+N1+N2):- nodo(I,N1), nodo(D,N2).