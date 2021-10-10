% Alberto Garcia Domenech

% 1) Ejecuta y compara:

% ?- eliminai([a,b,a,c],a,L). (i= 1,2,3)
% ?- eliminai([a,b,a,c],X,L). (i= 1,2,3)

% (a) Usando igualdad sint´actica:
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

% elimina1([a,b,a,c]a,L). => [b,c]
% elimina1([a,b,a,c],X,L). => [a,b,a,c]     no hay nunca igualdad sintactica

% (b) Usando unificaci´on:
elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

% elimina2([a,b,a,c]a,L). => [b,c]
% elimina2([a,b,a,c],X,L). => [b,c], X=a        unifica solo con el primer elemento de la lista
% (c) Combinando las dos anteriores:
elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% elimina3([a,b,a,c]a,L). => [b,c]
% elimina3([a,b,a,c],X,L). => [b,c], X=a ;  L=[a,a,c], X=b; L=[a,b,c], X=a; L=[a,b,a], X=c; L = [a,b,a,c]


% 2)
% a) utilizando la estructura de arbol binario vista en clase
arbol(Elemento,Izq,Der).
% a) sumatree(A,N) <-> A es un arbol binario con num enteros en sus nodos y N es la suma de esos nodos 

sumatree(void,0).
sumatree((E,I,D),N) :- sumatree(I,N1), sumatree(D, N2), N is N1+N2+E.

% b) maximo(A,X) <-> A arbol binario y X es el elemento maximo de los nodos de A. X vale 0 si A es vacio
maximo(void,X) :- X is 0
maximo((E,I,D),X) :- maximo(I,X1), maximo(D,X2), (X1 > X2 -> N is X1; N is X2), (E > N -> X is E; X is N).

% 3) define sublistas/2 para conseguir todas las listas de una lista

sublistas([], []).
sublistas([X|Xs], [X|Ys]) :- sublistas(Xs, Ys).
sublistas([_|Xs], Ys) :- sublistas(Xs, Ys).

% 4) define una version recursiva para resolver problema de las torres de Hanoi 

% hanoi(N,A,B,C,M) -> N fichas en la torre inicial, A torre inicial, B torre final, C torre aux, M lista movimientos para mover todas las fichas

hanoi(1,A,_,C,[[A,Z]]).
% hanoi(N,A,B,C,M):- N1 is N-1, hanoi(N1,A,C,B,M1),