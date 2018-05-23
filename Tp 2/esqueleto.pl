symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a(a|b))*
regexEj(5, or(star(E1), E4)) :- regexEj(1, E1), regexEj(4, E4). % (a*|(a(a|b))*)
regexEj(6, star(or(a, ab))). %(a|ab)*
regexEj(7, concat(or(a, concat(a,b)), or(b, empty))). %(a|ab)(b|)
regexEj(8, concat(star(a), star(b))). %a*b*
regexEj(9, star(or(star(a), star(b)))).


% Ejercicio 1: tieneEstrella(+RegEx)
tieneEstrella(star(_)).
tieneEstrella(or(E1,E2)) :- tieneEstrella(E1); tieneEstrella(E2).
tieneEstrella(concat(E1,E2)) :- tieneEstrella(E1); tieneEstrella(E2).

% Ejercicio 2: longitudMaxima(+RegEx, -Length)

longitudMaxima(E, N) :- symbol(E), N is 1.
longitudMaxima(concat(E1,E2),N) :- longitudMaxima(E1, N1), longitudMaxima(E2, N2), N is N1+N2.
longitudMaxima(or(E1,E2),N) :- longitudMaxima(E1,N1), longitudMaxima(E2,N2), N is max(N1,N2).

%% iesimo(0,[X|_],X).
%% iesimo(I,[_|Xs], Y) :- I \= 0, N is I-1, iesimo(N,Xs,Y).
%% iesimo2(I,L,X):- length(L,Long), Long2 is Long -1, between(0,Long2,I), iesimo(I,L,X).

% Ejercicio 3: cadena(?Cadena)

esCadena([]).
esCadena([E1|Exps]) :- symbol(E1), cadena(Exps).
cadena(L) :- length(L, _), esCadena(L).

% Ejercicio 4: match_inst(+Cadena, +RegEx)
hasta(X, 0). 
hasta(X, Y):- X is Y-1, hasta(_, X).

generar(empty,[],0).
generar(E,X,_) :- symbol(E), append([],[E],R), X = R.
generar(or(E1,E2), X, L) :- generar(E1,X1,L), X = X1 ; generar(E2,X2,L), X = X2.
generar(concat(E1,E2),X,L) :- L>0, between(0,L,L1), generar(E1,X1,L1), L2 is L-L1, generar(E2,X2,L2), append(X1,X2,X).
generar(star(E1),X,L) :- length(X,L), (generar(empty,X,L); generar(concat(E1,star(E1)),X,L)).

generar(empty,[],0).
generar(E,X,_) :- symbol(E), append([],[E],R), X = R.
generar(or(E1,E2), X, L) :- generar(E1,X1,L), X = X1 ; generar(E2,X2,L), X = X2.
generar(concat(E1,E2),X,L) :- L>0, generar(E1,X1,L), M is L-1, generar(E2,X2,M), append(X1,X2,X).
generar(star(E1),X,L) :- length(X,L), (generar(empty,X,L); generar(concat(E1,star(E1)),X,L)).

match_inst(C, E) :- length(C,L), generar(E,C,L).

match_inst(C, E) :- length(C,L), generar(E,C,L).


% Ejercicio 5: match(?Cadena, +RegEx)
match(_, _) :- fail.

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(_, _, _) :- fail.

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(_, _, _) :- fail.

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(_, _, _, _) :- fail.
