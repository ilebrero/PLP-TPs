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

iesimo(0,[X|_],X).
iesimo(I,[_|Xs], Y) :- I \= 0, N is I-1, iesimo(N,Xs,Y).
iesimo2(I,L,X):- length(L,Long), Long2 is Long -1, between(0,Long2,I), iesimo(I,L,X).

% Ejercicio 3: cadena(?Cadena)

esCadena([]).
esCadena([E1|Exps]) :- symbol(E1), cadena(Exps).
cadena(L) :- length(L, _), esCadena(L).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([X],E) :- symbol(X), symbol(E), X=E.
match_inst([],empty).
match_inst(C,or(E1,E2)) :- match_inst(C, E1); match_inst(C,E2).
match_inst(C, concat(E1,E2)) :- length(C,L), append(C1,C2,C), match_inst(C1,E1), match_inst(C2,E2).
match_inst([], star(E)).
match_inst([C|Cs], star(E)) :- match_inst([C],E), match_inst(Cs,star(E)).

% Ejercicio 5: match(?Cadena, +RegEx)

match(C, empty):- C = [].
match(C, E) :- not(tieneEstrella(E)), longitudMaxima(E,M), between(0,M,L), length(C,L), match_inst(C,E).
match(C, E) :- tieneEstrella(E), length(C,L), match_inst(C,E).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(C, E1, E2) :- match(C,E1), not(match(C,E2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(P, C, E) :- esPrefijoValido(P,C,E), not(hayMasGrandes(P,C,E)).

esPrefijoValido(P,C,E) :- prefix(P,C), match_inst(P,E).

hayMasGrandes(P,C,E) :- esPrefijoValido(P2,C,E), P\=P2, length(P,L), length(P2,L2), L2> L.

/*prefijoMaximo(P,C,E) :- prefix(P,C), not(hayMasGrandes(P,C,E)).
hayMasGrandes(P,C,E) :- prefix(P2,C), P\=P2, length(P,L), length(P2,L2), L2> L.*/

% Ejercicio 8: reemplazar(+X, +R, +E, -Res)
% Remover hasta(+Cadena, +itemsARemover, -Res)

% Solo nos interesa que sea true si el prefijo no es vacio 
prefijoMaximo2(P, C, E) :- prefijoMaximo(P, C, E), P \= [].

% Se remueven los elementos a consumir si son reemplazables
removerHasta(C, [], C).
removerHasta([C|Cs], [T|Ts], Res) :- C == T, removerHasta(Cs, Ts, Res).

reemplazar([], R, T, []).
reemplazar(C, R, T, Res) :- prefijoMaximo2(Pref, C, R), removerHasta(C, Pref, ProxC), reemplazar(ProxC, R, T, Res1), append(T, Res1, Res).
reemplazar([C|Cs], R, E, Res) :- not(prefijoMaximo2(Pref, [C|Cs], R)), reemplazar(Cs, R, E, Res1), append([C], Res1, Res).
