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
match_inst(C, concat(E1,E2)) :- length(C,_), append(C1,C2,C), match_inst(C1,E1), match_inst(C2,E2).
match_inst([], star(_)).
match_inst(C, star(E)) :-append(C1,C2,C), match_inst(C1,E), length(C, L), not(length(C2, L)), match_inst(C2,star(E)).

% Ejercicio 5: match(?Cadena, +RegEx)

match(C, empty):- C = [].
match(C, E) :- not(tieneEstrella(E)), longitudMaxima(E,M), between(0,M,L), length(C,L), match_inst(C,E).
match(C, E) :- tieneEstrella(E), length(C,_), match_inst(C,E).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(C, E1, E2) :- match(C,E1), not(match(C,E2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(P, C, E) :- esPrefijoValido(P,C,E), not(hayMasGrandes(P,C,E)).

esPrefijoValido(P,C,E) :- prefix(P,C), match_inst(P,E).

hayMasGrandes(P,C,E) :- esPrefijoValido(P2,C,E), P\=P2, length(P,L), length(P2,L2), L2> L.

/*prefijoMaximo(P,C,E) :- prefix(P,C), not(hayMasGrandes(P,C,E)).
hayMasGrandes(P,C,E) :- prefix(P2,C), P\=P2, length(P,L), length(P2,L2), L2> L.*/

% Ejercicio 8: reemplazar(+X, +R, +E, -Res)

% Solo nos interesa que sea true si el prefijo no es vacio 
prefijoMaximo2(P, C, E) :- prefijoMaximo(P, C, E), P \= [].

% Se remueven los elementos a consumir si son reemplazables
removerHasta(C, [], C).
removerHasta([C|Cs], [T|Ts], Res) :- C == T, removerHasta(Cs, Ts, Res).

reemplazar([], _, _, []).
reemplazar(C, R, T, Res) :- prefijoMaximo2(Pref, C, R), removerHasta(C, Pref, ProxC), reemplazar(ProxC, R, T, Res1), append(T, Res1, Res).
reemplazar([C|Cs], R, E, Res) :- not(prefijoMaximo2(_, [C|Cs], R)), reemplazar(Cs, R, E, Res1), append([C], Res1, Res).

%%%%%%%%%
% Tests %
%%%%%%%%%

use_module(library(plunit)).

% Ejercicio 1

test(1) :- tieneEstrella(or(a, concat(b, star(a)))).
test(2) :- not(tieneEstrella(concat(or(a, b), or(b, a)))).

% Ejercicio 2

test(3) :- longitudMaxima(a, N), N =:= 1.
test(4) :- longitudMaxima(concat(a, concat(b, or(a, b))), N), N =:= 3. 
test(5) :- longitudMaxima(or(a, b), N), N =:= 1. 
test(6) :- not( longitudMaxima(concat(star(a), or(a, b)), _) ).

test(7) :- iesimo(0, [1, 2, 3], X), X =:= 1.
test(8) :- iesimo(2, [1, 2, 3], X), X =:= 3.
test(9) :- not( iesimo(4, [], _) ).

% Ejercicio 3

test(10) :- esCadena([a, b, b, b, a]).
test(11) :- not( esCadena([1, a, b, b, a, a, a]) ).

% Ejercicio 4

test(12) :- match_inst([a,a,a], star(a)).
test(13) :- match_inst([], star(a)).
test(14) :- match_inst([a,b], star(concat(a,b))).
test(15) :- match_inst([a,b,a,b], star(concat(a,b))).
test(16) :- match_inst([a,b,a,b], star(or(a,b))).
test(17) :- match_inst([a,b], concat(a,b)).
test(18) :- match_inst([a,b], concat(a,or(b,c))).
test(19) :- match_inst([a,c], concat(a,or(b,c))).
test(20) :- match_inst([a], concat(a,star(a))).
test(21) :- match_inst([a,a,a], concat(a,star(a))).
test(22) :- match_inst([a,b], or(a,concat(a,b))).
test(23) :- match_inst([a], or(a,concat(a,b))).
test(24) :- match_inst([a], or(a,star(b))).
test(25) :- match_inst([b,b], or(a,star(b))).

% Ejercicio 5

test(26) :- match([a],a).
test(27) :- match([a,b], concat(a,b)).
test(28) :- match([a], or(a,b)), match([b],or(a,b)).
test(29) :- match([a,a], concat(a,or(a,b))), match([a,b], concat(a,or(a,b))).
test(30) :- match([a,a], concat(or(a,b),a)), match([b,a], concat(or(a,b),a)).
test(31) :- match([a,b], concat(or(a,b),or(a,b))), match([b,a], concat(or(a,b),or(a,b))), match([a,a], concat(or(a,b),or(a,b))), match([b,b], concat(or(a,b),or(a,b))).

% Ejercicio 6

test(32) :- diferencia([a], star(a), empty).
test(33) :- diferencia([a,a,a,a], star(a), concat(a,a)).
% Ver este tests
test(34). % :- diferencia([], concat(a,b), or(a,b)).
test(35) :- diferencia([a,b,a,b], star(or(a,b)), concat(a,concat(b,concat(a,concat(b,a))))).

% Ejercicio 7

test(36) :- prefijoMaximo([a,a,a], [a,a,a,b], star(a)).
test(37) :- prefijoMaximo([a,a], [a,a,a,b], concat(a,or(a,b))).
test(38) :- prefijoMaximo([a,a,a], [a,a,a,b], or(star(a),concat(a,a))).
test(39) :- prefijoMaximo([a,b,a,b,a], [a,b,a,b,a], concat(star(or(a,b)),concat(a,concat(b,a)))).

% Ejercicio 8

test(40) :- reemplazar([a, b], a, [c], [c,b]).
test(41) :- reemplazar([a, b, b], star(a), [c], [c,b,b]).
test(42) :- reemplazar([a, b], star(or(a,b)), [c], [c]).
test(43) :- reemplazar([a, b], concat(a,b), [c], [c]).
test(44) :- reemplazar([a, b,b,b], concat(b,b), [c], [a,c,b]).
test(45) :- reemplazar([a, b,b,b], concat(b,or(a,b)), [c], [a,c,b]).
test(46) :- reemplazar([a, b,b,b], concat(b,or(concat(b,b),b)), [c], [a,c,c]).
test(47) :- reemplazar([c, a, a, a, c, a, a, c], star(a), [1], [c, 1, c, 1, c]).

tests :- forall(between(1,47,N),test(N)).
