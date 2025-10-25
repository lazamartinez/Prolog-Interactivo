% Reglas básicas de Prolog para el sistema
% Hechos y reglas de ejemplo

% Base de conocimientos de ejemplo
color(rojo).
color(azul).
color(verde).
color(amarillo).

tamano(pequeno).
tamano(mediano).
tamano(grande).

forma(circular).
forma(cuadrado).
forma(triangular).

% Reglas de inferencia
es_primario(Color) :- color(Color), (Color = rojo; Color = azul; Color = amarillo).

es_calido(Color) :- color(Color), (Color = rojo; Color = amarillo).

es_frio(Color) :- color(Color), (Color = azul; Color = verde).

combinacion_armonica(Color1, Color2) :-
    es_calido(Color1), es_calido(Color2);
    es_frio(Color1), es_frio(Color2).

% Reglas para análisis de datos
es_mayor_que(X, Y) :- X > Y.
es_menor_que(X, Y) :- X < Y.
es_igual(X, X).

rango_aceptable(Valor, Min, Max) :- 
    Valor >= Min, 
    Valor =< Max.

% Reglas para patrones
es_patron_creciente([A, B, C]) :- A < B, B < C.
es_patron_decreciente([A, B, C]) :- A > B, B > C.

% Reglas de clasificación
clasificar_tamano(Valor, pequeno) :- Valor < 10.
clasificar_tamano(Valor, mediano) :- Valor >= 10, Valor < 50.
clasificar_tamano(Valor, grande) :- Valor >= 50.

% Reglas lógicas
y(A, B) :- A, B.
o(A, B) :- A; B.
no(A) :- \+ A.

% Listas
miembro(X, [X|_]).
miembro(X, [_|Cola]) :- miembro(X, Cola).

concatenar([], Lista, Lista).
concatenar([Cabeza|Cola1], Lista2, [Cabeza|Cola3]) :- 
    concatenar(Cola1, Lista2, Cola3).

longitud([], 0).
longitud([_|Cola], N) :- 
    longitud(Cola, M), 
    N is M + 1.

invertir(Lista, Invertida) :- 
    invertir(Lista, [], Invertida).

invertir([], Acum, Acum).
invertir([Cabeza|Cola], Acum, Invertida) :- 
    invertir(Cola, [Cabeza|Acum], Invertida).

% Reglas para el sistema de datos
valor_atipico(Valor, Media, Desviacion) :-
    Limite is Media + 2 * Desviacion,
    (Valor > Limite; Valor < (Media - 2 * Desviacion)).

tendencia_ascendente(Lista) :-
    longitud(Lista, N),
    N >= 3,
    ultimos_tres(Lista, [A, B, C]),
    A < B, B < C.

ultimos_tres(Lista, [A, B, C]) :-
    reverse(Lista, [C, B, A|_]).

% Reglas de negocio ejemplo
es_venenoso(Planta) :-
    caracteristica(Planta, color, rojo),
    caracteristica(Planta, forma, circular).

es_comestible(Planta) :-
    caracteristica(Planta, color, verde),
    caracteristica(Planta, tamano, mediano).

necesita_riego(Planta) :-
    caracteristica(Planta, humedad, Baja),
    Baja < 30.