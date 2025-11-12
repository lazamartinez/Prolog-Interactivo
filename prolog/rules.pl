% === REGLAS BÁSICAS PROLOG - COMPATIBLES CON TAU-PROLOG ===
% Versión simplificada para Tau-Prolog

% Reglas básicas de listas (COMPATIBLES)
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

length([], 0).
length([_|T], N) :-
    length(T, M),
    N is M + 1.

append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

% Conversión básica string/number (SIMPLIFICADA)
number_string(Num, Str) :-
    atom_number(Str, Num).  % Tau-Prolog usa atom_number en lugar de number_codes

% Comparación numérica con strings (SIMPLIFICADA)
es_mayor_que(Str1, Str2) :-
    number_string(Num1, Str1),
    number_string(Num2, Str2),
    Num1 > Num2.

% Búsqueda y filtrado (SIMPLIFICADO)
encontrar_por_columna(Columna, Valor, ID) :-
    dato(ID, Columna, Valor).

% Contar usando findall de Tau-Prolog
contar_por_columna(Columna, Total) :-
    findall(ID, dato(ID, Columna, _), Lista),
    length(Lista, Total).

% REGLAS ADICIONALES PARA EL SISTEMA DE IMÁGENES
es_comestible(ID) :- 
    seguridad_objeto(ID, 'seguro'), 
    estado_objeto(ID, Estado), 
    Estado \= 'podrida', 
    Estado \= 'madura_en_exceso'.

no_es_comestible(ID) :- seguridad_objeto(ID, 'peligroso').
no_es_comestible(ID) :- estado_objeto(ID, 'podrida').
no_es_comestible(ID) :- estado_objeto(ID, 'madura_en_exceso').

esta_podrido(ID) :- estado_objeto(ID, 'podrida').
esta_maduro(ID) :- estado_objeto(ID, 'madura').

es_manzana(ID) :- 
    objeto_detectado(ID, Objeto, _), 
    (Objeto = 'manzana'; Objeto = 'apple').

es_platano(ID) :- 
    objeto_detectado(ID, Objeto, _), 
    (Objeto = 'plátano'; Objeto = 'banana').

es_fruta(ID) :- 
    objeto_detectado(ID, Objeto, _), 
    (Objeto = 'fruta'; Objeto = 'fruit'; es_manzana(ID); es_platano(ID)).

% Conteos básicos
contar_objetos_seguros(Total) :- 
    findall(ID, seguridad_objeto(ID, 'seguro'), Lista), 
    length(Lista, Total).

contar_objetos_peligrosos(Total) :- 
    findall(ID, seguridad_objeto(ID, 'peligroso'), Lista), 
    length(Lista, Total).

contar_manzanas(Total) :- 
    findall(ID, es_manzana(ID), Lista), 
    length(Lista, Total).

contar_platanos(Total) :- 
    findall(ID, es_platano(ID), Lista), 
    length(Lista, Total).

% Mostrar información
mostrar_objetos :- 
    objeto_detectado(ID, Objeto, Confianza), 
    write('Objeto '), write(ID), write(': '), write(Objeto), 
    write(' ('), write(Confianza), write(')'), nl, fail.
mostrar_objetos.

mostrar_seguridad :- 
    seguridad_objeto(ID, Seguridad), 
    write('Objeto '), write(ID), write(': '), write(Seguridad), nl, fail.
mostrar_seguridad.

mostrar_estados :- 
    estado_objeto(ID, Estado), 
    write('Objeto '), write(ID), write(': '), write(Estado), nl, fail.
mostrar_estados.

resumen_seguridad :- 
    contar_objetos_seguros(Seguros), 
    contar_objetos_peligrosos(Peligrosos), 
    total_objetos(Total), 
    write('Seguros: '), write(Seguros), write(' / '), write(Total), nl,
    write('Peligrosos: '), write(Peligrosos), write(' / '), write(Total), nl.

verificar_manzanas :- 
    contar_manzanas(Total), 
    write('Total manzanas: '), write(Total), nl.