% === SISTEMA EXPERTO PARA CLASIFICACIÓN DE HONGOS ===
% Basado en el TP de Paradigmas y Lenguajes de Programación

% Reglas básicas de clasificación
es_ingerible(Sombrero, Olor, Habitat) :-
    olor_seguro(Olor),
    forma_segura(Sombrero),
    habitat_seguro(Habitat).

es_venenoso(Sombrero, Olor, Habitat) :-
    olor_peligroso(Olor);
    forma_peligrosa(Sombrero);
    habitat_peligroso(Habitat).

% Reglas específicas basadas en el árbol de decisión
es_ingerible('abultada', 'almendra', _).
es_ingerible('abultada', 'anis', _).
es_ingerible('abultada', 'ninguno', 'cercano').
es_ingerible('chata', 'ninguno', 'bosque').
es_ingerible('abotonada', 'ninguno', 'hojas').

es_venenoso('abultada', 'mohoso', _).
es_venenoso('abultada', 'ninguno', 'poblado').
es_venenoso('conica', 'ninguno', _).
es_venenoso(_, 'ninguno', 'praderas').

% Definición de características seguras
olor_seguro('almendra').
olor_seguro('anis').
olor_seguro('suave').

olor_peligroso('mohoso').
olor_peligroso('acre').
olor_peligroso('picante').
% olor_peligroso('ninguno'). % Comentado para evitar conflictos

forma_segura('abultada').
forma_segura('chata').
forma_segura('abotonada').
forma_segura('acampanada').

forma_peligrosa('conica').
forma_peligrosa('irregular').

habitat_seguro('cercano').
habitat_seguro('bosque').
habitat_seguro('hojas').
habitat_seguro('pastos').

habitat_peligroso('poblado').
habitat_peligroso('praderas').
habitat_peligroso('urbano').

% Regla de inferencia principal
clasificar_hongo(Sombrero, Olor, Habitat, Clase) :-
    es_ingerible(Sombrero, Olor, Habitat), Clase = 'ingerible'.
clasificar_hongo(Sombrero, Olor, Habitat, Clase) :-
    es_venenoso(Sombrero, Olor, Habitat), Clase = 'venenoso'.
clasificar_hongo(_, _, _, 'desconocido'). % Clasificación por defecto

% Consultas útiles para el sistema
listar_hongos_seguros :-
    findall([S, O, H], es_ingerible(S, O, H), Seguros),
    length(Seguros, Total),
    write('Hongos seguros para consumo: '), write(Total), nl,
    forall(member([S, O, H], Seguros), 
           (write(' - '), write(S), write(', '), write(O), write(', '), write(H), nl)).

listar_hongos_peligrosos :-
    findall([S, O, H], es_venenoso(S, O, H), Peligrosos),
    length(Peligrosos, Total),
    write('Hongos peligrosos: '), write(Total), nl,
    forall(member([S, O, H], Peligrosos), 
           (write(' - '), write(S), write(', '), write(O), write(', '), write(H), nl)).

% Estadísticas del sistema - CORREGIDO
estadisticas_hongos :-
    findall(_, es_ingerible(_, _, _), Seguros), 
    length(Seguros, TotalSeguros),
    findall(_, es_venenoso(_, _, _), Peligrosos), 
    length(Peligrosos, TotalPeligrosos),
    Total is TotalSeguros + TotalPeligrosos,
    (Total > 0 -> 
        PorcentajeSeguro is (TotalSeguros / Total) * 100;
        PorcentajeSeguro is 0
    ),
    write('=== ESTADÍSTICAS SISTEMA HONGOS ==='), nl,
    write('Total combinaciones analizadas: '), write(Total), nl,
    write('Combinaciones seguras: '), write(TotalSeguros), nl,
    write('Combinaciones peligrosas: '), write(TotalPeligrosos), nl,
    write('Porcentaje seguro: '), write(PorcentajeSeguro), write('%'), nl.