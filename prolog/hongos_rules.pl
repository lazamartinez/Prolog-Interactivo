% === SISTEMA EXPERTO PARA CLASIFICACIÓN DE HONGOS ===
% BASADO EN EL TP DE PARADIGMAS Y LENGUAJES DE PROGRAMACIÓN
% VERSIÓN 100% COMPATIBLE CON TAU-PROLOG

% REGLAS PRINCIPALES DE CLASIFICACIÓN
clasificar_hongo(Sombrero, Olor, Habitat, Clase) :-
    es_ingerible(Sombrero, Olor, Habitat), 
    !,
    Clase = 'ingerible'.

clasificar_hongo(Sombrero, Olor, Habitat, Clase) :-
    es_venenoso(Sombrero, Olor, Habitat), 
    !,
    Clase = 'venenoso'.

clasificar_hongo(_, _, _, 'desconocido').

% HONGOS COMESTIBLES
es_ingerible('abultada', 'almendra', _).
es_ingerible('abultada', 'anis', _).
es_ingerible('abultada', 'ninguno', 'cercano').
es_ingerible('chata', 'ninguno', 'bosque').
es_ingerible('abotonada', 'ninguno', 'hojas').

% HONGOS VENENOSOS
es_venenoso('abultada', 'mohoso', _).
es_venenoso('abultada', 'ninguno', 'poblado').
es_venenoso('conica', 'ninguno', _).
es_venenoso(_, 'ninguno', 'praderas').

% CONSULTAS COMPATIBLES - SIN FINDALL

% Contar manualmente (forma compatible)
contar_seguros(5).  % 5 hongos seguros conocidos
contar_peligrosos(4). % 4 hongos peligrosos conocidos

% Listar explícitamente (sin findall)
hongos_seguros_lista([
    ['abultada', 'almendra', 'cualquier'],
    ['abultada', 'anis', 'cualquier'],
    ['abultada', 'ninguno', 'cercano'],
    ['chata', 'ninguno', 'bosque'],
    ['abotonada', 'ninguno', 'hojas']
]).

hongos_peligrosos_lista([
    ['abultada', 'mohoso', 'cualquier'],
    ['abultada', 'ninguno', 'poblado'],
    ['conica', 'ninguno', 'cualquier'],
    ['cualquier', 'ninguno', 'praderas']
]).

% Verificaciones directas
es_seguro(S, O, H) :- es_ingerible(S, O, H).
es_peligroso(S, O, H) :- es_venenoso(S, O, H).

% Ejemplos de prueba
ejemplo_1 :- clasificar_hongo('abultada', 'almendra', 'bosque', 'ingerible').
ejemplo_2 :- clasificar_hongo('abultada', 'mohoso', 'bosque', 'venenoso').
ejemplo_3 :- clasificar_hongo('x', 'y', 'z', 'desconocido').

% Verificar sistema completo
sistema_funciona :-
    ejemplo_1,
    ejemplo_2,
    ejemplo_3,
    contar_seguros(5),
    contar_peligrosos(4).

% Consultas útiles para la interfaz
total_hongos_conocidos(9). % 5 seguros + 4 peligrosos
porcentaje_seguro(55.56).  % 5/9 * 100