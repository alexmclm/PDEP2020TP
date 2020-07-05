
viveEnMansion(tiaAgatha).
viveEnMansion(elCarnicero).
viveEnMansion(charles).

% odioDe(tiaAgatha,tiaAgatha).
% odioDe(tiaAgatha,charles).


odioDe(tiaAgatha,Persona):-
    viveEnMansion(Persona),
    Persona \= elCarnicero.
	
odioDe(charles,Persona):-
    viveEnMansion(Persona),
    not(odioDe(tiaAgatha,Persona)).

odioDe(elCarnicero,Persona):-
    odioDe(tiaAgatha,Persona).

esMasRicoQue(tiaAgatha,Persona):-
    viveEnMansion(Persona),
    not(odioDe(elCarnicero,Persona)).

matoA(Asesino,Victima):-
    /*claro, ya entendi por que hice mal al poner viveEnMansion , maldito termino de inversibilidad (? jajajaj*/
    odioDe(Asesino,Victima),
    esMasRicoQue(Victima,Persona).