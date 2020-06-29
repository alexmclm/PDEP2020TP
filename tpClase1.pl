
viveEnMansion(tiaAgatha).
viveEnMansion(elCarnicero).
viveEnMansion(charles).

odioDe(tiaAgatha,tiaAgatha).
odioDe(tiaAgatha,charles).

odioDe(charles,Persona):-
    viveEnMansion(Persona),
    not(odioDe(tiaAgatha,Persona)).

odioDe(elCarnicero,Persona):-
    odioDe(tiaAgatha,Persona).

esMasRicoQue(tiaAgatha,Persona):-
    viveEnMansion(Persona),
    not(odioDe(elCarnicero,Persona)).

matoA(Asesino,Victima):-
    viveEnMansion(Victima),
    odioDe(Asesino,Victima),
    esMasRicoQue(Victima,Persona).
