
;;Situación inicial
(every Personaje has
  (posicion ()) )

(*Lobo has
  (instance-of (Personaje))
  (posicion ("origen")) )

(*Oveja has
  (instance-of (Personaje))
  (posicion ("origen")) )

(*Granjero has
  (instance-of (Personaje))
  (posicion ("origen")) )

(*Comida has
  (instance-of (Personaje))
  (posicion ("origen")) )

(*Orilla-origen has
  (Personajes (*Lobo *Oveja *Granjero *Comida ) )
)

(*Orilla-destino has
  (Personajes ((a Personaje) (a Personaje) (a Personaje) (a Personaje) ) )
)

(Last has (ultimo ( (a Personaje ))))

(reglas has (

  (regla1 ( (if ( (the posicion of *Granjero) = (the posicion of *Oveja)  ) and ( (the ultimo of Last) /= *Oveja )
  then ( (*Oveja now-has (posicion ("destino"))) and ( (Last now-has (ultimo (*Oveja))) ) )  ) ) )

  (regla2 ( (if ( (the posicion of *Granjero) = "destino") and ( (oneof (the Personajes of *Orilla-origen) where (t)  ) )
  then ( (*Granjero now-has (posicion ("origen")) ) and (Last now-has (ultimo (nil))) )  ) ) )

  (regla3 ( (if ( (the posicion of *Granjero) = "origen" ) and ((oneof (the Personajes of *Orilla-origen) where (t) ))
  then ( (?x == (oneof (the Personajes of *Orilla-origen) where (t) )) and (?x now-has (posicion ("destino")) ) and (*Granjero now-has (posicion ("destino")) )
    and (Last now-has ( ultimo (?x))) )  ) )   ) ) )
