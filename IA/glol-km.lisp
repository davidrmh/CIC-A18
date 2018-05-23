
;;Situación inicial
(every Personaje has
  (posicion (string)) )

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

;;Situación LC-GO
(*Situacion-LC-GO has (instance-of (Situation)))
(in-situation *Situacion-LC-GO)
(*Orilla-origen now-has (Personajes (*Lobo *Comida)))
(*Orilla-destino now-has (Personajes (*Granjero *Oveja)))

;Situación LCG-O
(*Situacion-LCG-O has (instance-of (Situation)))
(in-situation *Situacion-LCG-O)
(*Orilla-origen now-has (Personajes (*Lobo *Comida *Granjero)))
(*Orilla-destino now-has (Personajes (*Oveja)))

;Situación C-LGO
(*Situacion-C-LGO has (instance-of (Situation)))
(in-situation *Situacion-C-LGO)
(*Orilla-origen now-has (Personajes (*Comida)))
(*Orilla-destino now-has (Personajes (*Lobo *Granjero *Oveja)))

;Situación CGO-L
(*Situacion-CGO-L has (instance-of (Situation)))
(in-situation *Situacion-CGO-L)
(*Orilla-origen now-has (Personajes (*Comida *Granjero *Oveja)))
(*Orilla-destino now-has (Personajes (*Lobo)))

;Situación O-LGC
(*Situacion-O-LGC has (instance-of (Situation)))
(in-situation *Situacion-O-LGC)
(*Orilla-origen now-has (Personajes (*Oveja)))
(*Orilla-destino now-has (Personajes (*Lobo *Granjero *Comida)))

;Situación GO-LC
(*Situacion-GO-LC has (instance-of (Situation)))
(in-situation *Situacion-GO-LC)
(*Orilla-origen now-has (Personajes (*Oveja *Granjero)))
(*Orilla-destino now-has (Personajes (*Lobo *Comida)))

;Situación Vacia-LCGO
(*Situacion-vacia-LCGO has (instance-of (Situation)))
(in-situation *Situacion-vacia-LCGO)
(*Orilla-origen now-has
  (Personajes ((a Personaje) (a Personaje) (a Personaje) (a Personaje))) )
(*Orilla-destino now-has (Personajes (*Lobo *Comida *Granjero *Oveja)))

;Regresa a la situación global
(in-situation *Global)
