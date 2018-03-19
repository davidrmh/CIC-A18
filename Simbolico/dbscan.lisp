;;Necesito una variable global *vecinos* para ir agregando
;;las observaciones que estén a distancia menor a epsilon de la
;;observación actual.
;;Cuando se examina un renglón completo, hacer pop de *vecinos* para saber
;;que vecino toca revisar.
;;Cuando *vecinos* sea null, entonces iniciar con otro grupo (incf *grupo*)
;;Necesito una *memoria* para no agregar observaciones que ya se agregaron a
;;*vecinos* en algún momento.

;;Si *vecinos* es null y además *memoria* contiene todas las observaciones
;; (sus índices), entonces terminar. En otro caso buscar el siguiente cluster
;; iniciando con una observación que no esté en *memoria*
