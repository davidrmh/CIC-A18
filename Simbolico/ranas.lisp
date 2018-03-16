;;==================================
;;PROBLEMA DE LAS RANAS
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

;;==============================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará una lista de la siguiente forma
;; ('C1 'C2 'C3 'C4 NIL 'V1 'V2 'V3 'V4)
;; El valor de NIL representa un espacio libre
;; los símbolos 'Ci y 'Vi representan las ranas
;; cafés y verdes respectivamente
;;===============================================

;;======================================
;; ESTADO INICIAL
;; ('C1 'C2 'C3 'C4 NIL 'V1 'V2 'V3 'V4)
;; ESTADO META
;; ('V1 'V2 'V3 'V4 NIL 'C1 'C2 'C3 'C4)
;;=======================================

;;==================================================================
;; OPERADORES
;; Los operadores tendrán la forma (indice saltos)
;; por ejemplo el operador (0 +1) se interpreta como
;; mover la rana en la posición 0 un lugar hacia adelante
;; en este caso es como dar cero saltos
;; (como se vio en clases 0 saltos es moverse un lugar a la derecha)

;; VALIDACIÓN DE OPERADORES
;; Un operador es válido si se cumple lo siguiente
;; 1. La posición origen es distinta de nil (hay una rana para mover)
;; 2. La posición destino es nil (no hay rana)
;;===================================================================

(defparameter *ops* '((:rana-pos-0-salta-0-derecha (0 1))
                      (:rana-pos-0-salta-1-derecha (0 2))
                      (:rana-pos-0-salta-2-derecha (0 3))
                      (:rana-pos-1-salta-0-izquierda (1 -1))
                      (:rana-pos-1-salta-0-derecha (1 1))
                      (:rana-pos-1-salta-1-derecha (1 2))
                      (:rana-pos-1-salta-2-derecha (1 3))
                      (:rana-pos-2-salta-1-izquierda (2 -2))
                      (:rana-pos-2-salta-0-izquierda (2 -1))
                      (:rana-pos-2-salta-0-derecha (2 1))
                      (:rana-pos-2-salta-1-derecha (2 2))
                      (:rana-pos-2-salta-2-derecha (2 3))
                      (:rana-pos-3-salta-2-izquierda (3 -3))
                      (:rana-pos-3-salta-1-izquierda (3 -2))
                      (:rana-pos-3-salta-0-izquierda (3 -1))
                      (:rana-pos-3-salta-0-derecha (3 1))
                      (:rana-pos-3-salta-1-derecha (3 2))
                      (:rana-pos-3-salta-2-derecha (3 3))
                      (:rana-pos-4-salta-2-izquierda (4 -3))
                      (:rana-pos-4-salta-1-izquierda (4 -2))
                      (:rana-pos-4-salta-0-izquierda (4 -1))
                      (:rana-pos-4-salta-0-derecha (4 1))
                      (:rana-pos-4-salta-1-derecha (4 2))
                      (:rana-pos-4-salta-2-derecha (4 3))
                      (:rana-pos-5-salta-2-izquierda (5 -3))
                      (:rana-pos-5-salta-1-izquierda (5 -2))
                      (:rana-pos-5-salta-0-izquierda (5 -1))
                      (:rana-pos-5-salta-0-derecha (5 1))
                      (:rana-pos-5-salta-1-derecha (5 2))
                      (:rana-pos-5-salta-2-derecha (5 3))
                      (:rana-pos-6-salta-2-izquierda (6 -3))
                      (:rana-pos-6-salta-1-izquierda (6 -2))
                      (:rana-pos-6-salta-0-izquierda (6 -1))
                      (:rana-pos-6-salta-0-derecha (6 1))
                      (:rana-pos-6-salta-1-derecha (6 2))
                      (:rana-pos-7-salta-2-izquierda (7 -3))
                      (:rana-pos-7-salta-1-izquierda (7 -2))
                      (:rana-pos-7-salta-0-izquierda (7 -1))
                      (:rana-pos-7-salta-0-derecha (7 1))
                      (:rana-pos-8-salta-2-izquierda (8 -3))
                      (:rana-pos-8-salta-1-izquierda (8 -2))
                      (:rana-pos-8-salta-0-izquierda (8 -1)) ))
