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
;; mover la rana en la posición (índice) 0 un lugar hacia adelante
;; en este caso es como dar cero saltos
;; (como se vio en clases 0 saltos es moverse un lugar a la derecha/izquierda)

;; VALIDACIÓN DE OPERADORES
;; Un operador es válido si se cumple lo siguiente
;; 1. La posición origen es distinta de nil (hay una rana para mover)
;; y además
;; 2. La posición destino es nil (no hay rana)
;;===================================================================

;;===================================================================
;; DEFINE VARIABLES GLOBALES

(defparameter  *open* '())       ;; Frontera de busqueda...
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;Operadores
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

;;=======================================================================
;;  CREATE-NODE [estado  op]
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
"Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
  (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
  (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN
;;
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================
  (defun insert-to-open (estado  op  metodo)
  "Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
  (let ((nodo  (create-node  estado  op)))
       (cond ((eql  metodo :depth-first)
       (push  nodo  *open*))
       ((eql  metodo :breath-first)
       (setq *open*  (append  *open*  (list nodo))))
   	   (T  Nil)))  )


  (defun get-from-open ()
  "Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (pop  *Open*))

;;=======================================================================
;; VALID-OPERATOR?
;; Función para validar si un operador es aplicable a cierto estado
;; recibe una lista representando un operador y una lista representando
;; el estado.
;;
;; Un operador es válido si se cumple lo siguiente
;; 1. La posición origen es distinta de nil (hay una rana para mover)
;; y además
;; 2. La posición destino es nil (no hay rana)
;;=======================================================================

(defun valid-operator?(op estado)
  (let ((indice nil) ;índice del elemento a mover
      (movimiento nil)); dirección y magnitud del desplazamiento
    (setq indice (first (second op)))
    (setq movimiento (second (second op)))
    (cond
      ((and (not (eql (nth indice estado) nil)) (eql (nth (+ indice movimiento) estado) nil)) t)
      (t nil))))

;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;=======================================================================
(defun apply-operator(op estado)
"Aplica la operación op al estado"
  (let ((indice nil)) (movimiento nil) (copia-estado nil)
    (setq indice (first (second op))) ;índice de la posición de la rana
    (setq movimiento (second (second op))) ;Dirección y magnitud del salto
    (setq copia-estado (copy-seq estado));Copia el estado para evitar modificarlo
    ;Intercambia el nil con la rana
    (setf (nth (+ indice movimiento) copia-estado) (nth indice copia-estado))
    (setf (nth indice copia-estado) nil)))

;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes)
	         (setq  nuevo-estado  (apply-operator  op estado))
		 (when (valid-operator?  op  estado)
      (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))
