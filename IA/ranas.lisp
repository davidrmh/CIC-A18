;;==================================
;;PROBLEMA DE LAS RANAS
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

;;==============================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará una lista de la siguiente forma
;; ('C 'C 'C 'C NIL 'V 'V 'V 'V)
;; El valor de NIL representa un espacio libre
;; los símbolos 'C y 'V representan las ranas
;; cafés y verdes respectivamente
;;===============================================

;;======================================
;; ESTADO INICIAL
;; ('C 'C 'C 'C NIL 'V 'V 'V 'V)
;; ESTADO META
;; ('V 'V 'V 'V NIL 'C 'C 'C 'C)
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
;Archivo en donde se guardarán los resultados
;(defparameter *stream* nil)

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
  (let ((indice nil) (movimiento nil) (copia-estado nil))
    (setq indice (first (second op))) ;índice de la posición de la rana
    (setq movimiento (second (second op))) ;Dirección y magnitud del salto
    (setq copia-estado (copy-seq estado));Copia el estado para evitar modificarlo
    ;Intercambia el nil con la rana
    (setf (nth (+ indice movimiento) copia-estado) (nth indice copia-estado))
    (setf (nth indice copia-estado) nil)
    copia-estado))

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

;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"RECURSIVA:
Busca un estado en una lista de nodos que sirve como memoria de intentos previos
el estado tiene estructura:  ('C1 'C2 'C3 'C4 NIL 'V1 'V2 'V3 'V4),
el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]
la memoria es una lista que contiene nodos"
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el primer nodo de la memoria?
		(T  (remember-state?  estado  (rest  lista-memoria))))  ) ;;Busca en el resto de la memoria

(defun  filter-memories (lista-estados-y-ops)
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]
esta lista resulta de aplicar la función expand a un estado"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta


;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial
los nodos son de la forma (list  *id*  estado  *current-ancestor*  (first op))
(first op) es la etiqueta huma de la operación
"
     (labels ((locate-node  (id  lista);; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista)) ;el  id del nodo es el id del primer elemento de la memoria?
		        (T  (locate-node  id (rest  lista)))))) ;Busca en el resto de la memoria

	  (let ((current  (locate-node  (first  nodo)  *memory*))) ;current es un nodo
	     (loop  while  (not (null  current))  do  ;Memoria de intentos previos...
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor ("va subiendo en la memoria")
	     *solucion*))

(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"

    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
        ;; imprimir el número de paso, operador y estado...
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )


;;==============================================================================
;; FUNCIÓN PARA INICIALIZAR VARIABLES GLOBALES
(defun reset-all ()
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     ;(setq *stream* (open "resultados-ranas.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create))
)
;;==============================================================================
;; FUNCIÓN PRINCIPAL

(defun  main-ranas (edo-inicial  edo-meta  metodo)
";; ESTADO INICIAL
;; '('C 'C 'C 'C NIL 'V 'V 'V 'V)
;; ESTADO META
;; '('V 'V 'V 'V NIL 'C 'C 'C 'C)
Realiza una búsqueda ciega, por el método especificado y
desde un estado inicial hasta un estado meta.
Los métodos posibles son:  :depth-first - búsqueda en profundidad
                           :breath-first - búsqueda en anchura"
 (reset-all)
 (let ((nodo nil)
 (estado nil)
 (sucesores  '())
 (operador  nil)
 (meta-encontrada  nil))

  (insert-to-open   edo-inicial  nil  metodo)
  (loop until  (or  meta-encontrada
  (null *open*))  do
    (setq  nodo    (get-from-open)
    estado  (second  nodo)
    operador  (third  nodo))
    (push  nodo  *memory*)
    (cond ((equal  edo-meta  estado)
         (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
         (display-solution  (extract-solution  nodo))
         ;(close *stream*)
         (setq  meta-encontrada  T))
         (t (setq  *current-ancestor*  (first  nodo))
  	      (setq  sucesores  (expand estado))
   			  (setq  sucesores  (filter-memories  sucesores))
   			  (loop for  element  in  sucesores  do
   				(insert-to-open  (first element)  (second element)  metodo))))))  )

;;=======================================================================
;;=======================================================================
