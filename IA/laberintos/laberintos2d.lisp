;;==================================
;;Laberintos 2D
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

(load "maze_lib.lisp")
(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
;;===================================================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará un arreglo de la forma #(i j)
;; (aref pos-actual 0) => renglón
;; (aref pos-actual 1) => columna
;;  #(i j) representa la posición en el arreglo *maze*
;; del archivo maze_lib.lisp
;;====================================================================

;;====================================================================
;; ESTADO INICIAL
;; *start* (de maze_lib.lisp)
;; ESTADO META
;; *goal* (de maze_lib.lisp)
;; para compara el estado meta con la posición actual utilizar equalp
;;====================================================================

;;======================================================================
;; VARIABLES GLOBALES
;;======================================================================
(defparameter *data-maze* nil) ;Datos del laberinto
(defparameter *maze-rows* nil) ;Renglones (índices) laberinto
(defparameter *maze-cols* nil) ;Columnas (índices) laberinto
(defparameter  *open* '()) ;; Frontera de busqueda...
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil) ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solution*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria


;;=======================================================================
;; FUNCIÓN PARA REINICIAR VARIABLES GLOBALES
;;=======================================================================
(defun reset-all ()
  (setq *data-maze* (slot-value *maze* 'data)) ;Datos del laberinto
  (setq *maze-rows* (1- (get-maze-rows))) ;Renglones (índices) laberinto
  (setq *maze-cols* (1- (get-maze-cols))) ;Columnas (índices) laberinto
  (setq *open* nil)
  (setq *memory* nil)
  (setq *id* 0)
  (setq *current-ancestor* nil)
  (setq *solution* nil)
);defun
;;========================================================================
;; OPERADORES
;; Se representarán con una lista de la forma (a b)
;; que representan moverse "renglón" y "columna" sobre el arreglo *maze*
;; a partir de la posición actual. a y b toman valores en {-1,0,1}
;; Se utilizarán los siguientes
;; :arriba (-1 0)
;; diag-arriba-derecha (-1 1)
;; derecha (0 1)
;; diag-abajo-derecha (1 1)
;; :abajo (1 0)
;; :diag-abajo-izquierda (1 -1)
;; :izquierda (0 -1)
;; :diag-arriba-izquierda (-1 -1)
;; Ya que moverse en diagonal es más rápido que en línea recta
;; los primeros operadores a revisar son los que involucran este tipo de
;; movimientos.
;;========================================================================
(defparameter *ops* '((:diag-arriba-der (-1 1))
                      (:diag-abajo-der (1 1))
                      (:diag-abajo-izq (1 -1))
                      (:diag-arriba-izq (-1 -1))
                      (:derecha (0 1))
                      (:abajo (1 0))
                      (:izquierda (0 -1))
                      (:arriba (-1 0))  ))

;;=======================================================================
;; REPRESENTA BINARIO
;; Función para representar una casilla del laberinto con un número
;; binario de 4 bits
;;=======================================================================
(defun representa-binario (numero)
"Representa un número entre el 0 y el 15 utilizando una codificación
binaria
ENTRADA
numero: Número obtenido del arreglo *data-maze*
SALDIA
binario: lista
"
  (case numero
    (0 '(0 0 0 0))
    (1 '(0 0 0 1))
    (2 '(0 0 1 0))
    (3 '(0 0 1 1))
    (4 '(0 1 0 0))
    (5 '(0 1 0 1))
    (6 '(0 1 1 0))
    (7 '(0 1 1 1))
    (8 '(1 0 0 0))
    (9 '(1 0 0 1))
    (10 '(1 0 1 0))
    (11 '(1 0 1 1))
    (12 '(1 1 0 0))
    (13 '(1 1 0 1))
    (14 '(1 1 1 0))
    (15 '(1 1 1 1)));case
);defun

;;=======================================================================
;; HUMANO-MAQUINA  y CODIFICA SOLUCIÓN
;;=======================================================================
(defun humano-maquina (etiqueta)
"Función para codificar una etiqueta humana de un operador en una
etiqueta de máquina"
  (case etiqueta
    (:diag-arriba-der 1)
    (:diag-abajo-der 3)
    (:diag-abajo-izq 5)
    (:diag-arriba-izq 7)
    (:derecha 2)
    (:abajo 4)
    (:izquierda 6)
    (:arriba 0))
);defun

(defun codifica-solucion ()
"Codifica la solución para que tenga la forma que utiliza el servidor
esta función se ejecuta después de actualizar la variable *solution* con
la función extract-solution"
  (let ((aux nil))
    (loop for i from 1 to (1- (length *solution*)) do
      (setq aux (append aux (list (humano-maquina (fourth (nth i *solution*))))))
      );loop
  aux
  );let
);defun
;;=======================================================================
;; VALIDA OPERADOR
;; Función para validar un operador
;;=======================================================================
(defun valid-operator? (op pos-actual)
"Valida si la aplicación de un operador es válido para la
posición actual
ENTRADA
op: Elemento de la lista *ops*
pos-actua: Arreglo de la forma #(i j) representando la posición actual
dentro del laberinto.
SALIDA
t: si es válido aplicar op a pos-actual
nil en otro caso.
"
  (let ((reng-pos-act 0) ;renglón de la posición actual
        (col-pos-act 0) ;Columna de la posición actual
        (reng-nva-pos 0) ;Renglón nueva posición
        (col-nva-pos 0);Columna nueva posición
        (rep-pos-act nil) ;Reprenstación binaria de la posición actual
        (rep-nva-pos nil) ;Representación binaria de la nueva posición
        (etiqueta nil)) ;Etiqueta de la operación

    ;Extrae información
    (setq etiqueta (first op))
    (setq reng-pos-act (aref pos-actual 0))
    (setq col-pos-act (aref pos-actual 1))
    (setq reng-nva-pos (+ (aref pos-actual 0) (first (second op))))
    (setq col-nva-pos (+ (aref pos-actual 1) (second (second op))))
    (setq rep-pos-act (representa-binario (aref *data-maze* reng-pos-act col-pos-act)))
    (if (and (>= reng-nva-pos 0) (<= reng-nva-pos *maze-rows*) (>= col-nva-pos 0) (<= col-nva-pos *maze-cols*) )
      (setq rep-nva-pos (representa-binario (aref *data-maze* reng-nva-pos col-nva-pos)))
      (return-from valid-operator? nil));if


    ;Revisa el operador caso por caso de acuerdo a la etiqueta
    (case etiqueta
      (:diag-arriba-der
        (if (and (>= reng-nva-pos 0 ) (<= col-nva-pos *maze-cols*) (or (and (= (nth 2 rep-pos-act) 0) (= (nth 1 rep-nva-pos) 0)) (and (= (nth 3 rep-pos-act) 0) (= (nth 0 rep-nva-pos) 0)) )  ) t))
      (:diag-abajo-der
        (if (and (<= reng-nva-pos *maze-rows* ) (<= col-nva-pos *maze-cols*) (or (and (= (nth 1 rep-pos-act) 0) (= (nth 0 rep-nva-pos) 0)) (and (= (nth 2 rep-pos-act) 0) (= (nth 3 rep-nva-pos) 0)) )  ) t) )
      (:diag-abajo-izq
        (if (and (<= reng-nva-pos *maze-rows* ) (>= col-nva-pos 0) (or (and (= (nth 1 rep-pos-act) 0) (= (nth 2 rep-nva-pos) 0)) (and (= (nth 0 rep-pos-act) 0) (= (nth 3 rep-nva-pos) 0)) )  ) t) )
      (:diag-arriba-izq
        (if (and (>= reng-nva-pos 0 ) (>= col-nva-pos 0) (or (and (= (nth 3 rep-pos-act) 0) (= (nth 2 rep-nva-pos) 0)) (and (= (nth 0 rep-pos-act) 0) (= (nth 1 rep-nva-pos) 0)) )  ) t) )
      (:arriba
        (if (and (>= reng-nva-pos 0) (= (nth 3 rep-pos-act) 0)) t ) )
      (:derecha
        (if (and (<= col-nva-pos *maze-cols*) (= (nth 2 rep-pos-act) 0)) t ) )
      (:abajo
        (if (and (<= reng-nva-pos *maze-rows*) (= (nth 1 rep-pos-act) 0)) t ) )
      (:izquierda
        (if (and (>= col-nva-pos 0) (= (nth 0 rep-pos-act) 0)) t ) )
    (otherwise nil)
    );case
  );let
);defun

;;=======================================================================
;; APPLY-OPERATOR (op pos-actual)
;; Aplica el operador op a las posición actual pos-actual
;; No verifica si la aplicación del operador es válida
;;=======================================================================
(defun apply-operator (op pos-actual)
"
ENTRADA
op: un elemento de la lista *ops*
pos-actual: arreglo de la forma #(i j)
SALIDA
nueva-pos: arreglo de la forma #(i j) con la nueva posición
"
  (let ((nueva-pos nil) (mov-reng 0) (mov-col 0) )
    (setq nueva-pos (make-array 2))
    (setq mov-reng (first (second op))) ;moviento en renglón
    (setq mov-col (second (second op))) ;moviento en columna
    (setf (aref nueva-pos 0) (+ (aref pos-actual 0) mov-reng)) ;nuevo renglón
    (setf (aref nueva-pos 1) (+ (aref pos-actual 1) mov-col)) ;nueva columna
  nueva-pos
  );let
);defun

;;=======================================================================
;;  CREATE-NODE [estado  op]
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
"Construye y regresa un nuevo nodo de búsqueda que contiene
al estado y operador recibidos como parámetro "
 ;;incrementamos primero para que lo último en procesarse sea la respuesta
  (incf  *id*)
  ;;los nodos generados son descendientes de *current-ancestor*
  (list  *id*  estado  *current-ancestor*  (first op)) )

;;=======================================================================
;; INSERT TO OPEN Y GET FROM OPEN
;;=======================================================================
(defun insert-to-open (estado  op  metodo)
"Permite insertar nodos de la frontera de busqueda *open*
de forma apta para buscar a lo profundo y a lo ancho"
(let ((nodo  (create-node  estado  op)))
     (cond ((eql  metodo :depth-first)
     (push  nodo  *open*))
     ((eql  metodo :breadth-first)
     (setq *open*  (append  *open*  (list nodo))))
     (T  Nil)))  )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
  (pop  *Open*))

;;=============================================================================
;;EXPAND [ estado]
;;Construye y regresa una lista con todos los descendientes validos de [estado]
;;=============================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado,
aplicando todos los operadores en *ops* en ese mismo órden"
  (let ((descendientes  nil)
	   (nuevo-estado  nil))
     (dolist  (op  *ops*  descendientes)
	      (setq  nuevo-estado  (apply-operator  op estado))
	    (when (valid-operator?  op  estado)
     (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

;;============================================================================
;; REMEMBER-STATE? y FILTER-MEMORIES
;;============================================================================
(defun  remember-state?  (estado  lista-memoria)
"RECURSIVA:
Busca un estado en una lista de nodos que sirve como memoria de intentos previos
el estado tiene estructura: #(i j),
el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]
la memoria es una lista que contiene nodos"

     (cond ((null  lista-memoria)  Nil)
	        ((equalp  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el primer nodo de la memoria?
		(T  (remember-state?  estado  (rest  lista-memoria))))  ) ;;Busca en el resto de la memoria

(defun  filter-memories (lista-estados-y-ops)
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo
estado está en la memoria *memory*
la lista de estados y operadores tiene estructura:
[(<estado> <op>) (<estado> <op>) ... ]
esta lista resulta de aplicar la función expand a un estado"

     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*);; si se recuerda el primer elemento de la lista, filtrarlo...
	       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;==========================================================================
;; EXTRACT-SOLUTION
;;==========================================================================

(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar
al estado inicial.
Los nodos son de la forma (list  *id*  estado  *current-ancestor*  (first op))
(first op) es la etiqueta huma de la operación
"
     (labels ((locate-node  (id  lista);; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista)) ;el  id del nodo es el id del primer elemento de la memoria?
		        (T  (locate-node  id (rest  lista)))))) ;Busca en el resto de la memoria

	  (let ((current  (locate-node  (first  nodo)  *memory*))) ;current es un nodo
	     (loop  while  (not (null  current))  do  ;Memoria de intentos previos...
		 (push  current  *solution*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor ("va subiendo en la memoria")
	     *solution*))

;;=========================================================================
;; BÚSQUEDA POR AMPLITUD (BREADTH-FIRST)
;;=========================================================================

(defun breadth-first ()

  (reset-all)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual *start*)
  (metodo :breadth-first))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do
     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (push  nodo  *memory*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  estado)
           (extract-solution  nodo)
           (setq aux-sol (codifica-solucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))

      ;Si todavía no se encuentra el estado meta
          (t (setq  *current-ancestor*  (first  nodo))
   	      (setq  sucesores  (expand estado))
    			  (setq  sucesores  (filter-memories  sucesores))
    			  (loop for  element  in  sucesores  do
    				(insert-to-open  (first element)  (second element)  metodo))))))

);defun

;;=========================================================================
;; BÚSQUEDA POR PROFUNDIDAD (DEPTH-FIRST)
;;=========================================================================

(defun depth-first ()

  (reset-all)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual *start*)
  (metodo :depth-first))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do
     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (push  nodo  *memory*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  estado)
           (extract-solution  nodo)
           (setq aux-sol (codifica-solucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))

      ;Si todavía no se encuentra el estado meta
          (t (setq  *current-ancestor*  (first  nodo))
   	      (setq  sucesores  (expand estado))
    			  (setq  sucesores  (filter-memories  sucesores))
    			  (loop for  element  in  sucesores  do
    				(insert-to-open  (first element)  (second element)  metodo))))))

);defun


(start-maze)
