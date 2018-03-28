;;==================================
;;Laberintos 3D
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

(load "maze_lib.lisp")
(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
(add-algorithm 'bestfs)
(add-algorithm 'a-estrella)
;;===================================================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará un arreglo de la forma #(i j k l)
;; (aref pos-actual 0) => renglón
;; (aref pos-actual 1) => columna
;; (aref pos-actual 2) => Valor de la función de aptitud (Sólo bestFS y A*)
;; (aref pos-actual 3) => Se está sobre un puente (1) o debajo de el (0)
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
(defparameter *altura* 0)


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
  (setq *altura* 0)
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
                      (:derecha-bajo-puente (0 1))
                      (:derecha-sobre-puente (0 1))
                      (:abajo (1 0))
                      (:abajo-bajo-puente (1 0))
                      (:abajo-sobre-puente (1 0))
                      (:izquierda (0 -1))
                      (:izquierda-bajo-puente (0 -1))
                      (:izquierda-sobre-puente (0 -1))
                      (:arriba (-1 0))
                      (:arriba-bajo-puente (-1 0))
                      (:arriba-sobre-puente (-1 0))  ))

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
    (15 '(1 1 1 1))
    (16 '(1 0 0 0 0)) ;puente izquierda-derecha
    (17 '(1 0 0 0 1)));puente arriba-abajo
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
    (:derecha-bajo-puente 2)
    (:derecha-sobre-puente 2)
    (:abajo 4)
    (:abajo-bajo-puente 4)
    (:abajo-sobre-puente 4)
    (:izquierda 6)
    (:izquierda-bajo-puente 6)
    (:izquierda-sobre-puente 6)
    (:arriba 0)
    (:arriba-bajo-puente 0)
    (:arriba-sobre-puente 0)  )
);defun

(defun codifica-solucion ()
"Codifica la solución para que tenga la forma que utiliza el servidor
esta función se ejecuta después de actualizar la variable *solution* con
la función extract-solution"
  (let ((aux nil))
    (loop for i from 1 to (1- (length *solution*)) do
      (format t "~a~%" (fourth (nth i *solution*)))
      (setq aux (append aux (list (humano-maquina (fourth (nth i *solution*))))))
      );loop
  aux
  );let
);defun
;;=======================================================================
;; VALIDA OPERADOR
;; Función para validar un operador
;;=======================================================================

(defun es-puente17? (renglon columna)
"Función para determinar si una casilla es un puente arriba-abajo"
  (if (= 17 (aref *data-maze* renglon columna))
    (return-from es-puente17? t)
    (return-from es-puente17? nil))
);defun

(defun es-puente16? (renglon columna)
"Función para determinar si una casilla es un puente izquierda-derecha"

(if (= 16 (aref *data-maze* renglon columna))
    (return-from es-puente16? t)
    (return-from es-puente16? nil))

);defun

(defun es-puente? (renglon columna)
"Función para determinar si una casilla es un puente"
  (if (or (= 16 (aref *data-maze* renglon columna))
      (= 17 (aref *data-maze* renglon columna)))
      (return-from es-puente? t) (return-from es-puente? nil))
);defun

(defun valid-operator? (op pos-actual)
"Valida si la aplicación de un operador es válido para la
posición actual
ENTRADA
op: Elemento de la lista *ops*
pos-actual: Arreglo de la forma #(i j k l) representando la posición actual
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

    ;Si alguna posición está fuera del laberinto regresa nil
    (if (and (>= reng-nva-pos 0) (<= reng-nva-pos *maze-rows*) (>= col-nva-pos 0) (<= col-nva-pos *maze-cols*) )
      (setq rep-nva-pos (representa-binario (aref *data-maze* reng-nva-pos col-nva-pos)))
      (return-from valid-operator? nil));if

    ;Revisa el operador caso por caso de acuerdo a la etiqueta
    ;Para el caso de las diagonales ahora se revisa que la nueva posición
    ;no sea un puente o que se "salte" uno.
    (case etiqueta
      (:diag-arriba-der
        (if (and (>= reng-nva-pos 0 ) (<= col-nva-pos *maze-cols*) (not (es-puente? reng-pos-act col-pos-act)) (not (es-puente? reng-pos-act col-nva-pos)) (not (es-puente? reng-nva-pos col-pos-act)) (not (es-puente? reng-nva-pos col-nva-pos))
         (or (and (= (nth 2 rep-pos-act) 0) (= (nth 1 rep-nva-pos) 0)) (and (= (nth 3 rep-pos-act) 0) (= (nth 0 rep-nva-pos) 0)) )  ) t))

      (:diag-abajo-der
        (if (and (<= reng-nva-pos *maze-rows* ) (<= col-nva-pos *maze-cols*) (not (es-puente? reng-pos-act col-pos-act)) (not (es-puente? reng-pos-act col-nva-pos)) (not (es-puente? reng-nva-pos col-pos-act)) (not (es-puente? reng-nva-pos col-nva-pos))
        (or (and (= (nth 1 rep-pos-act) 0) (= (nth 0 rep-nva-pos) 0)) (and (= (nth 2 rep-pos-act) 0) (= (nth 3 rep-nva-pos) 0)) )  ) t) )

      (:diag-abajo-izq
        (if (and (<= reng-nva-pos *maze-rows* ) (>= col-nva-pos 0) (not (es-puente? reng-pos-act col-pos-act)) (not (es-puente? reng-pos-act col-nva-pos)) (not (es-puente? reng-nva-pos col-pos-act)) (not (es-puente? reng-nva-pos col-nva-pos))
        (or (and (= (nth 1 rep-pos-act) 0) (= (nth 2 rep-nva-pos) 0)) (and (= (nth 0 rep-pos-act) 0) (= (nth 3 rep-nva-pos) 0)) )  ) t) )

      (:diag-arriba-izq
        (if (and (>= reng-nva-pos 0 ) (>= col-nva-pos 0) (not (es-puente? reng-pos-act col-pos-act)) (not (es-puente? reng-pos-act col-nva-pos)) (not (es-puente? reng-nva-pos col-pos-act)) (not (es-puente? reng-nva-pos col-nva-pos))
        (or (and (= (nth 3 rep-pos-act) 0) (= (nth 2 rep-nva-pos) 0)) (and (= (nth 0 rep-pos-act) 0) (= (nth 1 rep-nva-pos) 0)) )  ) t) )

      (:arriba
        (if (and (>= reng-nva-pos 0) (= (nth 3 rep-pos-act) 0) (not (es-puente? reng-pos-act col-pos-act)) )t))
      (:arriba-bajo-puente
        (if (and (>= reng-nva-pos 0) (= (nth 3 rep-pos-act) 0) (es-puente16? reng-pos-act col-pos-act) (= (aref pos-actual 3) 0) )t))
      (:arriba-sobre-puente
        (if (and (>= reng-nva-pos 0) (= (nth 3 rep-pos-act) 0) (es-puente17? reng-pos-act col-pos-act) (= (aref pos-actual 3) 1) )t))

      (:derecha
        (if (and (<= col-nva-pos *maze-cols*) (= (nth 2 rep-pos-act) 0) (not (es-puente? reng-pos-act col-pos-act)) )t))
      (:derecha-bajo-puente
        (if (and (<= col-nva-pos *maze-cols*) (= (nth 2 rep-pos-act) 0) (es-puente17? reng-pos-act col-pos-act) (= (aref pos-actual 3) 0) )t))
      (:derecha-sobre-puente
        (if (and (<= col-nva-pos *maze-cols*) (= (nth 2 rep-pos-act) 0) (es-puente16? reng-pos-act col-pos-act) (= (aref pos-actual 3) 1) )t))

      (:abajo
        (if (and (<= reng-nva-pos *maze-rows*) (= (nth 1 rep-pos-act) 0) (not (es-puente? reng-pos-act col-pos-act)) )t))
      (:abajo-bajo-puente
        (if (and (<= reng-nva-pos *maze-rows*) (= (nth 1 rep-pos-act) 0) (es-puente16? reng-pos-act col-pos-act) (= (aref pos-actual 3) 0) )t))
      (:abajo-sobre-puente
        (if (and (<= reng-nva-pos *maze-rows*) (= (nth 1 rep-pos-act) 0) (es-puente17? reng-pos-act col-pos-act) (= (aref pos-actual 3) 1) )t))

      (:izquierda
        (if (and (>= col-nva-pos 0) (= (nth 0 rep-pos-act) 0) (not (es-puente? reng-pos-act col-pos-act)))t))
      (:izquierda-bajo-puente
        (if (and (>= col-nva-pos 0) (= (nth 0 rep-pos-act) 0) (es-puente17? reng-pos-act col-pos-act) (= (aref pos-actual 3) 0) )t))
      (:izquierda-sobre-puente
        (if (and (>= col-nva-pos 0) (= (nth 0 rep-pos-act) 0) (es-puente16? reng-pos-act col-pos-act) (= (aref pos-actual 3) 1) )t))
    (otherwise nil)
    );case
  );let
);defun

;;=======================================================================
;; FUNCIÓNES DE APTITUD Y COSTO
;; Se utiliza la distancia euclidiana ya que se adminten
;; movimientos en diagonal. Entre más
;; pequeño sea el valor de la aptitud más prioridad tendrás ese estado
;; Para el costo se utiliza la distancia entre el nodo-padre y el nodo-hijo
;;=======================================================================

(defun costo (new-pos old-pos)
  (let ((costo 0))
    (setq costo (+ (* (- (aref new-pos 0) (aref old-pos 0) ) (- (aref new-pos 0) (aref old-pos 0) ) )
    (* (- (aref new-pos 1) (aref old-pos 1)) (- (aref new-pos 1) (aref old-pos 1)) ) ) )
    costo
  )
);defun

(defun aptitud (posicion &optional (star nil) (old-pos nil))
  "Función de aptitud para el algoritmo BestFS
  ENTRADA:
  posicion: arreglo #(i j k)
  SALIDA:
  aptitud: número
  "
  (let ((aptitud 0))
    (setq aptitud (+ (* (- (aref posicion 0) (aref *goal* 0) ) (- (aref posicion 0) (aref *goal* 0) ) )
     (* (- (aref posicion 1) (aref *goal* 1)) (- (aref posicion 1) (aref *goal* 1)) ) ) )
     (if star (setq aptitud (+ aptitud (costo posicion old-pos)))) ;A-estrella
     aptitud);let
);defun

;;=======================================================================
;; APPLY-OPERATOR (op pos-actual)
;; Aplica el operador op a las posición actual pos-actual
;; No verifica si la aplicación del operador es válida
;;=======================================================================
(defun apply-operator (op pos-actual &optional (star nil))
"
ENTRADA
op: un elemento de la lista *ops*
pos-actual: arreglo de la forma #(i j k l)
star: boolean para indicar si es o no el algoritmo estrella
SALIDA
nueva-pos: arreglo de la forma #(i j) con la nueva posición
"
  (let ((nueva-pos nil) (mov-reng 0) (mov-col 0) (etiqueta nil) )

    (setq etiqueta (first op)) ;etiqueta de la operación
    (setq nueva-pos (make-array 4)) ;Nueva posición

    (setq mov-reng (first (second op))) ;moviento en renglón
    (setq mov-col (second (second op))) ;moviento en columna
    (setf (aref nueva-pos 0) (+ (aref pos-actual 0) mov-reng)) ;nuevo renglón
    (setf (aref nueva-pos 1) (+ (aref pos-actual 1) mov-col)) ;nueva columna

    ;Calcula aptitud
    (setf (aref nueva-pos 2) (aptitud nueva-pos star pos-actual))

    ;Determina si va sobre/bajo el puente
    ;(case etiqueta
    ;  (:derecha-sobre-puente (setf (aref nueva-pos 3) 1))
    ;  (:abajo-sobre-puente (setf (aref nueva-pos 3) 1))
    ;  (:izquierda-sobre-puente (setf (aref nueva-pos 3) 1))
    ;  (:arriba-sobre-puente (setf (aref nueva-pos 3) 1))
    ;  (:derecha-bajo-puente (setf (aref nueva-pos 3) 0))
    ;  (:abajo-bajo-puente (setf (aref nueva-pos 3) 0))
    ;  (:izquierda-bajo-puente (setf (aref nueva-pos 3) 0))
    ;  (:abajo-bajo-puente (setf (aref nueva-pos 3) 0))
    ;);case

    (cond
      ((and (>= (aref nueva-pos 0) 0) (<= (aref nueva-pos 0) *maze-rows*) (>= (aref nueva-pos 1) 0) (<= (aref nueva-pos 1) *maze-cols*) (es-puente16? (aref nueva-pos 0) (aref nueva-pos 1))
        (or (eql etiqueta :derecha) (eql etiqueta :izquierda) (eql etiqueta :derecha-sobre-puente) (eql etiqueta :derecha-bajo-puente) (eql etiqueta :izquierda-sobre-puente) (eql etiqueta :izquierda-bajo-puente) ) ) (setf (aref nueva-pos 3) 1) )
      ((and (>= (aref nueva-pos 0) 0) (<= (aref nueva-pos 0) *maze-rows*) (>= (aref nueva-pos 1) 0) (<= (aref nueva-pos 1) *maze-cols*) (es-puente17? (aref nueva-pos 0) (aref nueva-pos 1))
        (or (eql etiqueta :arriba) (eql etiqueta :abajo) (eql etiqueta :arriba-sobre-puente) (eql etiqueta :arriba-bajo-puente) (eql etiqueta :abajo-sobre-puente) (eql etiqueta :abajo-bajo-puente) ) ) (setf (aref nueva-pos 3) 1) )
    )

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
;; REORDENA-OPEN
;; Reordena *open* de acuerdo a la aptitud de cada estado
;;=======================================================================

(defun reordena-open ()
  (let ((open-aux nil) (aptitudes nil) (aux nil))

    ;Extrae las aptitudes
    (loop for nodo in *open* do
      (setq aptitudes (append aptitudes (list (aref (second nodo) 2) ) ) ))

    ;Ordena la aptitudes de menor a mayor
    (setq aptitudes (sort aptitudes #'<))

    ;Acomoda los estados de acuerdo a su aptitud
    (loop for aptitud in aptitudes do
      (loop for nodo in *open* do
        (when (and (equal aptitud (aref (second nodo) 2)) (not (member aptitud aux)))
            (setq open-aux (append open-aux (list nodo)) ) )
      );loop nodo
      (setq aux (append aux (list aptitud)))
    );loop aptitud
    (setq *open* open-aux)
  );let

);defun

;;===========================================================================
;; ACTUALIZA-OPEN
;; Función para actualizar *open* de acuerdo al algoritmo A*
;;===========================================================================
(defun actualiza-open (nodo)
  "ENTRADA:
  nodo: elemento de la lista sucesores ([estado op] [estado op]...)
  SALIDA:
  Función destructiva que actualiza la variable *open*
  "
  (let ((edo-nodo nil) (costo-nodo nil) (edo-open nil) (costo-open nil) (flag nil))

    (when (null *open*) (push nodo *open*) (return-from actualiza-open nil) )
    (setq edo-nodo (second nodo)) ;Estado
    (setq costo-nodo (aref (second nodo) 2)) ;Costo

    ;Busca si el estado del sucesor ya se encontraba en open y
    ;actualiza de acuerdo al costo.
    (loop for i from 0 to (1- (length *open*)) do
      (setq edo-open (second (nth i *open*)))
      (setq costo-open (aref (second (nth i *open*)) 2))

      (cond

        ((and (equalp edo-nodo edo-open) (< costo-nodo costo-open) )
          (setf (nth i *open*) nodo) (setq flag t)) ;Si el nodo tiene un mejor costo

        ( (and (equalp edo-nodo edo-open) (>= costo-nodo costo-open)) (setq flag t)) ;Si el estado ya estaba pero el nodo no tiene mejor costo
      );cond
    );loop
    (if (not flag) (push nodo *open*)) ;Si el estado no estaba, se agrega el nodo
  );let
);defun

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
     ((eql metodo :bestfs)
      ;Revisa primero que el estado no esté ni en *open* ni en *memory*
      (when (and  (not (remember-state? estado *open*))  (not (remember-state? estado *memory*)) )
        (push nodo *open*) (reordena-open)) )
      ((eql metodo :star)
      ;Revisa primero que no esté en memoria
        (when (not (remember-state? estado *memory*))
          (actualiza-open nodo) (reordena-open)))
     (T  Nil)))  )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
  (pop  *Open*))

;;=============================================================================
;;EXPAND [ estado]
;;Construye y regresa una lista con todos los descendientes validos de [estado]
;;=============================================================================
(defun expand (estado &optional (star nil))
"Obtiene todos los descendientes válidos de un estado,
aplicando todos los operadores en *ops* en ese mismo órden"
  (let ((descendientes  nil)
	   (nuevo-estado  nil))
     (dolist  (op  *ops*  descendientes)
	      (setq  nuevo-estado  (apply-operator  op estado star))
	    (when (valid-operator?  op  estado)
     (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))


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
  (meta-aux (make-array 2))
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :breadth-first))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  meta-aux)
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
  (meta-aux (make-array 2))
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :depth-first))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  meta-aux)
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

;;========================================================================
;; BÚSQUEDA BEST FIRST
;;========================================================================

(defun bestfs ()

  (reset-all)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2)) ;;auxiliar para revisar si ya se llegó a la meta
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :bestfs))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))
  (setf (aref pos-actual 2) (aptitud pos-actual))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  meta-aux)
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


;;========================================================================
;; ALGORITMO A-ESTRELLA
;;========================================================================

(defun a-estrella ()

  (reset-all)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2)) ;;auxiliar para revisar si ya se llegó a la meta
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :star))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))
  (setf (aref pos-actual 2) (aptitud pos-actual))

   (insert-to-open   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (get-from-open)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (incf *altura*)
     (cond
       ;Si encontró el estado meta
        ((equalp  *goal*  meta-aux)
           (extract-solution  nodo)
           (setq aux-sol (codifica-solucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))

      ;Si todavía no se encuentra el estado meta
          (t (setq  *current-ancestor*  (first  nodo))
   	      (setq  sucesores  (expand estado t))
    			  (setq  sucesores  (filter-memories  sucesores))
    			  (loop for  element  in  sucesores  do
    				  (insert-to-open  (first element)  (second element)  metodo))))))

);defun

(start-maze)
