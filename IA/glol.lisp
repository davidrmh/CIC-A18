;;==================================
;;PROBLEMA GLOL
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

;;==============================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará una lista de la siguiente forma
;; '(('H 'L 'O 'C 1) (nil nil nil nil 0))
;; El valor de NIL representa un espacio libre
;; en donde 'H es hombre, L es lobo, O es oveja, C es comida
;; y 1 quiere decir que el bote está en esa orilla.
;; Cada elemento tiene un índice fijo en las dos orillas
;;===============================================

;;======================================
;; ESTADO INICIAL
;; '(('H 'L 'O 'C 1) (nil nil nil nil 0))
;; ESTADO META
;; '((nil nil nil nil 0) ('H 'L 'O 'C 1))
;;=======================================

;;==================================================================
;; OPERADORES
;; Los operadores tendrán la forma (Elemento1 Elemento2)
;; por ejemplo para el operador 'Hombre-con-Lobo ('H 'L)

;; VALIDACIÓN DE OPERADORES
;; Un operador es válido si se cumple lo siguiente
;; 1. Los elementos a transportar están en la misma orilla
;; y además
;; 2. El bote está en la misma orilla que los elementos a transportar
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
(defparameter *ops* '((:Hombre-solo (H H))
                      (:Hombre-oveja (H O))
                      (:Hombre-comida (H C))
                      (:Hombre-lobo (H L))
                       ))

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
;; 1. Los elementos a transportar están en la misma orilla
;; y además
;; 2. El bote está en la misma orilla que los elementos a transportar
;;=======================================================================

(defun valid-operator?(op estado)
  (let ((orilla-izq nil) (orilla-der nil) (elem1 nil) (elem2 nil))
    (setq orilla-izq (first estado));orilla izquierda
    (setq orilla-der (second estado));orilla derecha
    (setq elem1 (first (second op)));elemento 1
    (setq elem2 (second (second op)));elemento 2
      (cond
        ((and (member elem1 orilla-izq) (member elem2 orilla-izq) (= (nth 4 orilla-izq) 1)) t)
        ((and (member elem1 orilla-der) (member elem2 orilla-der) (= (nth 4 orilla-der) 1)) t)
        (t nil)
        );cond
    );let
);defun
;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;(:Hombre-solo ('H nil))
;;(:Hombre-oveja ('H 'O))
;;(:Hombre-comida ('H 'C))
;;(:Hombre-lobo ('H 'L))
;;=======================================================================
(defun apply-operator(op estado)
"Aplica la operación op al estado"
  (let ((etiqueta nil)  (copia-estado nil) (bote-izq? nil))
    (setq etiqueta (first op));etiqueta humana
    (setq copia-estado (copy-tree estado));copia estado
    (setq bote-izq? (if (= (nth 4 (first copia-estado)) 1) t nil)) ;Está el bote en el lado izquierdo?

    (case etiqueta
      (:Hombre-solo (cond
        (bote-izq? (setf (car (car copia-estado)) nil) (setf (car (cadr copia-estado)) 'H)
         (setf (fifth (car copia-estado)) 0) (setf (fifth (cadr copia-estado)) 1)  ) ;pasa a la derecha

        ((not bote-izq?) (setf (first (cadr copia-estado)) nil) (setf (first (car copia-estado)) 'H)
          (setf (fifth (cadr copia-estado)) 0) (setf (fifth (car copia-estado)) 1) ) ;pasa a la izquierda
        ))
      (:Hombre-oveja (cond
        (bote-izq? (setf (first (car copia-estado)) nil) (setf (first (cadr copia-estado)) 'H)
          (setf (third (car copia-estado)) nil) (setf (third (cadr copia-estado)) 'O)
          (setf (fifth (car copia-estado)) 0) (setf (fifth (cadr copia-estado)) 1)) ;pasan a la derecha

        ((not bote-izq?) (setf (first (cadr copia-estado)) nil) (setf (first (car copia-estado)) 'H)
          (setf (third (cadr copia-estado)) nil) (setf (third (car copia-estado)) 'O)
          (setf (fifth (cadr copia-estado)) 0) (setf (fifth (car copia-estado)) 1) ) ;pasan a la izquierda
        ))

        (:Hombre-comida (cond
          (bote-izq? (setf (first (car copia-estado)) nil) (setf (first (cadr copia-estado)) 'H)
            (setf (fourth (car copia-estado)) nil) (setf (fourth (cadr copia-estado)) 'C)
            (setf (fifth (car copia-estado)) 0) (setf (fifth (cadr copia-estado)) 1)) ;pasan a la derecha

          ((not bote-izq?) (setf (first (cadr copia-estado)) nil) (setf (first (car copia-estado)) 'H)
            (setf (fourth (cadr copia-estado)) nil) (setf (fourth (car copia-estado)) 'C)
            (setf (fifth (cadr copia-estado)) 0) (setf (fifth (car copia-estado)) 1) ) ;pasan a la izquierda
          ))

          (:Hombre-lobo (cond
            (bote-izq? (setf (first (car copia-estado)) nil) (setf (first (cadr copia-estado)) 'H)
              (setf (second (car copia-estado)) nil) (setf (second (cadr copia-estado)) 'L)
              (setf (fifth (car copia-estado)) 0) (setf (fifth (cadr copia-estado)) 1)) ;pasan a la derecha

            ((not bote-izq?) (setf (first (cadr copia-estado)) nil) (setf (first (car copia-estado)) 'H)
              (setf (second (cadr copia-estado)) nil) (setf (second (car copia-estado)) 'L)
              (setf (fifth (cadr copia-estado)) 0) (setf (fifth (car copia-estado)) 1) ) ;pasan a la izquierda
            ))
          (t "ERROR"));case
    copia-estado
  );let
);defun

;;=================================================================
;; FUNCIÓN PARA VALIDAR EL ESTADO
;; valida-estado
;; Un estado es válido si
;; 1. La oveja y el lobo no están solos en alguna orilla y no está el hombre
;; o
;; 2. La oveja y la comida no están solos en alguna orilla y no está el hombre

(defun valida-estado(estado)
  (let ((orilla-izq nil) (orilla-der nil) (flag-olh nil) (flag-och nil))
    (setq orilla-izq (first estado));orilla izquierda
    (setq orilla-der (second estado));orilla derecha

    ;determina el caso oveja-lobo-hombre
    (setq flag-olh (or
      (and (member 'O orilla-izq) (member 'L orilla-izq) (not (member 'H orilla-izq)))
      (and (member 'O orilla-der) (member 'L orilla-der) (not (member 'H orilla-der))) ))

    ;determina el caso oveja-comida-hombre
    (setq flag-och (or
      (and (member 'O orilla-izq) (member 'C orilla-izq) (not (member 'H orilla-izq)))
      (and (member 'O orilla-der) (member 'C orilla-der) (not (member 'H orilla-der))) ))

    (cond
      ((or flag-olh flag-och) nil)
      (t t))
  );let
);defun

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
		     (when ( and (valid-operator?  op  estado) (valida-estado nuevo-estado))
         (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))

;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"RECURSIVA:
Busca un estado en una lista de nodos que sirve como memoria de intentos previos
el estado tiene estructura:  '(('H 'L 'O 'C 1) (nil nil nil nil 0)),
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
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLU(setq  sucesores  (expand estado))TION
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

(defun  main-glol (edo-inicial  edo-meta  metodo)
";; ESTADO INICIAL
;; '((H L O C 1) (nil nil nil nil 0))
;; ESTADO META
;; '((nil nil nil nil 0) (H L O C 1))
;; (main-glol '((H L O C 1) (nil nil nil nil 0)) '((nil nil nil nil 0) (H L O C 1)) :depth-first)
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
         (format t "~a~%" estado)
  	      (setq  sucesores  (expand estado))
   			  (setq  sucesores  (filter-memories  sucesores))
   			  (loop for  element  in  sucesores  do
   				(insert-to-open  (first element)  (second element)  metodo)))))))

;;=======================================================================
;;=======================================================================
