;;=============================================================
;;                  Misioneros y Caníbales
;;  Estructura de los estados: 
;;    *Lista con dos sublistas internas, una por cada orilla.
;;    *En cada orilla, número de grajeros, lobos, ovejas, legumbres y barcas
;;                   M C B   M C B
;;                 ((3 3 1) (0 0 0))
;;               
;;                      Yair Velasco
;;============================================================


(defparameter  *open* '())     ;; Frontera de búsqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos

(defparameter  *ops*  
  '( (:Dos-Misioneros           (2 0))  ;; Operadores para el problema de misioneros y canibales
     (:Un-Misionero             (1 0))
     (:Misionero-y-Caníbal      (1 1))
     (:Un-Caníbal               (0 1))
     (:Dos-Caníbales            (0 2))))

(defparameter  *id*  0)  ;; Identificador del último nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria
(defparameter  *num-nodos* 0)  ;;Número de nodos creados
(defparameter  *num-expand* 0) ;;Número de nodos expandidos

;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;             op - El operador cuya aplicación generó el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo que contiene al estado y operador recibidos como parámetro "
  (incf *num-nodos*)
  (list  (incf  *id*)  estado  *current-ancestor*  (first op)))  ;

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
;;  BARGE-SHORE [estado]
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;=======================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  0 - origen  1 - destino"
     (if  (= 1 (third (first  estado)))  0  1))


;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla donde esta la barca
;;=======================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."  
  (let*  ((orilla  (barge-shore  estado))                         
	    (misioneros  (first (nth orilla estado)))
	    (canibales  (second (nth orilla  estado))))
	    (and (>= misioneros (first (second op)))
	         (>= canibales (second (second op))))
 ))

;;=======================================================================
;;  VALID-STATE [estado]
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                            Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;=======================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema..."
    (let* ((m0  (first (first estado)))  
	   (c0  (second (first estado)))
	   (m1  (first (second estado)))  
	   (c1  (second (second estado))))

	   (and  (or (>= m0 c0)(zerop m0))
	         (or (>= m1 c1)(zerop m1)))))
    
;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;=======================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (m0  (first orilla1))
   	       (c0   (second orilla1))
	       (b0   (third  orilla1))
	       (m1  (first  orilla2))
	       (c1   (second  orilla2))
	       (b1   (third   orilla2))
	       (orilla-barca  (barge-shore estado)) 
	       (operador (first op)))      ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Dos-Misioneros (if (= orilla-barca 0)  ;;siempre  restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	                        (list  (list  (- m0 2) c0 (flip b0))   (list  (+ m1 2) c1 (flip b1)))
				(list  (list  (+ m0 2) c0 (flip b0))  (list (- m1 2) c1 (flip b1)))))
	    (:Un-Misionero   (if (= orilla-barca 0)  
	                        (list  (list  (- m0 1) c0 (flip b0))   (list  (+ m1 1) c1 (flip b1)))
				(list  (list  (+ m0 1) c0 (flip b0))  (list (- m1 1) c1 (flip b1))))) 
	    (:Misionero-y-Caníbal  
	                 (if (= orilla-barca 0)  
	 		        (list  (list  (- m0 1) (- c0 1) (flip b0))   (list  (+ m1 1) (+ c1 1) (flip b1)))
				(list  (list  (+ m0 1) (+ c0 1) (flip b0))  (list (- m1 1) (- c1 1) (flip b1)))))
	    (:Un-Caníbal (if (= orilla-barca 0)  
			   (list  (list  m0  (- c0 1) (flip b0))   (list  m1  (+ c1 1) (flip b1)))
			   (list  (list  m0  (+ c0 1) (flip b0))  (list  m1  (- c1 1) (flip b1))))) 
	    (:Dos-Caníbales  (if (= orilla-barca 0)  
	                        (list  (list  m0  (- c0 2) (flip b0))   (list  m1 (+ c1 2) (flip b1)))
				(list  (list  m0 (+ c0 2) (flip b0))  (list m1 (- c1 2) (flip b1)))))
	    (T "error"))))


;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	   (nuevo-estado  nil))
       (incf *num-expand*)
       (dolist  (op  *ops*  descendientes)
	         (setq  nuevo-estado  (apply-operator  op estado))
		 (when (and (valid-operator?  op  estado) (valid-state?  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes)) )))) 


;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )

(defun Recuerdo? (estado listanodos)

  (cond ((null listanodos) nil)
	((equal estado (second (first listanodos))) T)
	(T (Recuerdo? estado (rest listanodos)))))

(defun  filter-memories (listaestados) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
;;     (format t "~A +++++~%" (first (first listaestados)))
     (cond ((null  listaestados)  Nil)
	   ((Recuerdo? (first (first listaestados)) *memory*)
	      (filter-memories (rest listaestados) ))
	   (T (cons (first listaestados)
		    (filter-memories (rest listaestados))))))

;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		         (cond ((null  lista)  Nil)
			           ((eql  id  (first (first  lista))) (first  lista))
				   (T  (locate-node  id (rest  lista))))))
	      (let ((current  (locate-node  (first  nodo)  *memory*)))
		    (loop  while  (not (null  current))  do                        
			 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
			 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	      *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~A\) aplicando ~A se llega al estado  ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;=======================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solucion del problema...
;;=======================================================================
(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *num-nodos* 0)
     (setq  *num-expand* 0))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                                             :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil) (estado nil) (sucesores  '()) (operador -1) (meta-encontrada nil))

    (insert-to-open   edo-inicial  nil  metodo)
    (loop until  (or  meta-encontrada  (null *open*))  do
	 (setq  nodo (get-from-open)
		estado  (second  nodo)
		operador  (third  nodo))
	   (push  nodo  *memory*)
	   (cond    ((equal  edo-meta  estado)  
		     (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		     (format t "Nodos creados: ~A ~%" *num-nodos*)
		     (format t "Nodos expandidos: ~A ~%" *num-expand*)
		            (display-solution  (extract-solution  nodo))
		            (setq  meta-encontrada  T))
		     (T (setq  *current-ancestor*  (first  nodo)) 
			(setq  sucesores  (expand estado))
			(setq  sucesores  (filter-memories  sucesores))
	
			(loop for  element  in  sucesores  do
			     (insert-to-open  (first element)  (second element)  metodo)
			     )) ))))
			     
     
;;=======================================================================
;;=======================================================================
