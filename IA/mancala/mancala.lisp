;;===============================================================
;; Tablero
;; Se representa con una lista de listas
;; cada lista representa una casilla del tablero
;; Las listas en los índices 6 y 13 representan las bases del jugador y
;; de la computadora respectivamente
;;================================================================
(defparameter *tablero* '((r a v) (r a v) (r a v) (r a v) (r a v) (r a v) () (r a v) (r a v) (r a v) (r a v) (r a v) (r a v) ()))


;;================================================================
;; Operaciones
;; Casillas que puede mover la IA
;; Son las casillas con los índices
;; 7, 8, 9, 10, 11, 12
;;================================================================
(defparameter *ops-maquina* '(7 8 9 10 11 12))

;;================================================================
;; turno y repite turno
;; jugador en turno (1-humano, 2-máquina)
;;================================================================
(defparameter *turno* 1)
(defparameter *bool-repite* nil)


;;================================================================
;; profundidad máxima
;;================================================================
(defparameter *profundidad-max* 12)

;;================================================================
;; Función para inicializar las variables globales
;;================================================================
(defun inicializa (prof)
  (setq *tablero* '((r a v) (r a v) (r a v) (r a v) (r a v) (r a v) () (r a v) (r a v) (r a v) (r a v) (r a v) (r a v) ()))
  (setq *ops-maquina* '(7 8 9 10 11 12))
  (setq *turno* 1)
  (setq *bool-repite* nil)
  (setq *profundidad-max* prof)
)

;;================================================================
;; Calcula puntaje
;;================================================================
(defun puntaje (lista)
  "
  Calcula el puntaje de una casilla en el tablero
  Regresa una lista con:
  first: Cadena representando número de elementos de cada tipo
  second: Puntaje total
  "
  (let ((rojo 0) (verde 0) (amarillo 0) (lista-resultado nil) )
    (loop for elem in lista do
      (case elem
        (r (incf rojo) )
        (v (incf verde) )
        (a (incf amarillo) ) )    )
    (setq lista-resultado (list (concatenate 'string "R:" (write-to-string rojo) " V:" (write-to-string verde) " A:" (write-to-string amarillo)  )
      (reduce #'+ (list (* 10 rojo) (* 5 verde)  (* 1 amarillo) )   )) )
    lista-resultado
  );let
);defun

(defun puntaje-final (tablero turno)
  (let ((puntaje1 0) (puntaje2 0) (lista-resultado nil) )
    (setq puntaje1 (second (puntaje (nth 6 tablero))))
    (setq puntaje2 (second (puntaje (nth 13 tablero))))
    (when (= turno 1)
      (loop for i in '(7 8 9 10 11 12) do
        (setq puntaje1 (+ puntaje1 (second (puntaje (nth i tablero))))) ) )
    (when (= turno 2)
      (loop for i in '(0 1 2 3 4 5) do
        (setq puntaje2 (+ puntaje2 (second (puntaje (nth i tablero))))) ) )
  (setq lista-resultado (list puntaje1 puntaje2))
  lista-resultado
  );let
)

;;================================================================
;; Despliega el tablero en la terminal
;;================================================================
(defun despliega-tablero ()
  (format t "~%==================================================================================================~%")
  (format t "Base-PC     Casilla-12     Casilla-11     Casilla-10     Casilla-9     Casilla-8     Casilla-7")
  (format t  "~%~a           ~a    ~a    ~a    ~a   ~a   ~a"
    (second (puntaje (nth 13 *tablero*))) (first (puntaje (nth 12 *tablero*))) (first (puntaje (nth 11 *tablero*))) (first (puntaje (nth 10 *tablero*))) (first (puntaje (nth 9 *tablero*)))
  (first (puntaje  (nth 8 *tablero*)))  (first  (puntaje (nth 7 *tablero*)))  )
  (format t "~%==================================================================================================")
  (format t  "~%~a  ~a   ~a    ~a    ~a   ~a       ~a"
    (first (puntaje (nth 0 *tablero*))) (first (puntaje (nth 1 *tablero*))) (first (puntaje (nth 2 *tablero*))) (first (puntaje (nth 3 *tablero*))) (first (puntaje (nth 4 *tablero*)))
  (first (puntaje  (nth 5 *tablero*)))  (second  (puntaje (nth 6 *tablero*)))  )
  (format t  "~%Casilla-0    Casilla-1     Casilla-2      Casilla-3      Casilla-4     Casilla-5     Base-humano")
  (format t "~%==================================================================================================~%")
);defun

;;================================================================
;; Aplica la jugada sobre el tablero
;;================================================================
(defun aplica-jugada (indice-casilla tablero-orig turno orden-fichas)
  "
  Modifica el tablero repartiendo las fichas en la casilla índice-casilla
  en las casillas vecinas
  Regresa una lista con
  first: nuevo tablero
  second: booleano para saber si se repetirá de turno
  "
  (let ((contenido nil) (aux 0)  (tablero nil) (ultima-casilla 0) (bool-repite nil)  )
    (setq tablero (copy-seq tablero-orig))
    (setq contenido orden-fichas) ;Copia el contenido de la casilla
    (setq aux (+ indice-casilla 1)) ;Primer casilla vecina

    ;Actualiza las  casillas vecinas
    (loop for elem in contenido do
      ;Omite casilla base del jugador humano si fue un movimiento de la máquina
      (if (and (= aux 6) (= turno 2)) (setq aux (mod (1+ aux) 13 )) )
      ;Omite casilla base de la máquina si fue un movimiento del humano
      (if (and (= aux 13) (= turno 1)) (setq aux (- (mod (1+ aux) 13 ) 1)) )

      ;Actualiza tablero
      (setf (nth aux tablero) (append (nth aux tablero) (list elem)) )
      (setq ultima-casilla aux)
      (setq aux (mod (1+ aux) 14)) ;Próxima casilla vecina
    );loop

    ;Revisa si se va a repetir turno
    (if (or (and (= ultima-casilla 6) (= turno 1) ) (and (= ultima-casilla 13) (= turno 2) ) ) (setq bool-repite t) (setq bool-repite nil)  )

    ;Vacía la casilla de movimiento
    (setf (nth indice-casilla tablero) nil)
    (list tablero bool-repite)
  );let
);defun


;;=============================================================================
;; Determina turno
;;=============================================================================
(defun determina-turno (turno-anterior bool-repite)
  (cond
    ((and (= turno-anterior 1) (equal bool-repite t) ) (return-from determina-turno 1) )
    ((and (= turno-anterior 2) (equal bool-repite t) ) (return-from determina-turno 2) )
    ((and (= turno-anterior 1) (equal bool-repite nil) ) (return-from determina-turno 2) )
    ((and (= turno-anterior 2) (equal bool-repite nil) ) (return-from determina-turno 1) ) )
);defun

;;=============================================================================
;; Revisa si se llegó a un estado terminal
;;=============================================================================
(defun es-terminal? (tablero)
  (if (or
    (and (equal (nth 0 tablero) nil) (equal (nth 1 tablero) nil) (equal (nth 2 tablero) nil) (equal (nth 3 tablero) nil) (equal (nth 4 tablero) nil) (equal (nth 5 tablero) nil))
    (and (equal (nth 7 tablero) nil) (equal (nth 8 tablero) nil) (equal (nth 9 tablero) nil) (equal (nth 10 tablero) nil) (equal (nth 11 tablero) nil) (equal (nth 12 tablero) nil)) ) t nil)
);defun

;;============================================================================
;; Valida jugada
;; Las validaciones son:
;; 1. Que no sea el índice de una casilla base (entero entre 0 y 12 excepto 6)
;; 2. Que la casilla no sea nil (al menos tiene una ficha)
;; 3. El índice de la casilla seleccionada no sea una casilla del oponente
;;============================================================================
(defun valida-jugada (indice-casilla tablero turno)
  "
  Valida una jugada POR HACER
  indice-casilla: índice de la casilla que se desea jugar
  tablero: estado del tablero antes de realizar la jugada
  "
  (let ((prueba1 nil) (prueba2 nil) (prueba3 nil) )
    ;prueba 1
    (if (and (>= indice-casilla 0) (<= indice-casilla 12) (/= indice-casilla 6) (integerp indice-casilla) ) (setq prueba1 t) (return-from valida-jugada nil)  )

    ;prueba 2
    (if (not (equal (nth indice-casilla tablero) nil)) (setq prueba2 t) (return-from valida-jugada nil)  )

    ;prueba 3
    (if (or (and (>= indice-casilla 0) (<= indice-casilla 5) (= turno 1) )
      (and (>= indice-casilla 7) (<= indice-casilla 12) (= turno 2) ) ) (setq prueba3 t) (return-from valida-jugada nil) )
    (if (and prueba1 prueba2 prueba3) t nil)
  );let
);defun

;;===========================================================================
;; Función para evaluar el tablero
;; da prioridad a los movimientos que permiten tener turnos consecutivos
;; si no hay casillas que proporcionen una repetición en el turno
;; se da prioridad al tablero de acuerdo a las funciones de similitud
;;===========================================================================

(defun similitud (tablero)
  (/  (+ (second (puntaje (nth 6 tablero) )) (second (puntaje (nth 13 tablero) )) ) 192  )
);defun

(defun similitud2 (tablero)
  "
  Calcula la diferencia entre el estado actual de las casillas y el estado
  objetivo (aquel que te permitiría ganar en un sólo turno)
  "
  (let ((dist-cpu 0) (dist-hum 0) (casillas-cpu nil) (casillas-humano nil)
           (objetivo '(1 2 3 4 5 6)  ) )

         ;obtiene el estado de las casillas del humano
         (loop for i in '(5 4 3 2 1 0) do (setq casillas-humano (append casillas-humano (list (length (nth i tablero) ) )))  )

         ;obtiene el estado de las casillas del CPU
         (loop for i in '(12 11 10 9 8 7) do (setq casillas-cpu (append casillas-cpu (list (length (nth i tablero) ) )))  )

         ;obtiene la distancia entre casillas del humano y casillas objetivo
         (setq dist-hum (reduce #'+ (mapcar #'abs (mapcar #'- casillas-humano objetivo) ) ))

         ;obtiene la distancia entre casillas de la mánquina y casillas objetivo
         (setq dist-cpu (reduce #'+ (mapcar #'abs (mapcar #'- casillas-cpu objetivo) ) ))


         (/ 1 (+ 1 dist-cpu))
  );let
);defun

(defun evalua-tablero (tablero turno)
  (if (es-terminal? tablero) (return-from evalua-tablero 1000) )
  (when (= turno 1)
    (cond
      ( (= (length (nth 0 tablero)) 6)  (return-from evalua-tablero 590)  )
      ((= (length (nth 1 tablero)) 5)  (return-from evalua-tablero 690)  )
      ((= (length (nth 2 tablero)) 4)  (return-from evalua-tablero 790)  )
      ((= (length (nth 3 tablero)) 3)  (return-from evalua-tablero 890)  )
      ((= (length (nth 4 tablero)) 2)  (return-from evalua-tablero 990)  )
      ((= (length (nth 5 tablero)) 1)  (return-from evalua-tablero 1000)  )
      (t (return-from evalua-tablero (similitud2 tablero)  ) ) );cond
  );when turno 1

  (when (= turno 2)
    (cond
      ((= (length (nth 12 tablero)) 1)  (return-from evalua-tablero 1000)  )
      ((= (length (nth 11 tablero)) 2)  (return-from evalua-tablero 990)  )
      ((= (length (nth 10 tablero)) 3)  (return-from evalua-tablero 890)  )
      ((= (length (nth 9 tablero)) 4)  (return-from evalua-tablero 790)  )
      ((= (length (nth 8 tablero)) 5)  (return-from evalua-tablero 690)  )
      ((= (length (nth 7 tablero)) 6)  (return-from evalua-tablero 590)  )
      (t (return-from evalua-tablero (similitud2 tablero)  ) ) );cond
  );when turno 2
);defun



;;============================================================================
;; Actualiza el tablero final
;;============================================================================
(defun tablero-final (tablero turno-ganador)
  (when (= turno-ganador 1)
    (loop for i in '(7 8 9 10 11 12) do
       (when (nth i tablero)
         (loop for elem in (nth i tablero) do
           (setf (nth 6 tablero) (append (nth 6 tablero) (list elem) ) ) ) )
           (setf (nth i tablero) nil)  ) )

  (when (= turno-ganador 2)
   (loop for i in '(0 1 2 3 4 5) do
      (when (nth i tablero)
        (loop for elem in (nth i tablero) do
          (setf (nth 13 tablero) (append (nth 13 tablero) (list elem) ) ) )
          (setf (nth i tablero) nil)   )  ) )
  tablero
);defun

;;=============================================================================
;; Función ordena-fichas
;; Orden las fichas de una casilla de mayor a menor valor (rojo-verde-amarillo)
;; si se repite el turno entonces las ordena de menor a mayor (amarillo-verde-rojo)
;aplica-jugada (indice-casilla tablero-orig turno orden-fichas)
;;=============================================================================
(defun ordena-fichas (casilla indice-casilla tablero turno)
  (let ((conteo-rojo 0) (conteo-verde 0) (conteo-amarillo 0) (resultado nil) (bool-repite nil) )
    (setq conteo-rojo (count 'R casilla))
    (setq conteo-verde (count 'V casilla))
    (setq conteo-amarillo (count 'A casilla))
    (setq bool-repite (second (aplica-jugada indice-casilla tablero turno casilla)))

    (when (not bool-repite )
      (loop for i from 0 to (1- conteo-rojo) do (setq resultado (append resultado (list 'R))) )
      (loop for i from 0 to (1- conteo-verde) do (setq resultado (append resultado (list 'V))) )
      (loop for i from 0 to (1- conteo-amarillo) do (setq resultado (append resultado (list 'A))) )
    )

    (when bool-repite
      (loop for i from 0 to (1- conteo-amarillo) do (setq resultado (append resultado (list 'A))) )
      (loop for i from 0 to (1- conteo-verde) do (setq resultado (append resultado (list 'V))) )
      (loop for i from 0 to (1- conteo-rojo) do (setq resultado (append resultado (list 'R))) )
    )

  resultado
  );let
)

;;============================================================================
;; Negamax alfa beta
;;============================================================================
(defun abnegamax (tablero profundidad turno alfa beta  &optional (profundidad-max *profundidad-max*) )
  "
  Algoritmo negamax
  regresa una lista con dos elementos
  first: mejor-valor
  second: mejor-movimiento
  "
  (let ((lista-resultado nil) (mejor-movimiento nil) (mejor-puntuacion nil) (nuevo-tablero nil)
        (puntaje-actual nil) (lista-movimientos nil)
        (lista-aux nil) (nuevo-turno nil) (bool-repite nil) (orden-fichas nil) )

        ;Revisa si es estado terminal o se llegó a la profundidad máxima
        (when (or (es-terminal? tablero) (>= profundidad profundidad-max)  )
          (setq lista-resultado (list (evalua-tablero tablero turno) nil) )
          (return-from abnegamax lista-resultado) )

        ;(format t "Profundidad = ~a ~%" profundidad)
        ;(sleep 0.5)
        (setq mejor-movimiento  nil)
        (setq mejor-puntuacion -1000000000)

        ;Obtiene los movimientos permitidos en el estado actual del tablero
        (loop for i in '(0 1 2 3 4 5 7 8 9 10 11 12) do
          (if (valida-jugada i tablero turno)
            (setq lista-movimientos (append lista-movimientos (list i))) ) )

        (loop for movimiento in lista-movimientos do
          (setq orden-fichas (ordena-fichas (nth movimiento tablero) movimiento tablero turno))
          (setq lista-aux (aplica-jugada movimiento tablero turno orden-fichas))
          (setq nuevo-tablero (first lista-aux))
          (setq bool-repite (second lista-aux))
          (setq nuevo-turno (determina-turno turno bool-repite))

          (setq lista-resultado (abnegamax nuevo-tablero (1+ profundidad) nuevo-turno (if bool-repite alfa (* -1 beta) )
          (if bool-repite beta (* -1 (max alfa mejor-puntuacion)) )  ))

          ;Considera la repetición de turnos
          ;si no repite se multiplica por -1
          (setq puntaje-actual (first lista-resultado))
          (if  (not bool-repite) (setq puntaje-actual (* -1 puntaje-actual)) )

          ;actualiza puntajes
          (when (> puntaje-actual mejor-puntuacion)
            (setq mejor-puntuacion puntaje-actual)
            (setq mejor-movimiento movimiento)

            ;Revisa la condición alfa-beta
            (when (>= mejor-puntuacion beta) (setq lista-resultado (list mejor-puntuacion mejor-movimiento))
              (return-from abnegamax lista-resultado)) )

        ) ;loop

      (setq lista-resultado (list mejor-puntuacion mejor-movimiento))
      lista-resultado
  );let
);defun

;;============================================================================
;; valida fichas a jugar
;; cuenta que se mueva el mismo número de fichas considerando el tipo
;;============================================================================
(defun valida-fichas (casilla-tablero lista-orden)
  "
  casilla-tablero: casilla del tablero que se modificará
  orden: en que orden de colocarán las fichas de casilla-tablero en las
  casillas vecinas
  "
  (if (and (= (count 'R casilla-tablero) (count 'R lista-orden)  )
           (= (count 'A casilla-tablero) (count 'A lista-orden)  )
           (= (count 'V casilla-tablero) (count 'V lista-orden)  )) (return-from valida-fichas t) nil)
)

;;============================================================================
;; inicia el juego
;; jugadores==1 humano vs máquina
;; flag-ordena==nil  el humano establece el orden de distribución de las fichas
;; flag-ordena==t el orden de las fichas es determinado por ordena-fichas
;;============================================================================
(defun main (&optional (jugadores 1) (flag-ordena nil) (profundidad 10) )
  (let ((jugador1 nil) (jugador2 nil) (puntaje1 0) (puntaje2 0) (lista-aux nil) (lista-puntajes nil)
         (orden-fichas nil)  )
    (inicializa profundidad)
    (setq *turno* 2)
    (despliega-tablero)

    (loop

      (when (and (= *turno* 1) (= jugadores 1) (not (es-terminal? *tablero*)) )
      (format t "~%Turno de jugador ~a: " *turno*)
      (format t "~%Elige la casilla~%")
      (setq orden-fichas nil)
        ;Valida casilla
        (loop
           (setq jugador1 (read))
           (if (not (valida-jugada jugador1 *tablero* *turno*)) (format t "~%Jugada no válida, elige otra opción "))
           (when (valida-jugada jugador1 *tablero* *turno*) (return t)) );loop


      ;Pide el orden de las fichas
      (when (not flag-ordena)
        (format t "~%Elige el orden de distribución de las fichas las fichas (escribelo como lista, ej, '(R V A) :~%")
          (loop
            (setq orden-fichas  (eval (read)))
            (if (not (valida-fichas (nth jugador1 *tablero*) orden-fichas)) (format t "~%Fichas no válidas, revisa tus fichas"))
            (when (valida-fichas (nth jugador1 *tablero*) orden-fichas) (return t) ) )
      );when

      ;ordena las fichas de manera automática
    (if flag-ordena (setq orden-fichas (ordena-fichas (nth jugador1 *tablero*) jugador1 *tablero* *turno*)) )

        (setq lista-aux (aplica-jugada jugador1 *tablero* *turno* orden-fichas))
        (setq *tablero* (first lista-aux)) ;aplica la jugada
        (setq *bool-repite* (second lista-aux))

        (despliega-tablero)
        (format t "~%El jugador ~a modificó la casilla ~a ~%~%" *turno* jugador1)
        (if *bool-repite* (format t "~%Jugador ~a vuelve a jugar~%~%" *turno*))

        (if (not *bool-repite*) (setq *turno* 2) (setq *turno* 1) ) ;Verifica si se repite turno
      );when (jugador 1 - humano)

    (when (and (= *turno* 1) (= jugadores 0)  (not (es-terminal? *tablero*)) )
      (format t "~%Turno de jugador ~a:~%" *turno*)
      (setq jugador1 (second (abnegamax *tablero* 0 *turno* -1000 1000)))
      (setq orden-fichas (ordena-fichas (nth jugador1 *tablero*) jugador1 *tablero* *turno*))
      (setq lista-aux (aplica-jugada jugador1 *tablero* *turno* orden-fichas))
      (setq *tablero* (first lista-aux)) ;aplica jugada
      (setq *bool-repite* (second lista-aux))

      (despliega-tablero)
      (format t "~%El jugador ~a modificó la casilla ~a ~%" *turno* jugador1)
      (if *bool-repite* (format t "~%Jugador ~a vuelve a jugar~%" *turno*))

      (if (not *bool-repite*) (setq *turno* 2) (setq *turno* 1) ) ;Verifica si se repite turno
    );when maquina vs maquina

    (when  ( and (= *turno* 2) (not (es-terminal? *tablero*)) )
      (format t "~%Turno de jugador ~a:~%" *turno*)
      (setq jugador2 (second (abnegamax *tablero* 0 *turno* -1000 1000)))
      (setq orden-fichas (ordena-fichas (nth jugador2 *tablero*) jugador2 *tablero* *turno*))
      (setq lista-aux (aplica-jugada jugador2 *tablero* *turno* orden-fichas))
      (setq *tablero* (first lista-aux)) ;aplica jugada
      (setq *bool-repite* (second lista-aux))

      (despliega-tablero)
      (format t "~%El jugador ~a modificó la casilla ~a ~%" *turno* jugador2)
      (if *bool-repite* (format t "~%Jugador ~a vuelve a jugar~%" *turno*))

      (if (not *bool-repite*) (setq *turno* 1) (setq *turno* 2) ) ;Verifica si se repite turno
    );when (humano-maquina)

    (when (es-terminal? *tablero*) (return t))
  );loop
  (if (not *bool-repite*) (setq *turno* (determina-turno *turno* *bool-repite*) )) ;turno ganador
  (setq lista-puntajes (puntaje-final *tablero* *turno*))
  (setq puntaje1 (first lista-puntajes))
  (setq puntaje2 (second lista-puntajes))
  (setq *tablero* (tablero-final *tablero* *turno*))
  (despliega-tablero)
  (format t "~%Fin del juego: la puntuación es ~a para jugador 1 y ~a para jugador 2~%" puntaje1 puntaje2)
  );let
);defun
