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
;; turno
;; jugador en turno (1-humano, 2-máquina)
;;================================================================
(defparameter *turno* 1)

;;================================================================
;; Aplica la jugada sobre el tablero
;;================================================================
(defun aplica-jugada (indice-casilla)
  "
  Modifica el tablero repartiendo las fichas en la casilla índice-casilla
  en las casillas vecinas
  "
  (let ((contenido nil) (aux 0)  (tablero nil))
    (setq tablero (copy-seq *tablero*))
    (setq contenido (copy-seq (nth indice-casilla tablero))) ;Copia el contenido de la casilla
    (setq aux (+ indice-casilla 1)) ;Primer casilla vecina

    ;Actualiza las  casillas vecinas
    (loop for elem in contenido do
      ;Omite casilla base del jugador humano si fue un movimiento de la máquina
      (if (and (= aux 6) (= *turno* 2)) (setq aux (mod (1+ aux) 13 )) )
      ;Omite casilla base de la máquina si fue un movimiento del humano
      (if (and (= aux 13) (= *turno* 1)) (setq aux (- (mod (1+ aux) 13 ) 1)) )

      ;Actualiza tablero
      (setf (nth aux tablero) (append (nth aux tablero) (list elem)) )
      (setq aux (mod (1+ aux) 13)) ;Próxima casilla vecina
    );loop

    ;Vacía la casilla de movimiento
    (setf (nth indice-casilla tablero) nil)
    tablero
  );let
);defun
