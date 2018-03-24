;;==================================
;;Laberintos 2D
;;David Ricardo Montalván Hernández
;;Marzo 2018
;;==================================

(load "maze_lib.lisp")
;;=================================================
;; REPRESENTACIÓN DE LOS ESTADOS
;; Se utilizará un arreglo de la forma #(i j)
;; (aref pos-actual 0) => renglón
;; (aref pos-actual 1) => columna
;; (setf (aref pos-actual 0) (aref *start* 0))
;;  #(i j) representa la posición en el arreglo *maze*
;; del archivo maze_lib.lisp
;;=================================================

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
(defparameter *data-maze* (slot-value *maze* 'data)) ;Datos del laberinto
(defparameter *maze-rows* (1- (get-maze-rows))) ;Renglones (índices) laberinto
(defparameter *maze-cols* (1- (get-maze-cols))) ;Columnas (índices) laberinto

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
(defun representa-binario(numero)
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
;; VALIDA OPERADOR
;; Función para validar un operador
;;=======================================================================
(defun valida-op(op pos-actual)
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
      (return-from valida-op nil));if


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
(defun apply-operator(op pos-actual)
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
