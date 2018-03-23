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

;;===================================================
;; VARIABLES GLOBALES
;;===================================================
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
  (let ((nueva-pos #(0 0)) (mov-reng 0) (mov-col 0) )
    (setq mov-reng (first (second op))) ;moviento en renglón
    (setq mov-col (second (second op))) ;moviento en columna
    (setf (aref nueva-pos 0) (+ (aref pos-actual 0) mov-reng)) ;nuevo renglón
    (setf (aref nueva-pos 1) (+ (aref pos-actual 1) mov-col)) ;nueva columna
  nueva-pos
  );let
);defun
