;;==============================================================================
;; Función para leer los datos
;; ENTRADA
;; nombre-archivo. String. Ruta de los datos
;;
;; SALIDA
;; datos. Lista. Una lista con cada elemento representando una línea del archivo
;;
;; ESTRUCTURA DEL ARCHIVO
;; El archivo debe de tener la siguiente estructura:
;; (val11 val12 val13...)
;; (val21 val22 val23...)
;;==============================================================================
(defun lee-datos (nombre-archivo)
  (let ((archivo nil) (datos nil) )
    (setq archivo (open nombre-archivo))
    (loop for linea = (read archivo nil nil) while linea do
      (setq datos (append datos (list linea)))
    );loop
    (close archivo)
    datos);let
);defun

;;==============================================================================
;; Función para obtener una lista con las clases posibles (sin repeticiones)
;;
;; ENTRADA
;; datos: Lista. Lista creada con la función lee-datos
;; indice: Entero. Índice (iniciando en 0) de la columna que tiene la clase
;;
;; SALIDA
;; clases: Lista. Lista con las clases en el conjunto de datos
;;==============================================================================
(defun obten-clases (datos indice)
  (let ((clases nil) (clase-aux nil))
    (loop for observacion in datos do
      (setq clase-aux (nth indice observacion))
      (if (not (find clase-aux clases)) (setq clases (append clases  (list clase-aux)))) );loop
  clases
  );let
);defun

;;==============================================================================
;; Función para clasificar ejemplos positivos y negativos de acuerdo a una
;; clase dada
;;
;; ENTRADA
;; datos: Lista. Lista creada con la función lee-datos
;; clase: Símbolo. Símbolo que representa la clase positiva. Debe de ser algún
;; símbolo que regresa la función obten-clases
;; indice: Entero. Índice (iniciando en 0) de la columna que tiene la clase
;;
;;
;; SALIDA
;; Una lista con los siguiente elementos
;; (first) positivas. Lista. Lista (subconjuto de datos) con las observaciones
;; que corresponden a la clase positiva
;; (second) negativas. Lista Lista (subconjuto de datos) con las observaciones
;; que corresponden a la clase negativa
;;==============================================================================
(defun separa-clases (datos clase indice)
  (let ((positivas nil) (negativas nil) (clase-aux nil)  )
    (loop for observacion in datos do
      (setq clase-aux (nth indice observacion))
      (if (equal clase-aux clase)
        (setq positivas (append positivas  (list observacion)))
        (setq negativas (append negativas (list observacion)))) );loop
  (list positivas negativas)
  );let
);defun
