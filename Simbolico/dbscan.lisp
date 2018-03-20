;;Necesito una variable global *vecinos* para ir agregando
;;las observaciones que estén a distancia menor a epsilon de la
;;observación actual.
;;Cuando se examina un renglón completo, hacer pop de *vecinos* para saber
;;que vecino toca revisar.
;;Cuando *vecinos* sea null, entonces iniciar con otro grupo (incf *grupo*)
;;Necesito una *memoria* para no agregar observaciones que ya se agregaron a
;;*vecinos* en algún momento.

;;Si *vecinos* es null y además *memoria* contiene todas las observaciones
;; (sus índices), entonces terminar. En otro caso buscar el siguiente cluster
;; iniciando con una observación que no esté en *memoria*

;;Detectar patrones centrales

;;Patrón central en su e-vecindad tiene nmin elementos

;;=========================================================
;; Función para leer los datos y organizarlos en una lista
;; Los datos originales vienen en un archivo .lisp
;;=========================================================
(defun crea-lista(ruta-archivo)
"Función para leer el archivo de UCI
ENTRADA
ruta-archivo: cadena con la ruta del archivo de UCI (chorales.lisp)
SALIDA:
arreglo: una lista de listas conteniendo la información arreglada.
"
  (let ((linea nil) (arreglo nil) (archivo nil) (nobs nil))
    (setq archivo (open ruta-archivo))
    (loop for i from 1 to 100 do
      (setq linea (rest (read archivo)))
      (setq nobs (length linea));número de observaciones en la línea
      ;modifica la observación j
      (loop for j from 0 to (1- nobs) do
        (loop for k from 0 to 5 do
          (setf (nth k (nth j linea)) (second (nth k (nth j linea))) ));loop k
        (setq arreglo (append arreglo (list (nth j linea)))));loop j
    );loop i
  (close archivo)
  arreglo));defun

;;===============================================
;; Función de distancia
;; Dada la naturaleza de los datos
;; utilizar la métrica manhattan resulta apropiado
;;================================================
(defun manhattan(obs1 obs2)
  (reduce #'+ (mapcar #'abs (mapcar #'- obs1 obs2)))
);defun

;;===============================================
;; Calcula la matriz de distancias
;; Sólo calcula la matriz triangular inferior
;;================================================
(defun matriz-distancias(lista)
  "Crea la matriz de distancias
  ENTRADA:
  lista: lista creada con la función crea-lista
  SALIDA:
  mat-dist: arreglo que representa la matriz de distancias
  "
  (let ((mat-dist nil) (dimension 0) (obs1 nil) (obs2 nil))
    (setq dimension (length lista))
    (setq mat-dist (make-array (list dimension dimension) :initial-element 0))
    (loop for i from 1 to (1- dimension) do
      (setq obs1 (nth i lista))
      (loop for j from 0 to (1- i) do
        (setq obs2 (nth j lista))
        (setf (aref mat-dist i j) (manhattan obs1 obs2))
        );loop j
      );loop i
      mat-dist
    );let
);defun
