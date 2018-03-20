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
;; Función para leer los datos y organizarlos en un arreglo
;; Los datos originales vienen en un archivo .lisp
;;=========================================================
(defun crea-arreglo(ruta-archivo)
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
