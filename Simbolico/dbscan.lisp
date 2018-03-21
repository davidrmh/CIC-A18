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
;; Variables globales auxiliares
;;=========================================================

(defparameter *con-grupo* nil) ;lista
(defparameter *centrales* nil) ;lista
(defparameter *vecinos* nil) ;lista
(defparameter *tabla* nil) ;arreglo
(defparameter *grupo* 1) ;número
(defparameter *dist-max* 0);número (máximo fue 465)
(defparameter *dist-min* 1000000000);número (mínimo fue 1)
(defparameter *ignorar* nil) ;Los 25 que se ignoran
(defparameter *ruido* nil)
(defparameter *todas-obs* nil)


;;=========================================================
;; Función para reiniciar variables globales
;;=========================================================
(defun reset-all()
  (setq *con-grupo* nil)
  (setq *centrales* nil)
  (setq *vecinos* nil)
  (setq *tabla* nil)
  (setq *grupo* 1)
  (setq *ignorar* ;Esto es lo que obtuve con la función genera-aleatorios
    '(5 4065 3002 2347 2147 1573 224 3354 3423 2791 3929 1203 311 1194
      1618 3676 2524 3121 1198 2055 2987 2440 3258 1679)) 
  (setq *ruido* nil)
  (setq *todas-obs* nil)
);defun

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
        ;Auxiliar para la distancia máxima
        (if (> (manhattan obs1 obs2) *dist-max*) (setq *dist-max* (manhattan obs1 obs2)) )
        ;Auxiliar para la distancia mínima
        (if (and (< (manhattan obs1 obs2) *dist-min*) (/= (manhattan obs1 obs2) 0)) (setq *dist-min* (manhattan obs1 obs2)) )
        );loop j
      );loop i
      mat-dist
    );let
);defun

;;==================================================
;; Función para obtener los elementos de la matriz
;; de distancias
;;==================================================
(defun get-element(renglon columna matriz)
  (cond
    ((= renglon columna) 0)
    ((< renglon columna) (aref matriz columna renglon ))
    (t (aref matriz renglon columna ))));defun

;;===================================================
;; Función para generar números aleatorios
;; entre 0 y un límite
;; estos número serán los índices de las observaciones
;; que serán ignoradas (conjunto de prueba)
;;===================================================
(defun genera-aleatorios(cantidad limite)
  (let ((lista nil))
    (loop for i from 1 to cantidad do
      (setq lista (append lista (list (random limite)))));loop
    lista));defun
;;====================================================
;; Función para encontrar los patrones centrales
;; (encuentra-centrales mat-dist 10 100)
;; (encuentra-centrales mat-dist 10 200)
;;====================================================
(defun encuentra-centrales(matriz eps mu)
"Función para encontrar los patrones centrales
ENTRADA:
matriz:Matriz de distancias
eps:epsilon
mu:mi
SALIDA:
lista:Una lista con los índices de los patrones centrales
"
  (let ((conteo 0) (dim 0) (lista nil))
    (setq dim (array-dimension matriz 0))
    (loop for i from 0 to (1- dim) do
      (when (not (member i *ignorar*))
        (loop for j from 0 to (1- dim) do
          (when (and (<= (get-element i j matriz) eps) (/= i j)) ;cuando la distancia es menor a epsilon y no son el mismo elemento
            (setq conteo (1+ conteo)));when
        ));when
      (if (>= conteo mu) (setq lista (append lista (list i))))
      (setq conteo 0)
    );loop i
    lista
  );let
);defun

;;=======================================================
;; Actualiza tabla asignando etiquetas
;; PENDIENTE
;;=======================================================

;;=======================================================
;; Función para encontrar los vecinos de un patrón
;;=======================================================
(defun encuentra-vecinos(matriz renglon eps)
  (let ((dim 0) (resultado nil))
    (setq dim (array-dimension matriz 0))
    (loop for col from 0 to (1- dim) do
      (when (and (<= (get-element renglon col matriz) eps)
                 (not (member col *ignorar*)))
        (setq resultado (append resultado (list col)))))
  );let
);defun

;;========================================================
;; Función para asignar grupos a vecinos
;; PENDIENTE ACTUALIZAR TABLA
;;========================================================
(defun asigna-grupos(vecinos)
  (loop for elem in vecinos do
    (if (not (member elem *con-grupo*))
      (setq *con-grupo* (append *con-grupo* (list elem))) )
  );loop
);defun


;;=======================================================
;; Auxiliar que contiene los índices de todas los
;; patrones (0,1,2.....,N)
;;=======================================================
(defun indices(matriz)
  (loop for i from 0 to (1- (array-dimension matriz 0)) do
    (setq *todas-obs* (append *todas-obs* (list i))))
);defun

;;========================================================
;; Algoritmo dbscan
;;========================================================
(defun dbscan(matriz eps mu)
  (let ((ind-cent nil) (ind-vec nil) (dim 0))
    (setq dim (array-dimension matriz 0))
    (setq *centrales* (encuentra-centrales matriz eps mu))
   (loop until (null *centrales*) do
     (setq ind-cent (pop *centrales*))
     (when (not (member ind-cent *con-grupo*))
       (push ind-cent *con-grupo*)
       (setq *vecinos* (encuentra-vecinos matriz ind-cent eps))
       (asigna-grupos *vecinos*)
       ;actualiza tabla falta determinar que tipo de punto son (centrales,frontera,ruido)
       (loop until (null *vecinos*) do
         (setq ind-vec (pop *vecinos*))
         (when (member ind-vec *centrales*) ;cuando es directamente alcanzable
           (loop for i from 0 to (1- dim) do
             (when (and (<= (get-element ind-vec i matriz) eps) (not (member i *con-grupo*)))
                (push i *vecinos*)
                (push i *con-grupo*)
              ;actualiza tabla falta determinar que tipo de punto son (centrales,frontera,ruido)
             );when
           );loop
         );when
        );loop
     );when
    (incf *grupo*)
   );loop
   (setq *ruido* (set-difference *todas-obs* *con-grupo*))
  );let
);defun
