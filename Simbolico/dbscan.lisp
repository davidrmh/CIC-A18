;;=========================================================
;; Variables globales auxiliares
;;=========================================================

(defparameter *con-grupo* nil) ;lista
(defparameter *centrales* nil) ;lista
(defparameter *centrales-permanentes* nil) ;lista
(defparameter *frontera* nil);lista
(defparameter *ruido* nil) ;list
(defparameter *vecinos* nil) ;lista
(defparameter *tabla* nil) ;arreglo
(defparameter *grupo* 0) ;número
(defparameter *dist-max* 0);número (máximo fue 465)
(defparameter *dist-min* 1000000000);número (mínimo fue 1)
(defparameter *ignorar* nil) ;Los 25 que se ignoran
(defparameter *todas-obs* nil)


;;=========================================================
;; Función para reiniciar variables globales
;;=========================================================
(defun reset-all()
  (setq *con-grupo* nil)
  (setq *centrales* nil)
  (setq *centrales-permanentes* nil)
  (setq *vecinos* nil)
  (setq *tabla* nil)
  (setq *grupo* 0)
  (setq *ignorar* ;Esto es lo que obtuve con la función genera-aleatorios
    '(5 4065 3002 2347 2147 1573 224 3354 3423 2791 3929 1203 311 1194
      1618 3676 2524 3121 1198 2055 2987 2440 3258 1679))
  (setq *ruido* nil)
  (setq *todas-obs* nil)
  (setq *frontera* nil)
);defun

;;=========================================================
;; Función para inicializar la tabla de resultados
;;=========================================================
(defun inicializa-tabla(clases)
"Función para inicializar la tabla que contiene la información
de los patrones
ENTRADA:
clases: lista que contiene el total de observaciones y su clase
SALIDA:
Esta función inicializa la variable global *tabla* que será
un arreglo con cuatro columnas
1.Índice del patrón.
2.Clase a la que pertenece de acuerdo al archivo UCI
3.Tipo de patrón (central,frontera,ruido).
4.Grupo al que pertenece de acuerdo el algoritmo dbscan.
"
(let ((nobs 0))
  (setq nobs (length clases));número de observaciones
  (setq *tabla* (make-array (list nobs 4) :initial-element nil))
  ;Llena columnas 1 y 2
  (loop for i from 0 to (1- nobs) do
    (setf (aref *tabla* i 0) i);columna 1
    (setf (aref *tabla* i 1) (nth i clases))))
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
(first (first arreglo)) contiene la clase a la que pertenece cada observación
(first (first (rest arreglo))) ;contiene los datos numéricos
"
  (let ((linea nil) (arreglo nil) (archivo nil) (nobs nil)
    (clases nil) (aux nil))
    (setq archivo (open ruta-archivo))
    (loop for i from 1 to 100 do
      (setq aux (read archivo))
      (setq linea (rest aux))
      (setq nobs (length linea));número de observaciones en la línea
      ;modifica la observación j
      (loop for j from 0 to (1- nobs) do
        (setq clases (append clases (list (first aux))))
        (loop for k from 0 to 5 do
          (setf (nth k (nth j linea)) (second (nth k (nth j linea))) ));loop k
        (setq arreglo (append arreglo (list (nth j linea)))));loop j
    );loop i
  (close archivo)
  (setq arreglo (list (list clases) (list arreglo)))
  arreglo));defun

;;===============================================
;; Función de distancia
;;================================================
(defun funcion-distancia(obs1 obs2)
  ;manhattan
  ;(reduce #'+ (mapcar #'abs (mapcar #'- obs1 obs2)))
  ;Euclidiana
  (sqrt (reduce #'+ (mapcar #'* (mapcar #'- obs1 obs2) (mapcar #'- obs1 obs2))))
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
        (setf (aref mat-dist i j) (funcion-distancia obs1 obs2))
        ;Auxiliar para la distancia máxima
        (if (> (funcion-distancia obs1 obs2) *dist-max*) (setq *dist-max* (funcion-distancia obs1 obs2)) )
        ;Auxiliar para la distancia mínima
        (if (and (< (funcion-distancia obs1 obs2) *dist-min*) (/= (funcion-distancia obs1 obs2) 0)) (setq *dist-min* (funcion-distancia obs1 obs2)) )
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
;; Función para encontrar los vecinos de un patrón
;;=======================================================
(defun encuentra-vecinos(matriz renglon eps)
  (let ((dim 0) (resultado nil))
    (setq dim (array-dimension matriz 0))
    (loop for col from 0 to (1- dim) do
      (when (and (<= (get-element renglon col matriz) eps)
                 (not (member col *ignorar*)) (/= renglon col))
        (setq resultado (append resultado (list col)))))
    resultado
  );let
);defun

;;========================================================
;; Función para asignar grupos a vecinos
;;========================================================
(defun asigna-grupo(vecinos)
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
    (setq *centrales-permanentes* (copy-seq *centrales*)) ;para referencia
   (loop until (null *centrales*) do

     (setq ind-cent (pop *centrales*)) ;Elige un patrón central

     (when (not (member ind-cent *con-grupo*)) ;Si no se ha clasificado
       (incf *grupo*)
       (push ind-cent *con-grupo*) ;Agrega a la lista de los que ya tienen un grupo

       ;actualiza tabla
       (setf (aref *tabla* ind-cent 2) :central) ;tipo de patrón
       (setf (aref *tabla* ind-cent 3) *grupo*) ;grupo

      ;obtiene sus vecinos
       (setq *vecinos* (encuentra-vecinos matriz ind-cent eps))
      ;Estos vecinos son patrones frontera o centrales
      ;por lo tanto tendrán un grupo
       (asigna-grupo *vecinos*)
       (loop until (null *vecinos*) do
         (setq ind-vec (pop *vecinos*)) ;Elige un vecino

         ;El vecino es un patrón frontera?
         (when (not (member ind-vec *centrales-permanentes*))

          (if (not (member ind-vec *frontera*)) (push ind-vec *frontera*))
          ;actualiza tabla
          (setf (aref *tabla* ind-vec 2) :frontera)
          (setf (aref *tabla* ind-vec 3) *grupo*));when

         ;El vecino es un patrón central?
         (when (member ind-vec *centrales-permanentes*)
          ;actualiza tabla
          (setf (aref *tabla* ind-vec 2) :central)
          (setf (aref *tabla* ind-vec 3) *grupo*)

            ;Encuentra los patrones indirectamente alcanzables
            ;desde el patrón central ind-cent utilizando como
            ;patrón intermedio el patrón ind-vec
            (loop for i from 0 to (1- dim) do
              ;los puntos que cumplen las condiciones
              ;son density-reachables (indirectamente alcanzables)
              (when (and (<= (get-element ind-vec i matriz) eps) (not (member i *con-grupo*)) (not (member i *ignorar*)))
                  (push i *vecinos*)
                  (push i *con-grupo*)
              );when
            );loop
         );when

       );loop *vecinos*
     );when patrón central no se ha clasificado
    ;(incf *grupo*)
   );loop *centrales
   (setq *ruido* (set-difference *todas-obs* *con-grupo*))
   (setq *ruido* (set-difference *ruido* *ignorar*))
   ;Actualiza la tabla con los patrones ruido
   (loop for i in *ruido* do
     (setf (aref *tabla* i 2) :ruido)
     (setf (aref *tabla* i 3) :ruido))
  );let
);defun


;;=================================================================
;; Función que regresa el argumento en donde se encuentra el máximo
;; en una lista (primer ocurrencia)
;;=================================================================
(defun argmax (lista)
  (let ((maximo 0) (argumento 0) )
    (setq maximo (reduce #'max lista))
    (loop for elem in lista do
      (when (= elem maximo)
        (return-from argmax argumento))
      (incf argumento) ));let
);defun


;;=================================================================
;; Función que regresa el argumento en donde se encuentra el mínimo
;; en una lista (primer ocurrencia)
;;=================================================================
(defun argmin (lista)
  (let ((minimo 0) (argumento 0) )
    (setq minimo (reduce #'min lista))
    (loop for elem in lista do
      (when (= elem minimo)
        (return-from argmin argumento))
      (incf argumento) ));let
);defun

;;=================================================================
;; Encuentra los índices de los k-ésimos menores en una lista
;;=================================================================
(defun k-esimos-menores (lista k)
  (let ((indices nil) (lista-aux nil) (aux 0) (memoria nil))
    (setq lista-aux (copy-seq lista))
    (setq lista-aux (sort lista-aux #'<))
    (loop for i from 0 to (- k 1) do
        (setq aux (nth i lista-aux))
        (setq memoria nil)
        (loop for j from 0 to (1- (length lista-aux)) do
          (when ( and (= aux (nth j lista)) (not (member j indices)) (not (member aux memoria)) )
            (push j indices)
            (push aux memoria) )  ) )
  (setq indices (reverse indices))
  indices
  )
)

;;================================================================
;; Algoritmo KNN
;;================================================================
(defun knn (lista)
  "Algoritmo KNN"

  (let ((lista-clases nil) (lista-conteo nil) (contador 0) (clase nil) )
    (loop for i in lista do
      (loop for j in lista do
        (when (and (equal i j) (not (member j lista-clases)) ) (incf contador) )
      );loop j
      (push i lista-clases)
      (push contador lista-conteo)
      (setq contador 0)
    );loop i
    (setq clase (nth (argmax lista-conteo) lista-clases ))
    clase
  );let
);defun

;;================================================================
;; Función para clasificar los patrones omitidos
;; con base en la distancia promedio mínima a los centroides
;;================================================================
(defun clasifica (matriz)
  "Función para clasificar los patrones omitidos
  ENTRADA
  matriz: matriz de distancias
  SALIDA
  resultado: lista cuya i-ésima entrada es el grupo en el cual
  se clasifica la i-ésima entranda de la lista *ignorar*
  "
  (let ((lista-conteo nil) (lista-dist nil) (lista-dist-prom nil) (grupo nil) (dist 0) (lista-flags nil) (resultado nil))
    (loop for reng in *ignorar* do
       ;reinicia listas
       (setq lista-conteo nil)
       (setq lista-dist nil)
       (setq lista-dist-prom nil)
       (setq lista-flags nil)
       (loop for i from 0 to (1- *grupo*) do (push 1 lista-conteo) (push 100000 lista-dist) (push t lista-flags))

       (loop for i from 0 to (1- (array-dimension *tabla* 0)) do
         (when (equal (aref *tabla* i 2) :central)
            (setq grupo (1- (aref *tabla* i 3)) )
            (setq dist (get-element reng i matriz))
            (cond
              ((nth grupo lista-flags) (setf (nth grupo lista-dist) dist) (setf (nth grupo lista-flags) nil)  )
              ((not (nth grupo lista-flags)) (setf (nth grupo lista-dist) (+ (nth grupo lista-dist) dist)) (setf (nth grupo lista-conteo) (incf (nth grupo lista-conteo)))  )
            );cond
         );when
       );loop
       ;Calcula las distancias promedio
       (setq lista-dist-prom (mapcar #'/ lista-dist lista-conteo))

       ;Encuentra el argumento en donde se encuentra el mínimo
       ;asigna a reng el grupo  1 + argumento (inicié los grupos en 1 por eso el 1+)
       (push (1+ (argmin lista-dist-prom)) resultado )
    );loop
    (setq resultado (reverse resultado))
    resultado
  );let
);defun

;;=============================================================
;; Función para clasificar utilizando KNN
;;=============================================================
(defun clasifica-knn (matriz &optional (k 3))
  "Clasifica las observaciones de prueba utilizando KNN"

  (let ((lista-dist nil) (lista-grupos-min nil) (clases nil) )
    (loop for i in *ignorar* do
      (setq lista-dist nil)
      (setq lista-grupos-min nil)

      ;Obtiene el i-ésimo renglon de la matriz de distancias
      (loop for j from 0 to (1- (array-dimension matriz 1)) do
        (when (/= i j) (push (get-element i j matriz) lista-dist ) )
      );loop j
      (setq lista-dist (reverse lista-dist))

      ;Calcula los k-ésimos-menores
      (loop for j in (k-esimos-menores lista-dist k) do
         (push (aref *tabla* j 3) lista-grupos-min) )

      ;Aplica KNN
      (push (knn lista-grupos-min) clases )
    );loop i
    (setq clases (reverse clases))
    clases
  );let
);defun

;;=============================================================
;; Función para quitar repetidos de una lista
;;=============================================================
(defun unicos (lista aux)
  (cond
    ((null lista) (reverse  aux))
    ((member (first lista) aux) (unicos (rest lista) aux))
    (t (push (first lista) aux) (unicos (rest lista) aux))
  )
)

;;=============================================================
;; Función principal
;;=============================================================

(defun main-dbscan (&optional (ruta-datos "chorales.lisp") (eps 2) (mu 10) (k 3))
"Función principal
ENTRADA:
ruta-datos: Cadena con la ruta del archivo de UCI
eps: Epsilon (radio de la vecindad)
mu: Mi (número mínimo necesario de puntos para que una vecindad sea considerada densa)
k: Parámetro del algoritmo KNN
SALIDA:
Una lista con:
first = Matriz de confusión
second = Clases reales de las 25 observaciones no consideradas en el entrenamiento (la clase que DBSCAN arroja)
third = Clases determinadas por el modelo de las 25 observaciones no consideradas en el entrenamiento (la clase que KNN arroja)
"
  (let ((matriz nil) (datos nil) (clases nil) (omitidos nil) (clase-omitidos nil)
         (contador 0) (clases-verdaderas nil) (matriz-confusion nil)
        (verd-unicas nil) (omit-unicas nil) (aux-verd nil) (aux-omit nil))

    (reset-all);reinicia variables globales

    (setq datos (crea-lista ruta-datos));lee datos
    (setq clases (first (first datos)));obtiene la clase a la que pertenece cada observación
    (setq datos (first (first (rest datos)))) ;datos numéricos
    (setq matriz (matriz-distancias datos));Obtiene la matriz de distancias

    ;(setq *ignorar* (genera-aleatorios 25 (1- (array-dimension matriz 0)) ))
    (setq omitidos (copy-seq *ignorar*))
    (setq *ignorar* nil)
    (indices matriz)
    (inicializa-tabla clases)
    (dbscan matriz eps mu)
    (setq *ignorar* (copy-seq omitidos))
    (setq clase-omitidos (clasifica-knn matriz k))

    ;Escribe resultados
    (loop for i from 0 to (1- (length omitidos)) do
      (push (aref *tabla* (nth i omitidos) 3) clases-verdaderas )
      (format t "La predicción es ~a la real es es ~a~%" (nth i clase-omitidos) (aref *tabla* (nth i omitidos) 3)))

    (setq clases-verdaderas (reverse clases-verdaderas))
    (setq verd-unicas (unicos clases-verdaderas nil))
    (setq omit-unicas (unicos clase-omitidos nil))
    (setq matriz-confusion (make-array (list (length verd-unicas) (length omit-unicas) ) ))

    ;Calcula la matriz de confusión
    (loop for reng from 0 to (1- (length verd-unicas)) do
      (setq aux-verd (nth reng verd-unicas))

      (loop for col from 0 to (1- (length omit-unicas)) do
        (setq aux-omit (nth col omit-unicas))
        (setq contador 0)

        (loop for c from 0 to (1- (length omitidos)) do
            (when (and (equal (nth c clases-verdaderas) aux-verd ) (equal (nth c clase-omitidos) aux-omit ) (incf contador)) )
        );loop c

        (setf (aref matriz-confusion reng col) contador)
      );loop col
    );loop reng
  (list matriz-confusion clases-verdaderas clase-omitidos)
  );let
);defun
