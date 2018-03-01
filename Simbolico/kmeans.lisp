;n = Número de patrones
;m = Número de columnas
;k = Número de centroides

;ALGORITMO
;1.Crear la matriz de distancias como un arreglo de n x n
;2.Seleccionar los k patrones más alejados como atractores iniciales
;3.Calcular la distancia entre cada punto y cada atractor (matriz n x k)
;4.Particionar el espacio (agrupar)
;5.Para cada grupo encontrar el centroide
;6.Considerar los centroides del paso 5 como los nuevos atractores
;7.Regresar al paso 6
;8. Repetir hasta condición de paro.

;ALGORITMO MATRIZ DE DISTANCIAS
;CALCULA TODA LA MATRIZ
;Inicializa arreglo de D de dimensión n x n (matriz de distancias)
;Para cada patrón i:
  ;Obtener los valores a  utilizar del patrón i
  ;Para cada patrón j distinto de i:
    ;Obtener los valores a utilizar del patrón j
    ;Calcula la distancia entre el patrón i y j
    ;Guarda la distancia calculada en D[i,j]

;Suma los elementos de una lista
(defun suma-numérica(lista)
  (cond
    ((null lista) 0)
    ((not (numberp (first lista))) (+ 0 (suma-numérica (rest lista))))
    ((numberp (first lista)) (+ (first lista) (suma-numérica (rest lista))))
    );cond
);defun

;Distancia euclidiana
;Inputs
;Dos listas con los rasgos de los patrones
(defun dist-eucl(patron1 patron2)
  (let ((sum-dif (list)))
      ;Calcula el cuadrado de las diferencias
      (setq sum-dif(mapcar #'(lambda(x1 x2)
        (when (and (numberp x1) (numberp x2)) (* (- x1 x2) (- x1 x2))))
         patron1 patron2))
      (sqrt (suma-numérica sum-dif))
    );let
  );defun

(defun matriz-distancias(datos indices &optional (headers nil) (fun-dist 'dist-eucl))
;NOTA: indices es una lista con los índices de las "columnas" de interés
;fun-dist es un símbolo con el nombre de una función
  (let ((n-datos 0) (D nil) (patron1 (list)) (patron2 (list)) (aux-reng 0)
    (aux-col 0))
      ;Obtiene el número de datos considerando encabezados
      (if headers
        (setq n-datos (- (array-dimension datos 0) 1))
        (setq n-datos (array-dimension datos 0))
       );if

      ;Inicializa la matriz de distancias
      (setq D (make-array (list n-datos n-datos) :initial-element 0 :adjustable t))

      ;Comienza el llenado de la matriz
      (loop for i from (if headers 1 0) to (- n-datos 1) do

          ;obtiene el patrón i
          (loop for l in indices do
              (setq patron1 (append patron1 (list (aref datos i l))))
            );loop

          ;obtiene los patrones distintos al patrón i
          ;y calcula las distancias

          (loop for j from (if headers 1 0) to (- n-datos 1) do
                ;patrón 2
                (loop for l in indices do
                    (setq patron2 (append patron2 (list (aref datos j l))))
                  );loop
              ;distancia
              (setf (aref D aux-reng aux-col) (funcall fun-dist patron1 patron2))
              ;reinicia para la próxima iteración
              (setq patron2 (list))
              (setq aux-col (1+ aux-col))
            );loop
          ;reinicia para la próxima iteración
          (setq patron1 (list))
          (setq aux-reng (1+ aux-reng))
          (setq aux-col 0)
        );loop
    (return-from matriz-distancias D)
    );let
  );defun

;Función para obtener entradas de la matriz de distancias
(defun obten-el(matriz reng col)
  (if (<= reng col)
      (aref matriz reng col)
      (aref matriz col reng)
    );if
);defun

;Función para calcular la suma acumulada de distancias
;regresa una lista cuya entrada i es la distancia acumulada
;entre la observación i y el resto de las observaciones
(defun suma-dist(matriz)
  (let ((n-datos 0) (resultado (list)) (suma 0))
      (setq n-datos (array-dimension matriz 0))

      (loop for i from 0 to (1- n-datos) do

          (loop for j from 0 to (1- n-datos) do
              (setq suma (+ suma (obten-el matriz i j)))
            );loop

          (setq resultado (append resultado (list suma)))
          (setq suma 0)
        );loop
    resultado
    );let
  );defun

;Función para encontrar el k-ésimo mayor elemento en una lista
(defun k-max(lista k)
  ;primero ordena de mayor a menor
  ;Lo hace de forma NO DESTRUCTIVA
  (setq lista (sort (copy-seq lista) #'>))
  (nth (1- k) lista)
);defun

;Función para obtener el argumento del k-ésimo mayor en una lista
;Los índices son relativos a la tabla que contiene los datos
;no son relativos a la lista con la suma de distancias
(defun arg-k-max(lista k &optional (headers nil))
    (let ((max-val 0) (arg-max 0))
        ;primero encuentra el valor k-max
        (setq max-val (k-max lista k))

        ;itera la lista hasta encontrar el valor máximo
        (loop for val in lista do
            (when (equal val max-val)
                (if (not headers) ;relativo a la tabla de datos
                  (return-from arg-k-max arg-max)
                  (return-from arg-k-max (1+ arg-max))
                  );if
              );when
           (setq arg-max (1+ arg-max))
          );loop
      );let
  );defun

  ;Filtra e identifica los registros repetidos
  ;Primero guarda cada renglón en una lista
  ;después compara, utilizando equal, los renglones subsecuentes
  ;datos es un arreglo creado con la función lee-separado

  (defun repetidos(datos)
    (let ((lista-repetidos (list)) (n-reng 0) (patron1 (list)) (patron2 (list))
      (n-col 0) (indices-repetidos (list)));args-let

    ;número de registros
     (setq n-reng (first (array-dimensions datos)))
    ;número de columnas
    (setq n-col (second (array-dimensions datos)))

     (loop for i from 0 to (- n-reng 2) do
       ;extrae el patrón 1
       (setq patron1 (list))
       (loop for j from 0 to (1- n-col) do
          (setq patron1 (append patron1 (list (aref datos i j))))
        );loop

        ;extrae el patrón 2 y compara
        (loop for k from (1+ i) to (1- n-reng) do
          (setq patron2 (list))
          (loop for j from 0 to (1- n-col) do
            (setq patron2 (append patron2 (list (aref datos k j))))
          );loop
          (when
            (and (equal patron1 patron2) (or (not (find i indices-repetidos :test #'equal))
              (not (find k indices-repetidos :test #'equal))))
            (setq lista-repetidos (append lista-repetidos (list (list i k) patron1)))
            (setq indices-repetidos (append indices-repetidos (list i k)))
            );when
        );loop
      );loop
    lista-repetidos
    );let
  );defun



  ;Función para encontrar el primer atractor
  ;Inputs
  ;Matriz de distancias
  (defun atractor1(mat)
    (let ((dist-acum (list)))
    (setq dist-acum (suma-dist mat))
    (arg-k-max dist-acum 1)
    );let
  );defun

  ;Función para encontrar el segundo atractor
  ;Inputs
  ;Matriz de distancias
  ;Índice del primer atractor
  (defun atractor2(mat ind)
    (let ((max-dist 0) (dist 0) (n-col 0) (clase nil))
      (setq n-col (array-dimension mat 1));número de columnas
      (loop for i from 0 to (1- n-col) do
        (setq dist (aref mat ind i))
        (when (> dist max-dist)
          (setq max-dist dist)
          (setq clase i)
        );when
      );loop
      clase
    );let
  );defun

  ;Función para encontrar el k-ésimo atractor
  ;Inputs
  ;Matriz de distancias
  ;Lista con los índices de los atractores previos
  (defun atractork(mat list-ind)
    (let ((clase 0) (suma 0) (suma-max 0) (n-col 0))
      (setq n-col (array-dimension mat 1));número de columnas
      (loop for j from 0 to (1- n-col) do
        (loop for i in list-ind do
          (setq suma (+ suma (aref mat i j)))
          (when (>= suma suma-max)
            (setq suma-max suma)
            (setq clase j)
          );when
        );loop
      );loop
    clase
    );let
  );defun


  ;Función para obtener los rasgos de los k atractores
  ;Inputs
  ;Arreglo de datos
  ;Lista con índices de los k atractores
  (defun aux-atractores(datos list-ind etiquetas)
    (let ((list-atractores (list)) (n-col 0) (atractor (list)))
    (setq n-col (array-dimension datos 1));Número de columnas
    (loop for i in list-ind do
      (loop for j from 0 to (if etiquetas (1- n-col) (- n-col 2)) do
        (setq atractor (append atractor (list (aref datos i j))))
      );loop
      (setq list-atractores (append list-atractores (list atractor)))
      (setq atractor (list))
    );loop
    list-atractores
    );let
  );defun

  ;Función para encontrar los indices de los k atractores
  ;Inputs
  ;Matriz de distancias
  ;Número de atractores k
  (defun encuentra-atractores(mat k datos &optional (etiquetas nil))
    (let ((list-ind (list)) (list-atractores (list)))
      (loop for i from 1 to k do
        (cond
          ((= i 1) (setq list-ind (append list-ind (list (atractor1 mat)))))
          ((= i 2) (setq list-ind (append list-ind (list (atractor2 mat (first list-ind))))))
          ((>= i 3) (setq list-ind (append list-ind (list (atractork mat list-ind)))))
        );cond
      );loop
    (setq list-atractores (aux-atractores datos list-ind etiquetas))
    list-atractores
    );let
  );defun
