;;======================================================
;; Define la lista que representa la tabla de datos
;; la última columna son las etiquetas
;; los elementos son símbolos
;;======================================================
(defvar *tabla* '((S C ALTA NO NO-JUGAR)
                  (S C ALTA SI NO-JUGAR)
                  (N C ALTA NO JUGAR)
                  (LL T ALTA NO JUGAR)
                  (LL F NULA NO JUGAR)
                  (LL F NULA SI NO-JUGAR)
                  (N F NULA SI JUGAR)
                  (S T ALTA NO NO-JUGAR)
                  (S F NULA NO JUGAR)
                  (LL T NULA NO JUGAR)
                  (S T NULA SI JUGAR)
                  (N T ALTA SI JUGAR)
                  (N C NULA NO JUGAR)
                  (LL T ALTA SI NO-JUGAR)  ))

;;=======================================================
;; Define las etiquetas de los atributos
;;=======================================================
(defvar *etiquetas-atributos* '(pronostico temperatura humedad viento))

;;=======================================================
;; Función para inicializar variables globales
;;=======================================================
(defun inicializa ()
  (setq *tabla* '((S C ALTA NO NO-JUGAR)
                    (S C ALTA SI NO-JUGAR)
                    (N C ALTA NO JUGAR)
                    (LL T ALTA NO JUGAR)
                    (LL F NULA NO JUGAR)
                    (LL F NULA SI NO-JUGAR)
                    (N F NULA SI JUGAR)
                    (S T ALTA NO NO-JUGAR)
                    (S F NULA NO JUGAR)
                    (LL T NULA NO JUGAR)
                    (S T NULA SI JUGAR)
                    (N T ALTA SI JUGAR)
                    (N C NULA NO JUGAR)
                    (LL T ALTA SI NO-JUGAR)  ))

 (setq *etiquetas-atributos* '(pronostico temperatura humedad viento))
)

;;=======================================================
;; Obtiene las etiquetas de la tabla
;; La tabla las debe de tener en la última columna
;;=======================================================
(defun obtiene-etiquetas (tabla)
  (let ((etiquetas nil) (ncol 0) )
    (setq ncol (length (first *tabla*)))
    (loop for renglon in tabla do
      (if (not (member (nth (1- ncol) renglon ) etiquetas )) (push (nth (1- ncol) renglon) etiquetas) ) )
    etiquetas
  );let
);defun

;;=======================================================
;; Variable global que guarda las etiquetas posibles
;;=======================================================
(defparameter *etiquetas* (obtiene-etiquetas *tabla*))

;;=============================================================
;; Función para calcular la entropía
;; lista-etiquetas son las posibles clases en las observaciones
;;=============================================================
(defun calcula-entropia (tabla lista-etiquetas)
  (let ((nreng 0 ) (ncol 0) (conteo-etiquetas nil) (etiqueta nil) (pos-etiqueta nil) (entropia 0) (probabilidad 0) )
    (setq nreng (length tabla));Número de observaciones
    (setq ncol (length (first tabla))) ;Número de atributos

    ;Inicializa conteo-etiquetas (lista)
    ;Conteo-etiquetas es una lista cuya i-ésima entrada corresponde a
    ;el conteo de la i-ésima entrada en lista-etiquetas
    (loop for i in lista-etiquetas do (push 0 conteo-etiquetas))

    ;Cuenta el número de veces que se presenta una etiqueta
    (loop for renglon in tabla do
      (setq etiqueta (nth (1- ncol) renglon))
      (setq pos-etiqueta (position etiqueta lista-etiquetas))
      (setf (nth pos-etiqueta conteo-etiquetas) (1+ (nth pos-etiqueta conteo-etiquetas)))  )

    ;calcula la entropia
    (loop for conteo in conteo-etiquetas do
      (setq probabilidad (/ conteo nreng))
      ;Calcula la entropía considerando los casos cuando probabilidad==0
      (setq entropia (if (/= probabilidad 0) (- entropia (* probabilidad (log probabilidad 2))) entropia ) )  )
    entropia
  );let
);defun

;;=========================================================
;; Función para separar el conjunto de datos de acuerdo a
;; el valor de un atributo.
;;=========================================================
(defun split-data (tabla indice valor)
  "
  ENTRADA
  tabla: tabla de datos
  indice: indice del atributo
  valor: valor del atributo
  SALIDA
  nueva-tabla: nueva tabla omitiendo las observaciones en las cuales el
  atributo[indice] == valor
  "
  (let ((nueva-tabla nil) (ncol 0) (nuevo-renglon nil) )
    ;Número de columnas (atributos)
    (setq ncol (length (first tabla)))

    ;comienza a filtrar la información
    (loop for renglon in tabla do

       (when (equal (nth indice renglon) valor )

        (loop for i from 0 to (1- ncol) do
            ;Extrae el nuevo renglón omitiendo el índice del atributo dado
           (if (/= i indice) (setq nuevo-renglon (append nuevo-renglon (list (nth i renglon) ))) ) )

        ;agrega el nuevo renglón a la tabla
        (push nuevo-renglon nueva-tabla)
      );when

      ;reinicia nuevo-renglon para nueva iteración
      (setq nuevo-renglon nil)
    );loop renglon
    (setq nueva-tabla (reverse nueva-tabla))
  nueva-tabla
  );let
);defun

;;=============================================================
;; Función para seleccionar el atributo que tiene la mayor
;; ganancia en información
;; lista-etiquetas son las clases de las observaciones
;;=============================================================
(defun mejor-atributo (tabla lista-etiquetas)
  (let ((num-atributos 0) (num-obs 0) (entropia-total 0) (ganancia 0) (mejor-ganancia 0)
   (valores-atributo 0) (mejor-atributo nil) (entropia 0) (sub-tabla nil) (probabilidad 0) )

   ;Número de atributos
   (setq num-atributos (1- (length (first tabla))))

   ;Número de observaciones en total
   (setq num-obs (length tabla))

   ;Entropía del la tabla completa
   (setq entropia-total (calcula-entropia tabla lista-etiquetas))

   (loop for i from 0 to (1- num-atributos) do
     (setq entropia 0)
     (setq valores-atributo nil)
     ;Extrae los posibles valores para el atributo i
     (loop for renglon in tabla do
       (if (not (member (nth i renglon) valores-atributo )) (setq valores-atributo (append valores-atributo (list (nth i renglon) )))) ) ;loop renglon

    ;Calcula la entropía para el atributo i
    (loop for valor in valores-atributo do
       (setq sub-tabla (split-data tabla i valor))
       (setq probabilidad (/ (length sub-tabla) num-obs ))
       (setq entropia (+ entropia (* probabilidad (calcula-entropia sub-tabla lista-etiquetas))))  );loop valor
    (setq ganancia (- entropia-total entropia))

    ;registra el mejor atributo (mayor ganancia)
    (when (> ganancia mejor-ganancia) (setq mejor-ganancia ganancia) (setq mejor-atributo i) )
   );loop i
   mejor-atributo
  );let
);defun

;;===================================================================
;; Función para contar la etiqueta con mayor número de apariciones
;;===================================================================
(defun voto-mayoria (lista)
  (let ((revisados nil) (num-elementos 0) (clase-maximo 0) (conteo 0) (conteo-maximo 0) (clase nil))
    (setq num-elementos (length lista))

    (loop for i from 0 to (1- num-elementos) do
       (setq clase (nth i lista))
        (when (not (member clase revisados))
          (setq conteo (count clase lista))
          (when (> conteo conteo-maximo)
            (setq conteo-maximo conteo)
            (setq clase-maximo clase)))
       (push clase revisados))
    clase-maximo
  );let
);defun

;;=================================================================
;; Crea el árbol utilizando una función recursiva
;;=================================================================
(defun crea-arbol (tabla etiquetas-atributos)
  "
  tabla: lista de listas con las observaciones
  etiquetas-atributos: etiquetas de cada atributo (headers)
  "
  (let ((clases nil) (num-col 0) (indice-mejor-atributo nil) (etiqueta-mejor-atributo nil)
         (arbol nil) (valores-atributo nil) (sub-etiquetas-atributos nil) (aux-etiquetas nil) )

    ;número de columnas
    (setq num-col (length (first tabla)))

    ;Obtiene la clase de cada observación en la tabla
    (loop for renglon in tabla do (setq clases (append clases (list (nth (1- num-col) renglon))) ) )

    ;Case base 1
    ;Cuando todas las observaciones pertenecen a la misma clase
    (if (= (count (first clases) clases) (length clases) ) (return-from crea-arbol (first clases))  )

    ;Caso base 2
    ;Cuando ya no hay más atributos que revisar, se regresa la clase mayoritaria
    (if (= (length clases) 1 ) (return-from crea-arbol (voto-mayoria clases)) )

    ;obtiene el índice del mejor atributo para separar
    ;y la etiqueta "humana" del atributo
    (setq indice-mejor-atributo (mejor-atributo tabla *etiquetas*) )
    (setq etiqueta-mejor-atributo (nth indice-mejor-atributo etiquetas-atributos ) )

    ;el árbol es creado con acons
    (setq arbol (append arbol (list etiqueta-mejor-atributo)))

    ;actualiza las etiquetas de los atributos
    (loop for etiqueta in etiquetas-atributos do
      (if (not (equal etiqueta etiqueta-mejor-atributo)) (setq sub-etiquetas-atributos (append sub-etiquetas-atributos (list etiqueta))) ) )

    ;obtiene los valores (sin repeticiones) del mejor atributo
    (loop for renglon in tabla do
      (if (not (member (nth indice-mejor-atributo renglon) valores-atributo))
        (setq valores-atributo (append valores-atributo (list (nth indice-mejor-atributo renglon)))) ) )

    (loop for valor in valores-atributo do
      ;No quiero que las etiquetas sean modificadas por cada VALOR
      ;iterado
       (setq aux-etiquetas (copy-seq sub-etiquetas-atributos ))
      (setq arbol (append arbol (list valor  (list (crea-arbol (split-data tabla indice-mejor-atributo valor) aux-etiquetas)) )))    )
   arbol
  );let
);defun

;;======================================================
;; Main
;;======================================================
(defun main ()
  (inicializa)
  (crea-arbol *tabla* *etiquetas-atributos*)
)
