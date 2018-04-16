;;======================================================
;; Define la lista que representa la tabla de datos
;; la última columna son las etiquetas
;; los elementos son símbolos
;;======================================================
(defparameter *tabla* '((S C ALTA NO NO-JUGAR)
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

;;=======================================================
;; Función para calcular la entropía
;;=======================================================
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
      (setq entropia (- entropia (* probabilidad (log probabilidad 2))))  )
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
