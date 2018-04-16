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
    ;Conteo etiquetas es una lista cuya i-ésima entrada corresponde a
    ;el conteo de la i-ésima etiqueta en lista-etiquetas
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
