;PAQUETE 4
;DAVID RICARDO MONTALVÁN HERNÁNDEZ


;EJERCICIO 1
(defun collect(pred lista)
  "Toma un predicado (símbolo con el nombre de una función)
  y una lista.
  Regresa una lista conteniendo sólomante los elementos en los cuales
  el predicado evalua T
  "
  (let ((lista-resultado (list)))
    (cond
      ((null lista) nil);caso base
      ((funcall pred (first lista)) (setq lista-resultado
        (append lista-resultado (cons (first lista) (collect pred (rest lista))))))
      (t (collect pred (rest lista))))));

;EJERCICIO 2

(defun palíndromo(lista &optional (pos 0))
  "En esta función se realiza la recursión
  Se compara la lista original y su reversión en reversa posición
  por posición
  "
  (let ((lista-rev (list)) (flag t))
    (setq lista-rev (reverse lista)) ;Lista original en reversa
    (cond
      ((= pos (length lista)) (setq flag t));caso base (una lista vacía es palíndromo)
      ((equal (nth pos lista) (nth pos lista-rev)) (palíndromo lista (1+ pos)))
      (t (setq flag nil)))
      flag));defun

;EJERCICIO 3
;2Palindrome

;EJERCICIO 4
(defun iterativepalindrome(cadena)
  (let ((cadena-rev) (palindromo))
    (setq cadena-rev (reverse cadena)) ;cadena en reversa
    (setq palindromo (copy-seq cadena)) ;Copia la cadena de entrada
    (setq palindromo (concatenate 'string palindromo " ")) ;Fines estéticos
    (loop for char across cadena-rev do
      ;Concatena utilizando la cadena en reversa
      ;OJO CON (list char), CONCATENATE es para secuencias
      (setq palindromo (concatenate 'string palindromo (list char)))
    );loop
  palindromo
  );let
);defun

;EJERCICIO 5
(defun listrotate(cadena n  &key (right nil) (left nil))
  (let ((resultado nil) (long 0))
    (setq long (length cadena))
    (cond
      ((and right left) (format t "Utiliza sólo una llave"))
      ((= (/ long 2) n) (setq resultado (concatenate 'string (subseq cadena n long)
        (subseq cadena 0 n)))) ;Cadenas de longitud par y se corta en el punto medio
      ((not left) (setq resultado (concatenate 'string (subseq cadena n long)
        (subseq cadena 0 n))));Derecha
      ((not right) (setq resultado (concatenate 'string (subseq cadena (- long n) long)
        (subseq  cadena 0 (- long n) )))) ;izquierda
      (t nil)
    );cond
    resultado
  );let
);defun


;EJERCICIO 6

;(setq lista (append lista (list (cons col row))))

(defun max&pos(matriz)
  (let ((num-col 0) (num-reng 0) (maximo 0) (reng-max 0) (lista (list)))
    ;Obtiene las dimensiones de la matriz
    (setq num-col (second (array-dimensions matriz))) ;Columnas
    (setq num-reng (first (array-dimensions matriz))) ;renglones

    ;Itera sobre la matriz
    (loop for col from 0 to (1- num-col) do ;fija columna
      (setq maximo (aref matriz 0 col)) ;Valor arbritario (lo utilicé para considerar positivos y negativos)
      (loop for reng from 0 to (1- num-reng) do
        (when (>= (aref matriz reng col) maximo)
            (setq maximo (aref matriz reng col))
            (setq reng-max reng)
        );when
      );loop reng
      (setq lista (append lista (list (cons col reng-max))))
    );loop col
  lista
  );let
);defun
