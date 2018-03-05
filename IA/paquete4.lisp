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
