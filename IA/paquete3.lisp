;Paquete 3
;David Ricardo Montalván Hernández

;EJERCICIO 1
;El argumento pos es un número entero mayor o igual a 1
;esto es para tener una posición "natural"
(defun eleminpos(ele lista pos)
  (cond ((null lista) nil) ;caso base
        ((and (equal (first lista) ele) (= pos 1)) t)
        (t (eleminpos ele (rest lista) (- pos 1)))
    );cond
  );defun

;EJERCICIO 2
(defun inicio-en(lista elemento)
  (cond
    ((null lista) nil) ;caso base 1
    ((equal (first lista) elemento) lista) ;caso base 2
    (t (inicio-en (rest lista) elemento))
    );cond
  );defun

;EJERCICIO 3
(defun termina-en(lista elemento)
  (setq lista (reverse lista))
  (setq lista (inicio-en lista elemento))
  (reverse lista)
  );defun

;EJERCICIO 4
;Se utiliza una función auxiliar para llevar
;el índice de la posición impar
(defun primer-impar-aux(lista indice)
  (cond
    ((null lista) nil)
    ((/= (mod (first lista) 2) 0) (list (first lista) indice));revisa impar
    (t (primer-impar-aux (rest lista) (+ indice 1)))
    );cond
  );defun

(defun primer-impar(lista)
  (primer-impar-aux lista 0)
  );defun

  ;EJERCICIO 5

  ;EJERCICIO 6
  ;Se utiliza una función auxiliar para llevar a cabo el conteo
  ;de los números y las listas.
  (defun aux-conteo(lista conteo-numeros conteo-listas)
  (if (numberp (first lista))
    (setq conteo-numeros (+ conteo-numeros 1))
    );if
  (if (and (listp (first lista)) (not (null (first lista))))
    (setq conteo-listas (+ conteo-listas 1))
    );if
      (cond
        ((null lista) (cons conteo-numeros conteo-listas))
        (t (aux-conteo (rest lista) conteo-numeros conteo-listas))
        );cond
    );defun
    
  (defun conteo(lista)
    (aux-conteo lista 0 0)
    );defun
