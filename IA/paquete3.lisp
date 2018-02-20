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

  ;EJERCICIO 7
  ;APLANA

  ;EJERCICIO 8
  ;Determinar el mínimo entre columnas y renglones
  ;Llevar un contador que no rebase o sea igual al mínimo
  ;Caso base si el contador es mayor o igual al mínimo
  ;De first de la lista extraer el nth contador lista
  ;Recursión utilizando rest
  ;Definir una función auxiliar en la que se realiza la Recursión
  ;La función principal calcula el mínimo entre columnas y renglones
  ;y manda a llamar a la función auxiliar

  (defun diagonal-aux(lista contador minimo)
    (cond
      ((or (null lista) (>= contador minimo)) nil) ;Casos base
      (t (append (list (nth contador (first lista)))
         (diagonal-aux (rest lista) (+ contador 1) minimo)))
      );cond
    );defun

  (defun diagonal(lista)
    (let ((renglones 0) (columnas 0) (minimo 0) (diagonal-res (list)))
      (setq renglones (length lista))
      (setq columnas (length (first lista)))
      (setq minimo (min renglones columnas))
      (setq diagonal-res (diagonal-aux lista 0 minimo))
      );let
    );defun

  ;EJERCICIO 9
  ;Determinar el tipo de elemento del primer miembro de la lista
  ;Recursión utilizando rest
  ;Utiliza una función auxiliar (sólo por comodidad) para determinar el tipo

  (defun determina-tipo-aux(elemento)
    (typecase elemento
      (null 'N)
      (atom 'A)
      (list 'L)
    );typecase
  );defun

  (defun ejercicio9(lista)
    (cond
      ((null lista) nil)
      (t (append (list (determina-tipo-aux (first lista))) (ejercicio9 (rest lista))))
      );cond
    );defun

  ;EJERCICIO 10
  ;Caso base: si no es número regresar cero

  (defun suma-numérica(lista)
    (cond
        ((null lista) 0)
        ((not (numberp (first lista))) (+ 0 (suma-numérica (rest lista))))
        ((numberp (first lista)) (+ (first lista) (suma-numérica (rest lista))))
      );cond
    );defun

  ;EJERCICIO 11
  ;FILTRA VOCALES

  ;EJERCICIO 12
  (defun filtra-múltiplos(lista entero)
      (cond
        ((null lista) nil)
        ((= (mod (first lista) entero) 0)
          (append (list) (filtra-múltiplos (rest lista) entero))) ;Si es múltiplo
        ((/= (mod (first lista) entero) 0)
          (append (list (first lista)) (filtra-múltiplos (rest lista) entero)));No es múltiplo
      );cond
    );defun

  ;EJERCICIO 13
  ;EJERCICIO 14
  ;EJERCICIO 15

  ;EJERCICIO 16
  (defun cambia(lista elem1 elem2)
    (cond
      ((null lista) nil)
      ((equal (first lista) elem1) (append (list elem2) (cambia (rest lista) elem1 elem2)))
      ((not (equal (first lista) elem1)) (append (list (first lista)) (cambia (rest lista) elem1 elem2)))
    );cond
  );defun
