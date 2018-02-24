;Función para leer un archivo separado por un delimitador

;ALGORITMO
;abrir el archivo
;Leer la primer línea y contar el número de delimitadores (Número de columnas)
;flag conteo-columnas = nil
;Crea arreglo ajustable (make-array tabla '(1 columnas) :adjustable T)
;Para cada línea del archivo:
;;Inicializa renglon-pos=0
;;Inicializa columna-pos=0
;;Encontrar las posiciones de los delimitadores(con la función posicion-aux)
;;inicio-aux=0
;;;Para cada posición de un delimitador
;;;;Extrae la información antes del delimitador (subseq linea inicio-aux pos-delim)
;;;;Actualiza Arreglo (setf (aref tabla renglon-pos columna-pos) dato)
;;;;inicio-aux = pos-delim + 1
;;;;columna-pos = columna-pos + 1
;;renglon-pos=renglon-pos + 1
;;Redimensiona el arreglo (ajust-array tabla '(renglon-pos columnas))

;Función auxiliar para contar el número de columnas en el archivo
(defun cuenta-columnas(linea separador)
  (let ((cont-col 0) (inicio-pos 0))
    (loop
      (when (not (position separador linea :start inicio-pos)) (return (1+ cont-col)))
      (setq cont-col (+ cont-col 1))
      (setq inicio-pos (+ (position separador linea :start inicio-pos) 1))
      );loop
    );let
  );defun

;Función auxiliar que encuentra los índices de los delimitadores de una línea
(defun indices-delim(linea separador)
  (let ((indices (list)) (inicio-pos 0))
      (loop
        (when (not (position separador linea :start inicio-pos))
          (return (append indices (list (length linea)))))
        (setq indices (append indices (list (position separador linea :start inicio-pos))))
        (setq inicio-pos (1+ (position separador linea :start inicio-pos)))
        );loop
    );let
  );defun

(defun lee-separado(ruta-archivo &optional (separador '#\,))
  (let
    ((archivo nil) (num-col 0) (reng-pos 0) (col-pos 0)
      (inicio-aux 0) (tabla nil) (linea nil) (dato nil)
      );argumentos de let
      ;Abre el archivo, validando que exista
      (setq archivo (open ruta-archivo :if-does-not-exist nil))
      ;Cuenta cuantas columnas tiene el archivo
      (setq num-col (cuenta-columnas (read-line archivo) separador))
      ;Crea arreglo que contendrá la información
      (setq tabla (make-array (list 1 num-col) :adjustable t))

      ; ME FALTA CONSIDERAR ENCABEZADOS
      ; POR EL MOMENTO NO CREO QUE SEA TAN NECESARIO
      ; CREO QUE BASTARÍA CON CERRAR Y VOLVER A ABRIR EL ARCHIVO

      ;Lee cada línea del archivo y guarda los datos
      (loop
        (setq linea (read-line archivo nil))

        ;Si ya se leyó todo el archivo, regresa la tabla
        (when (not linea) (return-from lee-separado tabla))

        ;Itera sobre cada línea separando la información con los delimitadores
        (setq inicio-aux 0)
        (setq col-pos 0)
        (loop for indice-delim in (indices-delim linea separador) do
          (setq dato (subseq linea inicio-aux indice-delim))

          ;revisa si el dato es numérico o cadena
          (if (numberp (read-from-string dato))
            (setq dato (read-from-string dato))
            );if

          ;Actualiza la tabla
          (format t "~a~%~a~%~a~%~a~%" reng-pos col-pos dato indice-delim)

          (setf (aref tabla reng-pos col-pos) dato)
          (format t "~a~%" tabla)

          ;Prepara el índice de inicio de subseq para la siguiente iteración
          (setq inicio-aux (+ indice-delim 1))

          ;Prepara la columna actual en aref para la siguiente iteración
          (setq col-pos (+ col-pos 1))

          );loop de delimitadores en una línea

        ;Actualiza el índice del renglón para la siguiente iteración
        (setq reng-pos (+ reng-pos 1))

        ;Redimensiona el arreglo
        (setq tabla (adjust-array tabla (list reng-pos num-col)))
        (format t "~a~%" tabla)

        );loop de línea en el archivo
    );let
  );defun
