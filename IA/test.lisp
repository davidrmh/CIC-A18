;Opens a file and prints all its lines

(let ((in (open "prices.csv" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
      while line do (format t "~a~%" line))
      (close in)))
