#LISP Notes

## Input methods
**Reading from keyboard**
+ read
+ read-line
+ read-char (reads only one character)

**Examples**

(defvar x (read))

(defvar x (read-line))

**Reading from files**

+ *open* -> creates/opens a new/existing file.
+ *with-open-file* -> usually more convenient than the above method. This method automatically closes the file.

**Example**

This example shows how to read a file (also considering the case that it doesn't exists)

~~~~
(let ((in (open "prices.csv" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
      while line do (format t "~a~%" line))
      (close in)))

~~~~

## Output methods
**Outputting to the terminal**
+ (format t "STRING")
+ (write-line "STRING")
+ (print "STRING")

**Output formatting**

|Directive|Description|
|---------|-----------|
|~A       |Displays ASCII|
|~S       |Displays s-expressions|
|~D       |Displays decimal arguments|
|~B       |Binary arguments|
|~O       |Octal arguments|
|~X       |Hexadecimal arguments|
|~C       |Character arguments|
|~F       |Fixed-format floating points|
|~E       |Exponential floating points|
|~$       |Dollar and floating point arguments|
|~%       |New line|

## Functions definition
The syntax for declaring a new function is
~~~~
(defun funcName (arg1 arg2 ...)
  (statements)
)
~~~~
**Example**
~~~~
(defun circleArea(radio)
  (defconstant PI 3.1416)
  (setq area (* PI radio radio))
  (format t "The area of a circle with radio ~F is ~F" radio area)
)
~~~~

In order to load a *.lisp* file the command *load* can be used

~~~~
(load "file-path")
~~~~

Also one can compile the file first and then load it
~~~~
(load (compile-file "hello.lisp"))
~~~~

## Equality predicates

From the more specific to the more general.
+ **eq** Returns *true* if arguments are the same, identical object.

+ **eql** Returns *true* if:
  * Arguments are **eq**.
  * If they are numbers of the same type and value.
  * They are both the same character.
+ **equal** Returns *true* if the arguments are structurally similar.
  * *Rule of thumb* Two objects are **equal** if and only if their printed representations are the same.
+  **equalp** returns *true* if arguments are equal in some sense.

+ **=** Is a particular case for **equal** in which only numbers can be used as arguments.
