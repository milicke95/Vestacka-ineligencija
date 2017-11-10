(defun settablesize(size)
  (cond((< size 9) "Mora uneses veci broj od 8 konju :)")
        (t(setq tablesize size))))

(defun generateList(tsize char)
  (cond((zerop tsize)'())
        (t(cons char (generateList (1- tsize) char)))))

(defun generateTable(tsize)
 (cond((zerop tsize)'())
       ((> tsize (- tablesize 2)) (cons (generateList tablesize 'x) (generateTable (1- tsize))))
       ((< tsize 3) (cons (generateList tablesize 'o) (generateTable (1- tsize))))
       (t(cons(generateList tablesize '-) (generateTable (1- tsize))))))

(defun printnumbers(tsize)
  (cond((zerop tsize) (format t " "))
        (t(format t " ~d" tsize (printnumbers(- tsize 1))))))


(defun printtable1(rowNum table)
  (cond((null table) "Unesite sledeci potez")
        (t (format t "~a~b ~c" rowNum (car table) #\linefeed) (printtable1 (1+ rowNum) (cdr table)))))



(defun printtable(table tablesize)
  (printnumbers tablesize)
  (format t "~c" #\linefeed)
  (printtable1 1 table))
