(defun settablesize()
  (print "Unesite velicinu table, broj mora biti veci od 8")
  (let((size (read)))
  (cond((< size 9) (settablesize))
        (t(setq tablesize size)))))

(defun generateList(tsize char)
  (cond((zerop tsize)'())
        (t(cons char (generateList (1- tsize) char)))))

(defun generateTable(tsize)
 (cond((zerop tsize)'())
       ((> tsize (- tablesize 2)) (cons (generateList tablesize 'x) (generateTable (1- tsize))))
       ((< tsize 3) (cons (generateList tablesize 'o) (generateTable (1- tsize))))
       (t(cons(generateList tablesize '-) (generateTable (1- tsize))))))

(defun setTable()
               (setq table (generateTable tablesize)))

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

(defun whoisplayingfirst()
  (format t "~a ~a" "Unesite C ako zelite da prvi igra covek, ako zelite da prvo igra masina unesite K" #\linefeed)
  (let((input (read)))
    (cond((equal input 'c)(setq player 'c))
          ((equal input 'k)(setq player 'k))
          (t(whoisplayingfirst)))))

 (defun main()
   (settablesize)
   (whoisplayingfirst)
   (settable)
   (printtable table tablesize))