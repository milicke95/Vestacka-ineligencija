(defun settablesize()
  (print "Unesite velicinu table, broj mora biti veci od 8:")
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

;;
(defun setTable()
  (setq table (generateTable tablesize))
  (if (equal player 'x) (setq table (reverse table))))

;;ispisivanje brojeva vrsta i kolona
(defun printnumbers(tsize)
  (cond((zerop tsize) (format t " "))
        (t(format t " ~d" tsize (printnumbers(- tsize 1))))))

;;crtanje jedne vrste
(defun printtable1(rowNum table)
  (cond((null table) "Unesite sledeci potez:")
        (t (format t "~a~b ~c" rowNum (car table) #\linefeed) (printtable1 (1+ rowNum) (cdr table)))))


;;crtanje polja
(defun printtable(table tablesize)
  (printnumbers tablesize)
  (format t "~c" #\linefeed)
  (printtable1 1 table))

;;korisnik bira ko igra prvi
(defun whoisplayingfirst()
  (format t "~a ~a" "Unesite K ako zelite da prvi igra covek, ako zelite da prvo igra masina unesite C:" #\linefeed)
  (let((input (read)))
    (cond((equal input 'c)(setq player 'o))
          ((equal input 'k)(setq player 'x))
          (t(whoisplayingfirst)))))

;;pocetak igre
(defun main()
   (settablesize)
   (whoisplayingfirst)
   (settable)
   (printtable table tablesize))


(defun getelement(dest)
  (nth (- (cadr dest) 1) (nth (- (car dest) 1) table)))


(defun setElementoflist(list ind val)
  (cond((null list)'())
        ((zerop ind) (cons val (setElementoflist (cdr list) (- ind 1) val)))
        (t(cons (car list) (setElementoflist (cdr list) (- ind 1) val)))))


(defun setElement1(table dest value)
  (cond((null table) '())
        ((zerop (- (car dest) 1)) (cons (setelementoflist (car table) (- (cadr dest) 1) value) (setelement1 (cdr table) (list (- (car dest) 1) (cadr dest)) value)))
        (t(cons (car table) (setelement1 (cdr table) (list (- (car dest) 1) (cadr dest)) value)))))


(defun setelement(dest value)
  (setq table (setelement1 table dest value)))


(defun playmove()
  (let((source (read)))
   (cond((atom source) (print "Potez morate uneti  kao listu npr (3 4)") (playmove))
         ((or (< (cadr source) 1) (< (car source) 1) (>(cadr source) tablesize) (> (car source) tablesize)) (print "Nevalidan potez, izasli ste iz opsega") (playmove))
         (t(let ((destination (read)))
             (cond((atom destination) (print "Odrediste mora biti lista npr (3 4)") (playmove))
                   ((or (< (cadr destination) 1) (< (car destination) 1) (> (cadr destination) tablesize) (> (car destination) tablesize)) (print "Nevalidan potez, izasli ste iz opsega") (playmove))))))))

(defun endofgame()
  (cond ((or (< (countx table) 5) (< (counto table) 5)) t)
        ((checkvertical (getelement played-move)) t)
        ((checkdiagonal (getelement played-move)) t)
        (t ('()))))

(defun countx(tab)
  (cond ((null tab) 0)
        (t (+ (countxrow(car tab)) (countx (cdr tab))))))

(defun countxrow(row)
  (cond ((null row) 0)
        ((equal (car row) 'x) (1+ (countxrow (cdr row))))
        (t (+ 0 (countxrow (cdr row))))))

(defun counto(tab)
  (cond ((null tab) 0)
        (t (+ (countorow(car tab)) (counto (cdr tab))))))

(defun countorow(row)
  (cond ((null row) 0)
        ((equal (car row) 'x) (1+ (countorow (cdr row))))
        (t (+ 0 (countorow (cdr row))))))

(defun checkvertical(char)
  (cond ((> (- (+ (checkdown char played-move) (checkup char played-move)) 1) 4) t)
        (t ('()))))

(defun checkup(c move)
  (cond ((equal (car move) (- tablesize 2)) 0)
        ((equal (getelement move) c) (1+ (checkup c (cons (1+ (car move)) (cdr move)))))
        (t (0))))

(defun checkdown(c move)
  (cond ((equal (car move) 2) 0)
        ((equal (getelement move) c) (1+ (checkdown c (cons (- (car move) 1) (cdr move)))))
        (t (0))))