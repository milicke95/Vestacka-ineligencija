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
                 (cond((atom source) (print "Potez morate uneti kao listu npr (3 4)") (playmove))
                       ((isoutofbounds source) (print "Nevalidan potez, izasli ste iz opsega") (playmove))
                       ((not(isplayerselected source)) (print "Morate selektovati svoju figuru") (playmove))
                       (t(let ((destination (read)))
                           (cond((atom destination) (print "Odrediste mora biti lista npr (3 4)") (playmove))
                                 ((isoutofbounds destination) (print "Nevalidan potez, izasli ste iz opsega") (playmove))
                                 ((srcequaldest source destination) (print "Nevalidan potez, odrediste ne sme biti jednako polaznom polju") (playmove))
                                 ((= (fieldsaway source destination) 0) (print "Nevalidan potez, potezi se mogu odigrati vertikalno ili horizontalno") (playmove))
                                 ((and (> (fieldsaway source destination) 2) (has-barrier source destination)) (print "Nevalidan potez, imate prepreku na putu") (playmove))
                                 ((and (= (fieldsaway source destination) 2) (has-barrier source destination)) (playmove1 source destination))
                                 (t (playmove1 source destination))))))))

(defun playmove1(src dest)
  (setelement dest player)
  (setelement src '-)
  (setq played-move dest)
  (printtable table tablesize)
  (endofgame))


(defun isoutofbounds(pos)
  (cond((or (< (cadr pos) 1) (< (car pos) 1) (>(cadr pos) tablesize) (> (car pos) tablesize)) t)
        (t '())))

(defun srcequaldest(src dest)
  (cond((equal src dest) t)
        (t '())))


(defun isplayerselected(pos)
  (cond((not (equal player (getelement pos))) '())
        (t t)))

(defun dest-bellow(src dest)
  (cond((and (< (car src) (car dest)) (= (cadr src) (cadr dest))) t)
        (t '())))

(defun dest-up(src dest)
  (cond((and (> (car src) (car dest)) (= (cadr src) (cadr dest))) t)
        (t '())))


(defun dest-left(src dest)
  (cond((and (> (cadr src) (cadr dest)) (= (car src) (car dest))) t)
        (t '())))

(defun dest-right(src dest)
  (cond((and (< (cadr src) (cadr dest)) (= (car src) (car dest))) t)
        (t '())))


(defun fieldsaway(src dest)
  (cond((dest-up src dest) (- (car src) (car dest)))
        ((dest-bellow src dest) (- (car dest) (car src)))
        ((dest-left src dest) (- (cadr src) (cadr dest)))
        ((dest-right src dest) (- (cadr dest) (cadr src)))
        (t (print "Nevalidan potez") '0)))

(defun has-barrier(src dest)
 (has-barrier1 src dest '1))


(defun has-barrier1(src dest it)
  (cond((not (equal (getelement dest) '-)) t)
                     ((equal src dest) '())
                     ((and (equal player 'x) (equal (getelement src) 'o) ) t)
                     ((and (equal player 'x) (> it 1) (equal (getelement src) 'x)) t)
                     ((and (equal player 'o) (equal (getelement src) 'x) ) t)
                     ((and (equal player 'o) (> it 1) (equal (getelement src) 'o)) t) 
                     ((dest-up src dest) (has-barrier1 (list (- (car src) 1) (cadr src)) dest (+ it 1)))
                     ((dest-bellow src dest) (has-barrier1 (list (+ (car src) 1) (cadr src)) dest (+ it 1)))
                     ((dest-left src dest) (has-barrier1 (list (car src) (- (cadr src) 1)) dest (+ it 1)))
                     ((dest-right src dest) (has-barrier1 (list (car src) (+ (cadr src) 1)) dest (+ it 1)))))


(defun endofgame()
  (cond ((or (< (countx table) 5) (< (counto table) 5)) t)
        ((checkvertical (getelement played-move)) t)
        ((checkdiagonal (getelement played-move)) t)
        (t '())))

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
  (cond ((> (- (+ (checkvdown char played-move) (checkvup char played-move)) 1) 4) t)
        (t '())))

(defun checkvup(c move)
  (cond ((equal (car move) 2 ) 0)
        ((equal (getelement move) c) (+ 1 (checkvup c (cons (- (car move) 1) (cdr move)))))
        (t 0)))

(defun checkvdown(c move)
  (cond ((equal (car move) (- tablesize 1)) 0)
        ((equal (getelement move) c) (+ 1 (checkvdown c (cons (+ 1 (car move)) (cdr move)))))
        (t 0)))

(defun checkdiagonal(char)
  (cond ((> (- (+ (checkddown char played-move) (checkdup char played-move)) 1) 4) t)
        (t '())))

(defun checkdup(c move)
  (cond ((or (> (cadr move) tablesize) (equal (car move) 2 )) 0)
        ((equal (getelement move) c) (+ 1 (checkdup c (list (- (car move) 1) (+ 1 (cadr move))))))
        (t 0)))

(defun checkddown(c move)
  (cond ((or (< (cadr move) 1) (equal (car move) (- tablesize 1))) 0)
        ((equal (getelement move) c) (+ 1 (checkddown c (list (+ 1 (car move)) (- (cadr move) 1)))))
        (t 0)))
