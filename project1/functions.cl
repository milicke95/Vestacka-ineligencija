;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                       F-JE ZA IGRANJE IGRICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;pocetak igre
(defun main()
  (napravi-hash)
  (connect-to-database)
  ;(load-from-hash)
  ;(maphash (lambda (key value)(format str "~a~b" (list key value) #\newline)) *hash-table*)
  (settablesize)
  (whoisplayingfirst)
  (settable)
  (printtable table tablesize)
  ;(loop while (not (endofgame)) 
  ;do (if (equal player 'x) (playmove) (play-machine))))
  (petlja-za-igranje))

;;petlja za igranje
(defun petlja-za-igranje()
  (cond ((not (kraj-igre table (diffpla player))) (if (equal player 'x) (playmove) (play-machine)) (petlja-za-igranje))
        (t (printtable table tablesize) (snapshot) (disconnect-db))))

;;masina igra potez
(defun play-machine()
  (make-move (alpha-beta table table 9 -9999 9999 t player (get-universal-time)))
  (if (equal player 'o) (setq player 'x) (setq player 'o))
  (printtable table tablesize))

;;igra potez
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
                                 ((and (= (fieldsaway source destination) 2) (has-barrier source destination) (equal (get-element-of-table destination table) '-)) (playmove1 source destination))
                                 ((equal (get-element-of-table destination table) '-) (playmove1 source destination) )
                                 (t(print "Nevalidan potez") (playmove))))))))


;;igra potez na tabeli
(defun playmove1(src dest)
  (setelement dest player)
  (setelement src '-)
  (setq played-move dest) 
  ;(update-player-mm)
  (setq table (sandwich table dest player))
  (if (equal player 'o) (setq player 'x) (setq player 'o))
  (printtable table tablesize))

(defun make-move(move)
  (cond ((exist-in-hash move) (get-from-hash move))
        ((equal (cadddr move) (diffpla player)) (setq table (load-in-hash move (car move))))
        (t (setq table (load-in-hash move (caddr move))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                   F-JE VEZANE ZA TABLU I ELEMENATA NA NJOJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;postavlja se velicina tabele
(defun settablesize()
  (print "Unesite velicinu table, broj mora biti veci od 8:")
  (let((size (read)))
  (cond((< size 9) (settablesize))
        (t(setq tablesize size)))))

;;pravljenje tabele
(defun generateTable(tsize)
 (cond((zerop tsize)'())
       ((> tsize (- tablesize 2)) (cons (generateList tablesize 'x) (generateTable (1- tsize))))
       ((< tsize 3) (cons (generateList tablesize 'o) (generateTable (1- tsize))))
       (t(cons(generateList tablesize '-) (generateTable (1- tsize))))))

;;pravljenje liste
(defun generateList(tsize char)
  (cond((zerop tsize)'())
        (t(cons char (generateList (1- tsize) char)))))


;;setuje tabelu
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
    (cond((equal input 'c)(setq player 'o)(update-player-mm))
          ((equal input 'k)(setq player 'x)(update-player-mm))
          (t(whoisplayingfirst)))))



;;vraca element sa trenutne tabele
(defun getelement(dest)
  (nth (- (cadr dest) 1) (nth (- (car dest) 1) table)))

;;vraca element sa tabele za zadato polje
(defun get-element-of-table(dest tabl)
  (cond ((or (> (car dest) tablesize) (> (cadr dest) tablesize) (< (car dest) 1) (< (cadr dest) 1)) '()) 
        (t (nth (- (cadr dest) 1) (nth (- (car dest) 1) tabl)))))

(defun setElementoflist(list ind val)
  (cond((null list)'())
        ((zerop ind) (cons val (setElementoflist (cdr list) (- ind 1) val)))
        (t(cons (car list) (setElementoflist (cdr list) (- ind 1) val)))))


;;postavlja figuru na odredjeno polje zadate tabele i vrednosti
(defun set-element(dest ntable pl)
  (setq newtable (setelement1 ntable dest pl)) newtable)

;;postalja element na zadatu poziciju sa zadatom vrednoscu
(defun setelement(dest value)
  (setq table (setelement1 table dest value)))

;;postavlja vrednost na tabeli
(defun setElement1(table dest value)
  (cond((null table) '())
        ((zerop (- (car dest) 1)) (cons (setelementoflist (car table) (- (cadr dest) 1) value) (setelement1 (cdr table) (list (- (car dest) 1) (cadr dest)) value)))
        (t(cons (car table) (setelement1 (cdr table) (list (- (car dest) 1) (cadr dest)) value)))))


(defun update-player-mm()
    (cond ((equal player 'x) (setq player1 'o))
          (t (setq player1 'x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                                 F-JE ZA PROVERU STANJA I POTEZA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;ukljanja sve figure koje su u sandwich-u
(defun sandwich(ta m p)
  (stand-in-trap (sand-up (sand-down (sand-right (sand-left ta m p) m p) m p) m p) m p))

(defun stand-in-trap(ta m p)
  (sint-up (sint-down (sint-right (sint-left ta m p) m p) m p) m p))

(defun sint-up(tabl mov pl)
  (cond ((and (<= (1+ (car mov)) tablesize) (equal (get-element-of-table (list (1+ (car mov)) (cadr mov)) tabl) (diffpla pl))) 
         (sand-down tabl (list (1+ (car mov)) (cadr mov)) (diffpla pl)))
        (t tabl)))

(defun sint-down(tabl mov pl)
  (cond ((and (>= (1- (car mov)) 1) (equal (get-element-of-table (list (1- (car mov)) (cadr mov)) tabl) (diffpla pl))) 
         (sand-up tabl (list (1- (car mov)) (cadr mov)) (diffpla pl)))
        (t tabl)))

(defun sint-right(tabl mov pl)
  (cond ((and (<= (1+ (cadr mov)) tablesize) (equal (get-element-of-table (list (car mov) (1+ (cadr mov))) tabl) (diffpla pl))) 
         (sand-left tabl (list (car mov) (1+ (cadr mov))) (diffpla pl)))
        (t tabl)))

(defun sint-left(tabl mov pl)
  (cond ((and (>= (1- (cadr mov)) 1) (equal (get-element-of-table (list (car mov) (1- (cadr mov))) tabl) (diffpla pl))) 
         (sand-right tabl (list (car mov) (1- (cadr mov))) (diffpla pl)))
        (t tabl)))

;;uklanja gore sandwich od zadate pozicije
(defun remove-sandwich-u(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-u (set-element move tabl '-) (list (1+ (car move)) (cadr move)) dest))))

;;uklanja dole sandwich od zadate pozicije
(defun remove-sandwich-d(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-d (set-element move tabl '-) (list (- (car move) 1) (cadr move)) dest))))

;;uklanja desno sandwich od zadate pozicije
(defun remove-sandwich-r(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-r (set-element move tabl '-) (list (car move) (1+ (cadr move))) dest))))

;;uklanja levo sandwich od zadate pozicije
(defun remove-sandwich-l(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-l (set-element move tabl '-) (list (car move) (- (cadr move) 1)) dest))))

;;proverava da li je upao u zamku
(defun stand-in-trap(ta m p)
  (sint-up (sint-down (sint-right (sint-left ta m p) m p) m p) m p))

(defun sint-up(tabl mov pl)
  (cond ((and (<= (1+ (car mov)) tablesize) (equal (get-element-of-table (list (1+ (car mov)) (cadr mov)) tabl) (diffpla pl))) 
         (sand-down tabl (list (1+ (car mov)) (cadr mov)) (diffpla pl)))
        (t tabl)))

(defun sint-down(tabl mov pl)
  (cond ((and (>= (1- (car mov)) 1) (equal (get-element-of-table (list (1- (car mov)) (cadr mov)) tabl) (diffpla pl))) 
         (sand-up tabl (list (1- (car mov)) (cadr mov)) (diffpla pl)))
        (t tabl)))

(defun sint-right(tabl mov pl)
  (cond ((and (<= (1+ (cadr mov)) tablesize) (equal (get-element-of-table (list (car mov) (1+ (cadr mov))) tabl) (diffpla pl))) 
         (sand-left tabl (list (car mov) (1+ (cadr mov))) (diffpla pl)))
        (t tabl)))

(defun sint-left(tabl mov pl)
  (cond ((and (>= (1- (cadr mov)) 1) (equal (get-element-of-table (list (car mov) (1- (cadr mov))) tabl) (diffpla pl))) 
         (sand-right tabl (list (car mov) (1- (cadr mov))) (diffpla pl)))
        (t tabl)))

;;pomocna f-ja za proveru gore sandwich od zadate pozicije
(defun sand-up(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (1+ i) (1+ tablesize)) ta)
          ((or (equal p (get-element-of-table (list (1+ i) j) ta)) (equal '- (get-element-of-table (list (1+ i) j) ta))) ta)
          (t (san-up ta m p (+ i 2) j)))))

;;proverava gore sandwich od zadate pozicije
(defun san-up(ta m p i j)
  (cond ((equal i (1+ tablesize)) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-u ta (list (1+ (car m)) (cadr m)) (list i j)))
        (t (san-up ta m p (1+ i) j))))

;;pomocna f-ja za proveru gore sandwich od zadate pozicije
(defun sand-down(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (- i 1) 0) ta)
          ((or (equal p (get-element-of-table (list (- i 1) j) ta)) (equal '- (get-element-of-table (list (- i 1) j) ta))) ta)
          (t (san-down ta m p (- i 2) j)))))

;;proverava dole sandwich od zadate pozicije
(defun san-down(ta m p i j)
  (cond ((equal i 0) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-d ta (list (- (car m) 1) (cadr m)) (list i j)))
        (t (san-down ta m p (- i 1) j))))

;;pomocna f-ja za proveru gore sandwich od zadate pozicije
(defun sand-right(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (1+ j) (1+ tablesize)) ta)
          ((or (equal p (get-element-of-table (list i (1+ j)) ta)) (equal '- (get-element-of-table (list i (1+ j)) ta))) ta)
          (t (san-right ta m p i (+ j 2))))))

;;proverava desno sandwich od zadate pozicije
(defun san-right(ta m p i j)
  (cond ((equal j (1+ tablesize)) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-r ta (list (car m) (1+ (cadr m))) (list i j)))
        (t (san-right ta m p i (1+ j)))))

;;pomocna f-ja za proveru gore sandwich od zadate pozicije
(defun sand-left(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (- j 1) 0) ta)
          ((or (equal p (get-element-of-table (list i (- j 1)) ta)) (equal '- (get-element-of-table (list i (- j 1)) ta))) ta)
          (t (san-left ta m p i (- j 2))))))

;;proverava levo sandwich od zadate pozicije
(defun san-left(ta m p i j)
  (cond ((equal j 0) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-l ta (list (car m) (- (cadr m) 1)) (list i j)))
        (t (san-left ta m p i (- j 1)))))


               

;;da li je potez izvan granica tabele
(defun isoutofbounds(pos)
  (cond((or (< (cadr pos) 1) (< (car pos) 1) (>(cadr pos) tablesize) (> (car pos) tablesize)) t)
        (t '())))

;;da li je odredisno polje isto kao i izvorno
(defun srcequaldest(src dest)
  (cond((equal src dest) t)
        (t '())))

;;da li je selektovana figura koja je igrac
(defun isplayerselected(pos)
  (cond((not (equal player (getelement pos))) '())
        (t t)))

;;udaljenost izmedju dva polja sa porukom
(defun fieldsaway(src dest)
  (cond((dest-up src dest) (- (car src) (car dest)))
        ((dest-bellow src dest) (- (car dest) (car src)))
        ((dest-left src dest) (- (cadr src) (cadr dest)))
        ((dest-right src dest) (- (cadr dest) (cadr src)))
        (t (print "Nevalidan potez") '0)))

;;udaljenost izmedju dva polja bez poruke
(defun fieldsaway1(src dest)
  (cond((dest-up src dest) (- (car src) (car dest)))
        ((dest-bellow src dest) (- (car dest) (car src)))
        ((dest-left src dest) (- (cadr src) (cadr dest)))
        ((dest-right src dest) (- (cadr dest) (cadr src)))
        (t '0)))

;;pomocne f-je za udaljenost
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

;;poziva f-ju za proveru barijere
(defun has-barrier(src dest)
  (has-barrier1 src dest '1))

;;proverava da li ima prepreka na putu izmedju dva zadata polja
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

;;poziva f-ju za proveru barijere
(defun has-barrier2(src dest)
  (has-barrier3 src (getelement src) dest '1))

;;proverava da li ima prepreka na putu izmedju dva zadata polja sa dodatnim argumentom
(defun has-barrier3(src sour dest it)
  (cond((not (equal (getelement dest) '-)) t)
        ((equal src dest) '())
        ((and (equal sour 'x) (equal (getelement src) 'o) ) t)
        ((and (equal sour 'x) (> it 1) (equal (getelement src) 'x)) t)
        ((and (equal sour 'o) (equal (getelement src) 'x) ) t)
        ((and (equal sour 'o) (> it 1) (equal (getelement src) 'o)) t) 
        ((dest-up src dest) (has-barrier3 (list (- (car src) 1) (cadr src)) sour dest (+ it 1)))
        ((dest-bellow src dest) (has-barrier3 (list (+ (car src) 1) (cadr src)) sour dest (+ it 1)))
        ((dest-left src dest) (has-barrier3 (list (car src) (- (cadr src) 1)) sour dest (+ it 1)))
        ((dest-right src dest) (has-barrier3 (list (car src) (+ (cadr src) 1)) sour dest (+ it 1)))))

;;proverava da li je kraj igre za zadatu tabelu i igraca  
(defun kraj-igre(tabl pla)
  (if (or (win-vertical tabl pla '1) (win-diagonal-down tabl pla '1) (win-diagonal-up tabl pla '1) (< (countx table) 5) (< (counto table) 5)) t '()))

;;proverava vertikalno da li je kraj
(defun win-vertical(tabl pla i)
  (cond ((equal i (1+ tablesize)) '())
        ((check-end-v tabl pla i) t)        
        (t (win-vertical tabl pla (+ 1 i)))))

;;proverava dijagonalno dole da li je kraj
(defun win-diagonal-down(tabl pla i)
  (cond ((equal i 6) '())
        ((check-end-dd tabl pla i) t)        
        (t (win-diagonal-down tabl pla (+ 1 i)))))

;;dijagonalno gore da li je kraj
(defun win-diagonal-up(tabl pla i)
  (cond ((equal i 6) '())
        ((check-end-du tabl pla i) t)        
        (t (win-diagonal-up tabl pla (+ 1 i)))))

;;vertikalno
(defun check-end-v(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '3 i) tabl))
           (equal pla (get-element-of-table (list '4 i) tabl))
           (equal pla (get-element-of-table (list '5 i) tabl))
           (equal pla (get-element-of-table (list '6 i) tabl))
           (equal pla (get-element-of-table (list '7 i) tabl)))
      t '()))

;;dijagonalno dole
(defun check-end-dd(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '3 i) tabl))
           (equal pla (get-element-of-table (list '4 (+ i 1)) tabl))
           (equal pla (get-element-of-table (list '5 (+ i 2)) tabl))
           (equal pla (get-element-of-table (list '6 (+ i 3)) tabl))
           (equal pla (get-element-of-table (list '7 (+ i 4)) tabl)))
      t '()))

;;dijagonalno gore
(defun check-end-du(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '7 i) tabl))
           (equal pla (get-element-of-table (list '6 (+ i 1)) tabl))
           (equal pla (get-element-of-table (list '5 (+ i 2)) tabl))
           (equal pla (get-element-of-table (list '4 (+ i 3)) tabl))
           (equal pla (get-element-of-table (list '3 (+ i 4)) tabl)))
      t '()))


;;kraj igre na osnovu poslednje odigranog poteza
(defun endofgame()
  (cond ((or (< (countx table) 5) (< (counto table) 5)) t)
        ((checkvertical (getelement played-move)) t)
        ((checkdiagonal (getelement played-move)) t)
        (t '())))

;;broji sve x na tabeli
(defun countx(tab)
  (cond ((null tab) 0)
        (t (+ (countxrow(car tab)) (countx (cdr tab))))))

;;broj x u zadatom redu
(defun countxrow(row)
  (cond ((null row) 0)
        ((equal (car row) 'x) (1+ (countxrow (cdr row))))
        (t (+ 0 (countxrow (cdr row))))))

;;broji sve o na tabeli
(defun counto(tab)
  (cond ((null tab) 0)
        (t (+ (countorow(car tab)) (counto (cdr tab))))))

;;broj o u zadatom redu
(defun countorow(row)
  (cond ((null row) 0)
        ((equal (car row) 'o) (1+ (countorow (cdr row))))
        (t (+ 0 (countorow (cdr row))))))

;;proverava vertikalno da li su figure iste
(defun checkvertical(char)
  (cond ((> (- (+ (checkvdown char played-move) (checkvup char played-move)) 1) 4) t)
        (t '())))

;;proverava vertikalno gore da li su figure iste
(defun checkvup(c move)
  (cond ((equal (car move) 2 ) 0)
        ((equal (getelement move) c) (+ 1 (checkvup c (cons (- (car move) 1) (cdr move)))))
        (t 0)))

;;proverava vertikalno dole da li su figure iste
(defun checkvdown(c move)
  (cond ((equal (car move) (- tablesize 1)) 0)
        ((equal (getelement move) c) (+ 1 (checkvdown c (cons (+ 1 (car move)) (cdr move)))))
        (t 0)))

;;proverava dijagonalno da li su figure iste
(defun checkdiagonal(char)
  (cond ((> (- (+ (checkddown char played-move) (checkdup char played-move)) 1) 4) t)
        (t '())))

;;proverava dijagonalno gore da li su figure iste
(defun checkdup(c move)
  (cond ((or (> (cadr move) tablesize) (equal (car move) 2 )) 0)
        ((equal (getelement move) c) (+ 1 (checkdup c (list (- (car move) 1) (+ 1 (cadr move))))))
        (t 0)))

;;proverava dijagonalno dole da li su figure iste
(defun checkddown(c move)
  (cond ((or (< (cadr move) 1) (equal (car move) (- tablesize 1))) 0)
        ((equal (getelement move) c) (+ 1 (checkddown c (list (+ 1 (car move)) (- (cadr move) 1)))))
        (t 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                       F-JE ZA GENERISANJE SLEDBENIKA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;generise novu tablu za sledece stanje na osnovu izvornog poteza zeljenog i tabele
(defun set-move(src dest tabela)
  (setq newtable table)
  (set-element src newtable '-)
  (set-element dest newtable (get-element-of-table src tabela))
  (sandwich newtable dest (get-element-of-table src tabela)))


;;"pla" je igrac za koga se generise a "i" i "j" su iteratori
(defun gen-all-possible-states(pla i j tabela)
  (cond ((equal i (1+ tablesize)) '())
        ((equal j (1+ tablesize)) (gen-all-possible-states pla (1+ i) 1 tabela))
        ((equal pla (get-element-of-table (list i j) tabela)) (append (next-state pla (list i j) tabela) (gen-all-possible-states pla i (1+ j) tabela)));; izmeni uslove f-je next-state da odgovaraju dole
        (t (gen-all-possible-states pla i (1+ j) tabela))))

;;generise sva stanja levo, desno, gore i dole u zavisnosti od zadate pozicije
(defun next-state(pl src tabela)
  (let* ((all-moves (append (gen-left pl src src tabela) (gen-right pl src src tabela) (gen-up pl src src tabela) (gen-down pl src src tabela))))
    (remove NIL all-moves)))

;;generise sva moguca stanja levo od zadatog polja
(defun gen-left(pl src sour tabela)
  (cond ((equal (1- (cadr src)) 0) '())
        ((not (has-barrier2 sour (list (car src) (1- (cadr src))))) (cons (set-move sour (list (car src) (1- (cadr src))) tabela) (gen-left pl (list (car src) (1- (cadr src))) sour tabela)))
        ((= (fieldsaway1 sour (list (car src) (1- (cadr src)))) 1) 
         ( cond((equal (- (cadr src) 2) 0) '()) 
               ((equal (get-element-of-table (list (car src) (- (cadr src) 2)) tabela) '-) (cons (set-move sour (list (car src) (- (cadr src) 2)) tabela) '()))
               (t '())))
        (t '())))

;;generise sva moguca stanja desno od zadatog polja
(defun gen-right(pl src sour tabela)
  (cond ((equal (1+ (cadr src)) 10) '())
        ((not (has-barrier2 sour (list (car src) (1+ (cadr src))))) (cons (set-move sour (list (car src) (1+ (cadr src))) tabela) (gen-right pl (list (car src) (1+ (cadr src))) sour tabela)))
        ((= (fieldsaway1 sour (list (car src) (1- (cadr src)))) 1) 
         (cond((equal (+ (cadr src) 2) 10) '()) 
               ((equal (get-element-of-table (list (car src) (+ (cadr src) 2)) tabela) '-) (cons (set-move sour (list (car src) (+ (cadr src) 2)) tabela) '()))
               (t '())))
        (t '())))

;;generise sva moguca stanja dole od zadatog polja
(defun gen-down(pl src sour tabela)
  (cond ((equal (1- (car src)) 0) '())
        ((not (has-barrier2 sour (list (1- (car src)) (cadr src)))) (cons (set-move sour (list (1- (car src)) (cadr src)) tabela) (gen-down pl (list (1- (car src)) (cadr src)) sour tabela)))
        ((= (fieldsaway1 sour (list (car src) (1- (cadr src)))) 1) 
         (cond((equal (- (car src) 2) 0) '()) 
               ((equal (get-element-of-table (list (- (car src) 2) (cadr src)) tabela) '-) (cons (set-move sour (list (- (car src) 2) (cadr src)) tabela) '()))
               (t '())))
        (t '())))

;;generise sva moguca stanja gore od zadatog polja
(defun gen-up(pl src sour tabela)
  (cond ((equal (1+ (car src)) 10) '())
        ((not (has-barrier2 sour (list (1+ (car src)) (cadr src)))) (cons (set-move sour (list (1+ (car src)) (cadr src)) tabela) (gen-up pl (list (1+ (car src)) (cadr src)) sour tabela)))
        ((= (fieldsaway1 sour (list (car src) (1- (cadr src)))) 1) 
         (cond((equal (+ (car src) 2) 10) '()) 
               ((equal (get-element-of-table (list (+ (car src) 2) (cadr src)) tabela) '-) (cons (set-move sour (list (+ (car src) 2) (cadr src)) tabela) '()))
               (t '())))
        (t '())))

;;pravi graf od naslednika
(defun add-to-graph(graph node successors)
               (cond((null graph) (list(list node successors)))
                     (t(list(car graph) (add-to-graph (cdr graph) node successors)))))

;;lepsi naziv f-je za generisanje sledbenika
(defun naslednici(state move)
  (gen-all-possible-states move '1 '1 state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                       F-JE ZA MINMAX I ALPHA-BETA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;menja igraca
(defun new-move-mm()
    (cond ((equal player1 'x) (setq player1 'o) player1)
          (t (setq player1 'x) player1)))

;;procena stanja za testiranje
(defun proceni-stanje(state pla)
  (random 10))

;;proverava max za stanja
(defun max-state (sv-list)
  ;;(format T "~A" sv-list)
  (max-state-rec (cdr sv-list) (car sv-list)))

;;racuna max za stanja
(defun max-state-rec (sv-list state-val)
  (cond 
   ((null sv-list) state-val)
   ((> (cadar sv-list) (cadr state-val))
    (max-state-rec (cdr sv-list) (car sv-list)))
   (:else (max-state-rec (cdr sv-list) state-val))))

;;proverava min za stanja
(defun min-state (sv-list)
  (min-state-rec (cdr sv-list) (car sv-list)))

;;racunua min za stanja
(defun min-state-rec (sv-list state-val)
  (cond ((null sv-list) state-val)
        ((< (cadar sv-list) (cadr state-val))
         (min-state-rec (cdr sv-list) (car sv-list)))
        (t (min-state-rec (cdr sv-list) state-val))))

;;minmax klasican
(defun minimax (current-state depth my-move whoplay)
  (let ((new-states (naslednici current-state whoplay)));;(new-move-mm)
    (cond 
     ((or (zerop depth) (null new-states))
      (list current-state (heuristic current-state)))
     ((equal my-move T)
      (max-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) whoplay)) new-states)))
     (:else 
      (min-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) (diffpla whoplay))) new-states))))))

;;menja igraca
(defun diffpla(pl)
  (cond ((equal pl 'x) 'o)
        (t 'x)))

;;minmax sa alpha-beta
(defun alpha-beta(state parent max-depth alpha beta moj-potez pl start-time)
  (cond ((or (zerop max-depth) (> (- (get-universal-time) start-time ) end-time)) (list state (heuristic1 state) parent pl max-depth))
        (t (if (null moj-potez)             
               (min-stanje state max-depth alpha beta moj-potez (naslednici state pl) pl (list '() '1000) start-time)
             (max-stanje state max-depth alpha beta moj-potez (naslednici state pl) pl (list '() '-1000) start-time)))))

;;proverava za max stanje
(defun max-stanje (state depth alpha beta moj-potez lp pl v start-time)
    (if (null lp) v
    (let* ((v1 (max2 (alpha-beta (car lp) state (1- depth) alpha beta (not moj-potez) (diffpla pl) start-time) v))
        (a (max1 v1 alpha))
        )
        (if (<= beta a) v1
            (max-stanje state depth a beta moj-potez (cdr lp) pl v1 start-time)
        )
    )
    )
)

;;proverava za min stanje
(defun min-stanje (state depth alpha beta moj-potez lp pl v start-time)
    (if (null lp) v
    (let* ((v1 (min2 (alpha-beta (car lp) state (1- depth) alpha beta (not moj-potez) (diffpla pl) start-time) v))
        (b (min1 v1 beta))
        )
        (if (<= b alpha) v1
            (min-stanje state depth alpha b moj-potez (cdr lp) pl v1 start-time)
        )
    )
    )
  )
;;uporedjuje vrednosti za max
(defun max1 (p d)
    (if (> (cadr p) d) (cadr p) d))
   
(defun max2 (p d)
    (if (> (cadr p) (cadr d)) p d))

;;uporedjuje vrednosti za min
(defun min1 (p d)
    (if (< (cadr p) d) (cadr p) d))

(defun min2 (p d)
  (if (< (cadr p) (cadr d)) p d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                       F-JE HASH TABELE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;pravi hash sa datim nazivom
(defun napravi-hash()
  (defparameter *hash-table* (make-hash-table :size 100000)))

;;ispituje da li postoji u hash-u taj kljuc
(defun exist-in-hash(key)
  (if (gethash key *hash-table*) t '()))

;;vraca element iz hesa za zadatim kljucem
(defun get-from-hash(key)
  (gethash key *hash-table*))

;;uklanja iz hash-a sa zadatim kljucem
(defun remove-from-hash(key)
  (remhash key *hash-table*))

;;upisuje u fajl hash 
(defun dodaj-u-fajl(key value)
  (with-open-file (str "D:\\GitHub\\Vestacka-ineligencija\\project1\\hash.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str "~a~b" (list key value) #\newline)))

;;snima kljuc i vrenost u hash pa zatim u fajl
(defun save-in-hash(key value)
  (load-in-hash key value)
  (dodaj-u-fajl key value)
  (get-from-hash key))

;;uctiava sve vrednosti iz fajla u hash
(defun load-from-hash()
  (let ((in (open "D:\\GitHub\\Vestacka-ineligencija\\project1\\hash.txt"
                  :if-does-not-exist nil)))
    (when in
      (loop for line = (read in nil)
            ;while line do (load-in-hash (car (from-string-to-list line)) (cadr (from-string-to-list line))))
            while line do (load-in-hash (car line) (cadr line)))
      (close in))))


;;postavlja kljuc sa vrednostcu u hash
(defun load-in-hash(key value)
  (setf (gethash key *hash-table*) value))

;;konverzija iz stringa u listu potrebnu za fajl
(defun from-string-to-list(s)
  (let ((L (read-from-string 
           (concatenate 'string s))))
    L))

;;snimanje u redis
(defun save-redis(key value)
  (red:set key value)
  (find-redis key))

;;pribavljanje iz redisa
(defun find-redis(key)
  (cond ((red:get key) (from-string-to-list (red:get key)))
        (t '())))

;;snimi snapshot
(defun snapshot()
  (red:save))

;;pravljenje konekcije
(defun connect-to-database()
  (redis:connect :host "127.0.0.1" :port '6379))

;;disconnectovanje
(defun disconnect-db()
  (redis:disconnect))

;(ql:quickload 'cl-redis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                                       F-JE HEURISTIKE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Da li se na toj poziciji nalazi zadati element
(defun checkElement(dest el table)
               (cond((equal dest '-) '())
                     ((equal (get-element-of-table dest table) el) 't)))

;;broj zadatih elemenata levo od zadate pozicije 
(defun get-number-of-elements-left(src state number)
  (cond((= (cadr src) 1) number)
        ((equal (get-element-of-table (list (car src)  (- (cadr src) 1)) state) 'x) (get-number-of-elements-left (list (car src) (- (cadr src) 1)) state (+ number 1)))
        (t number)))

;;broj zadatih elemenata desno od zadate pozicije
(defun get-number-of-elements-right(src state number)
  (cond ((= (cadr src) tablesize) number)
        ((equal (get-element-of-table (list (car src)  (+ (cadr src) 1)) state) 'x) (get-number-of-elements-right (list (car src) (+ (cadr src) 1)) state (+ number 1)))
        (t number)))

;;broj zadatih elemenata gore od zadate pozicije
(defun get-number-of-elements-top(src state number)
               (cond ((<= (car src) '3) number)
                     ((equal (get-element-of-table (list (- (car src) 1)  (cadr src)) state) 'x) (get-number-of-elements-top (list (- (car src) 1)  (cadr src)) state (+ number 1)))
                     (t number)))

;;broj zadatih elemenata dole od zadate pozicije
(defun get-number-of-elements-bottom(src state number)
               (cond ((>= (car src) (- tablesize 2)) number)
                     ((equal (get-element-of-table (list (+ (car src) 1)  (cadr src)) state) 'x) (get-number-of-elements-bottom (list (+ (car src) 1)  (cadr src)) state (+ number 1)))
                     (t number)))

;;broj zadatih elemenata gore desno od zadate pozicije
(defun get-number-of-elements-top-right(src state number)
  (cond((<= (car src) '3) number)
        ((equal (cadr src) tablesize) number)
                     ((checkelement (list (- (car src) 1) (+ (cadr src) 1)) 'x state) (get-number-of-elements-top-right (list (- (car src) 1) (+ (cadr src) 1)) state (+ number 1)))
                     (t number)))

;;broj zadatih elemenata gore levo od zadate pozicije
(defun get-number-of-elements-top-left(src state number)
  (cond((<= (car src) '3) number)
        ((equal (cadr src) '1) number)
                     ((checkelement (list (- (car src) 1) (- (cadr src) 1)) 'x state) (get-number-of-elements-top-left (list (- (car src) 1) (- (cadr src) 1)) state (+ number 1)))
        (t number)))

;;broj zadatih elemenata dole desno od zadate pozicije
(defun get-number-of-elements-bottom-right(src state number)
  (cond((>= (car src) (- tablesize 2)) number)
        ((equal (cadr src) tablesize) number)
                     ((checkelement (list (+ (car src) 1) (+ (cadr src) 1)) 'x state) (get-number-of-elements-bottom-right (list (+ (car src) 1) (+ (cadr src) 1)) state (+ number 1)))
                     (t number)))

;;broj zadatih elemenata dole levo od zadate pozicije
(defun get-number-of-elements-bottom-left(src state number)
               (cond((>= (car src) (- tablesize 2)) number)
                     ((equal (cadr src) '1) number)
                     ((checkelement (list (+ (car src) 1) (- (cadr src) 1)) 'x state) (get-number-of-elements-bottom-left (list (+ (car src) 1) (- (cadr src) 1)) state (+ number 1)))
                     (t number)))

;;maksimalni broj koji predstavlja koliko se maksimalno elemenata nalazi sa odredjene strane src-a za moguci sendvic
(defun get-number-of-elements(src state)
                (let(
                     (top (get-number-of-elements-top src state '0))
                     (bottom (get-number-of-elements-bottom src state '0))
                     (left (get-number-of-elements-left src state '0))
                     (right (get-number-of-elements-right src state '0))
                     (top-right (get-number-of-elements-top-right src state '0))
                     (top-left (get-number-of-elements-top-left src state '0))
                     (bottom-right (get-number-of-elements-bottom-right src state '0))
                     (bottom-left (get-number-of-elements-bottom-left src state '0)))
                     (cond((and (>= top bottom) (>= top left) (>= top right) (>= top top-right) (>= top top-left) (>= top bottom-right) (>= top bottom-left)) top)
                           ((and (>= bottom top) (>= bottom left) (>= bottom right) (>= bottom top-right) (>= bottom top-left) (>= bottom bottom-right) (>= bottom bottom-left)) bottom)
                           ((and (>= top-right top) (>= top-right left) (>= top-right right) (>= top-right bottom) (>= top-right top-left) (>= top-right bottom-right) (>= top-right bottom-left)) top-right)
                           ((and (>= top-left top) (>= top-left left) (>= top-left right) (>= top-left bottom) (>= top-left top-right) (>= top-left bottom-right) (>= top-left bottom-left)) top-left)
                           ((and (>= bottom-right top) (>= bottom-right left) (>= bottom-right right) (>= bottom-right bottom) (>= bottom-right top-right) (>= bottom-right top-left) (>= bottom-right bottom-left)) bottom-right)
                           ((and (>= bottom-left top) (>= bottom-left left) (>= bottom-left right) (>= bottom-left bottom)  (>= bottom-left top-right) (>= bottom-left top-left) (>= bottom-left bottom-right)) bottom-left)
                           ((and (>= left top) (>= left bottom) (>= left right) (>= left top-right) (>= left top-left) (>= left bottom-right) (>= left bottom-left)) left)
                           ((and (>= right top) (>= right bottom) (>= right left) (>= right top-right) (>= right top-left) (>= right bottom-right) (>= right bottom-left)) right))))
               
(defun get-number-of-elements-o(src state)
  (let(
       (top (get-number-of-elements-top-o src state '0))
       (bottom (get-number-of-elements-bottom-o src state '0))
       (top-right (get-number-of-elements-top-right-o src state '0))
       (top-left (get-number-of-elements-top-left-o src state '0))
       (bottom-right (get-number-of-elements-bottom-right-o src state '0))
       (bottom-left (get-number-of-elements-bottom-left-o src state '0)))
    (cond((and (>= top bottom) (>= top top-right) (>= top top-left) (>= top bottom-right) (>= top bottom-left)) top)
          ((and (>= bottom top)  (>= bottom top-right) (>= bottom top-left) (>= bottom bottom-right) (>= bottom bottom-left)) bottom)
          ((and (>= top-right top)  (>= top-right bottom) (>= top-right top-left) (>= top-right bottom-right) (>= top-right bottom-left)) top-right)
          ((and (>= top-left top)  (>= top-left bottom) (>= top-left top-right) (>= top-left bottom-right) (>= top-left bottom-left)) top-left)
          ((and (>= bottom-right top)  (>= bottom-right bottom) (>= bottom-right top-right) (>= bottom-right top-left) (>= bottom-right bottom-left)) bottom-right)
          ((and (>= bottom-left top) (>= bottom-left bottom)  (>= bottom-left top-right) (>= bottom-left top-left) (>= bottom-left bottom-right)) bottom-left))))

(defun calculate-adjecent-friendly-pawns1(state number i j)
  (cond((and (= i tablesize) (> j tablesize)) number)
        ((> j tablesize) (calculate-adjecent-friendly-pawns1 state number (+ i 1) '1))
        ((and (> i 2) (<= i (- tablesize 2)) (< number (+ (get-number-of-elements-o (list i j) state) 1)) (equal (get-element-of-table (list i j) state) 'o)) (calculate-adjecent-friendly-pawns1 state (+ (get-number-of-elements-o (list i j) state) 1) i (+ j 1)))
        (t(calculate-adjecent-friendly-pawns1 state number i (+ j 1)))))


(defun calculate-adjecent-friendly-pawns(state)
  (calculate-adjecent-friendly-pawns1 state '0 '1 '1))

(defun get-number-of-elements-top-o(src state number)
               (cond ((<= (car src) '3) number)
                     ((equal (get-element-of-table (list (- (car src) 1)  (cadr src)) state) 'o) (get-number-of-elements-top-o (list (- (car src) 1)  (cadr src)) state (+ number 1)))
                     (t number)))

;;broj o elemenata dole od zadate pozicije
(defun get-number-of-elements-bottom-o(src state number)
               (cond ((>= (car src) (- tablesize 2)) number)
                     ((equal (get-element-of-table (list (+ (car src) 1)  (cadr src)) state) 'o) (get-number-of-elements-bottom-o (list (+ (car src) 1)  (cadr src)) state (+ number 1)))
                     (t number)))

;;broj o elemenata gore desno od zadate pozicije
(defun get-number-of-elements-top-right-o(src state number)
  (cond((<= (car src) '3) number)
        ((equal (cadr src) tablesize) number)
                     ((checkelement (list (- (car src) 1) (+ (cadr src) 1)) 'o state) (get-number-of-elements-top-right-o (list (- (car src) 1) (+ (cadr src) 1)) state (+ number 1)))
                     (t number)))

;;broj o elemenata gore levo od zadate pozicije
(defun get-number-of-elements-top-left-o(src state number)
  (cond((<= (car src) '3) number)
        ((equal (cadr src) '1) number)
                     ((checkelement (list (- (car src) 1) (- (cadr src) 1)) 'o state) (get-number-of-elements-top-left-o (list (- (car src) 1) (- (cadr src) 1)) state (+ number 1)))
        (t number)))

;;broj o elemenata dole desno od zadate pozicije
(defun get-number-of-elements-bottom-right-o(src state number)
  (cond((>= (car src) (- tablesize 2)) number)
        ((equal (cadr src) tablesize) number)
                     ((checkelement (list (+ (car src) 1) (+ (cadr src) 1)) 'o state) (get-number-of-elements-bottom-right-o (list (+ (car src) 1) (+ (cadr src) 1)) state (+ number 1)))
                     (t number)))

;;broj o elemenata dole levo od zadate pozicije
(defun get-number-of-elements-bottom-left-o(src state number)
               (cond((>= (car src) (- tablesize 2)) number)
                     ((equal (cadr src) '1) number)
                     ((checkelement (list (+ (car src) 1) (- (cadr src) 1)) 'o state) (get-number-of-elements-bottom-left-o (list (+ (car src) 1) (- (cadr src) 1)) state (+ number 1)))
                     (t number)))

(defun number-of-possible-attacks1(state i j number)
              (cond((and (= i tablesize) (> j tablesize)) number)
                    ((> j tablesize) (number-of-possible-attacks1 state (+ 1 i) '1 number))
                    ((and (>= (get-number-of-elements (list i j) state) 2) (equal (get-element-of-table (list i j) state) 'o)) (number-of-possible-attacks1 state i (+ 1 j) (+ 1 number)))
                    (t(number-of-possible-attacks1 state i (+ j 1) number))))

(defun number-of-possible-attacks(state)
  (number-of-possible-attacks1 state '1 '1 '0))

(defun get-number-of-elements-for-sandwich1(state number overlap i j)
                (cond((and (equal i tablesize) (equal j (+ tablesize 1))) (list number overlap))
                      ((> j tablesize) (get-number-of-elements-for-sandwich1 state number overlap (+ i 1) '1))
                      ((and (< number (get-number-of-elements (list i j) state)) (equal (get-element-of-table (list i j) state) 'o)) (get-number-of-elements-for-sandwich1 state (get-number-of-elements (list i j) state) '0 i (+ j 1)))
                      ((and (= number (get-number-of-elements (list i j) state)) (equal (get-element-of-table (list i j) state) 'o)) (get-number-of-elements-for-sandwich1 state number (+ 1 overlap) i (+ j 1)))
                      (t(get-number-of-elements-for-sandwich1 state number overlap i (+ j 1)))))


(defun number-of-adjecent-friendly-pawns1(state i j number)
              (cond((and (= i tablesize) (> j tablesize)) (floor number 2))
                    ((> j tablesize) (number-of-adjecent-friendly-pawns1 state (+ 1 i) '1 number))
                    ((and (> i 2) (<= i (- tablesize 2)) (> (get-number-of-elements-o (list i j) state) 0) (equal (get-element-of-table (list i j) state) 'o)) (number-of-adjecent-friendly-pawns1 state i (+ 1 j) (+ 1 number)))
                    (t(number-of-adjecent-friendly-pawns1 state i (+ j 1) number))))

(defun number-of-adjecent-friendly-pawns(state)
               (number-of-adjecent-friendly-pawns1 state '1 '1 '0))



;;za dato stanje racuna maksimalni broj elemenata za moguci sendvic
(defun get-number-of-elements-for-sandwich(state)
  (get-number-of-elements-for-sandwich1 state '0 '0 '1 '1))

;;racuna score prema razlici oksa i xsa
(defun score(tabela)
  (- (counto tabela) (countx tabela)))

;;racuna ukupan score u odnosu na parenta
(defun calculate-score(current successor)
               (let(
                    (result (- (score successor) (score current))))
                 (cond((> result '0) (* result 20))
                       (t result))))

;;heuristika
(defun heuristic(state)
                (cond((kraj-igre state 'o) '1000)
                      ((> (car (get-number-of-elements-for-sandwich state)) 1) (+ 10 (* (calculate-adjecent-friendly-pawns state) 6) (number-of-adjecent-friendly-pawns state) (calculate-score table state) (number-of-possible-attacks state) (+ (car (get-number-of-elements-for-sandwich state)) (cadr(get-number-of-elements-for-sandwich state)))))
                      (t(+ (* (calculate-adjecent-friendly-pawns state) 6) (number-of-adjecent-friendly-pawns state) (calculate-score table state)))))

;;ispitivanje sa hash-om
(defun heuristic1(state)
  (cond ((find-redis state) (find-redis state))
        (t (save-redis state (heuristic state)))))