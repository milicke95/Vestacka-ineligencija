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
    (cond((equal input 'c)(setq player 'o)(update-player-mm))
          ((equal input 'k)(setq player 'x)(update-player-mm))
          (t(whoisplayingfirst)))))

;;pocetak igre
(defun main()
   (settablesize)
   (whoisplayingfirst)
   (settable)
   (printtable table tablesize))


(defun getelement(dest)
  (nth (- (cadr dest) 1) (nth (- (car dest) 1) table)))

(defun get-element-of-table(dest tabl)
  (nth (- (cadr dest) 1) (nth (- (car dest) 1) tabl)))

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
  (if (equal player 'o) (setq player 'x) (setq player 'o))
  (update-player-mm)
  (printtable table tablesize)
  (endofgame))

(defun update-player-mm()
    (cond ((equal player 'x) (setq player1 'o))
          (t (setq player1 'x))))

;;generise novu tablu za sledece stanje
(defun set-move(src dest tabela)
  (setq newtable table)
  (set-element src newtable '-)
  (set-element dest newtable (get-element-of-table src tabela))
  (sandwich newtable dest (get-element-of-table src tabela)))

;;ukljanja sve figure koje su u sandwich-u
(defun sandwich(ta m p)
  (sand-up (sand-down (sand-right (sand-left ta m p) m p) m p) m p))

(defun remove-sandwich-u(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-u (set-element move tabl '-) (list (1+ (car move)) (cadr move)) dest))))

(defun remove-sandwich-d(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-d (set-element move tabl '-) (list (- (car move) 1) (cadr move)) dest))))

(defun remove-sandwich-r(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-r (set-element move tabl '-) (list (car move) (1+ (cadr move))) dest))))

(defun remove-sandwich-l(tabl move dest)
  (cond ((equal move dest) tabl)
        (t (remove-sandwich-l (set-element move tabl '-) (list (car move) (- (cadr move) 1)) dest))))

(defun sand-up(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (1+ i) (1+ tablesize)) ta)
          ((or (equal p (get-element-of-table (list (1+ i) j) ta)) (equal '- (get-element-of-table (list (1+ i) j) ta))) ta)
          (t (san-up ta m p (+ i 2) j)))))

(defun san-up(ta m p i j)
  (cond ((equal i (1+ tablesize)) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-u ta (list (1+ (car m)) (cadr m)) (list i j)))
        (t (san-up ta m p (1+ i) j))))

(defun sand-down(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (- i 1) 0) ta)
          ((or (equal p (get-element-of-table (list (- i 1) j) ta)) (equal '- (get-element-of-table (list (- i 1) j) ta))) ta)
          (t (san-down ta m p (- i 2) j)))))

(defun san-down(ta m p i j)
  (cond ((equal i 0) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-d ta (list (- (car m) 1) (cadr m)) (list i j)))
        (t (san-down ta m p (- i 1) j))))

(defun sand-right(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (1+ j) (1+ tablesize)) ta)
          ((or (equal p (get-element-of-table (list i (1+ j)) ta)) (equal '- (get-element-of-table (list i (1+ j)) ta))) ta)
          (t (san-right ta m p i (+ j 2))))))

(defun san-right(ta m p i j)
  (cond ((equal j (1+ tablesize)) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-r ta (list (car m) (1+ (cadr m))) (list i j)))
        (t (san-right ta m p i (1+ j)))))

(defun sand-left(ta m p)
  (let ((i (car m)) (j (cadr m)))
    (cond ((equal (- j 1) 0) ta)
          ((or (equal p (get-element-of-table (list i (- j 1)) ta)) (equal '- (get-element-of-table (list i (- j 1)) ta))) ta)
          (t (san-left ta m p i (- j 2))))))

(defun san-left(ta m p i j)
  (cond ((equal j 0) ta)
        ((equal '- (get-element-of-table (list i j) ta)) ta)
        ((equal p (get-element-of-table (list i j) ta)) (remove-sandwich-l ta (list (car m) (- (cadr m) 1)) (list i j)))
        (t (san-left ta m p i (- j 1)))))


               
;;postavlja figuru na odredjeno polje
(defun set-element(dest ntable pl)
  (setq newtable (setelement1 ntable dest pl)) newtable)

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

(defun fieldsaway1(src dest)
  (cond((dest-up src dest) (- (car src) (car dest)))
        ((dest-bellow src dest) (- (car dest) (car src)))
        ((dest-left src dest) (- (cadr src) (cadr dest)))
        ((dest-right src dest) (- (cadr dest) (cadr src)))
        (t '0)))

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

(defun has-barrier2(src dest)
  (has-barrier3 src (getelement src) dest '1))


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


(defun endofgame1(tabl pla)
  (cond ((or (< (countx tabl) 5) (< (counto tabl) 5)) t)
        (
         (t (checkend tabl pla 1 1)))))
  
(defun kraj-igre(tabl pla)
  (if (or (win-vertical tabl pla '1) (win-diagonal-down tabl pla '1) (win-diagonal-up tabl pla '1)) t '()))

(defun win-vertical(tabl pla i)
  (cond ((equal i (1+ tablesize)) '())
        ((check-end-v tabl pla i) t)        
        (t (win-vertical tabl pla (+ 1 i)))))

(defun win-diagonal-down(tabl pla i)
  (cond ((equal i 6) '())
        ((check-end-dd tabl pla i) t)        
        (t (win-diagonal-down tabl pla (+ 1 i)))))

(defun win-diagonal-up(tabl pla i)
  (cond ((equal i 6) '())
        ((check-end-du tabl pla i) t)        
        (t (win-diagonal-up tabl pla (+ 1 i)))))

(defun check-end-v(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '3 i) tabl))
           (equal pla (get-element-of-table (list '4 i) tabl))
           (equal pla (get-element-of-table (list '5 i) tabl))
           (equal pla (get-element-of-table (list '6 i) tabl))
           (equal pla (get-element-of-table (list '7 i) tabl)))
      t '()))

(defun check-end-dd(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '3 (+ i 1)) tabl))
           (equal pla (get-element-of-table (list '4 (+ i 2)) tabl))
           (equal pla (get-element-of-table (list '5 (+ i 3)) tabl))
           (equal pla (get-element-of-table (list '6 (+ i 4)) tabl))
           (equal pla (get-element-of-table (list '7 (+ i 5)) tabl)))
      t '()))

(defun check-end-du(tabl pla i)
  (if (and (equal pla (get-element-of-table (list '7 (+ i 1)) tabl))
           (equal pla (get-element-of-table (list '6 (+ i 2)) tabl))
           (equal pla (get-element-of-table (list '5 (+ i 3)) tabl))
           (equal pla (get-element-of-table (list '4 (+ i 4)) tabl))
           (equal pla (get-element-of-table (list '3 (+ i 5)) tabl)))
      t '()))

    
        
(defun checkend(tabl pla i j)
  (cond ((equal i (1+ tablesize)) '())
        ((equal j (1+ tablesize)) (checkend tabl pla (1+ i) 1))
        ((equal (getelement (list i j)) pla)
         (if (or (checkvertical (getelement (list i j))) (checkdiagonal (getelement (list i j)))) t '()))
        (t (checkend tabl pla i (+1 j)))))


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

(defun add-to-graph(graph node successors)
               (cond((null graph) (list(list node successors)))
                     (t(list(car graph) (add-to-graph (cdr graph) node successors)))))

(defun naslednici(state move)
  (gen-all-possible-states move '1 '1 state))

(defun new-move-mm()
    (cond ((equal player1 'x) (setq player1 'o) player1)
          (t (setq player1 'x) player1)))

(defun proceni-stanje(state pla)
  (random 10))

(defun max-state (sv-list)
  ;;(format T "~A" sv-list)
  (max-state-rec (cdr sv-list) (car sv-list)))

(defun max-state-rec (sv-list state-val)
  (cond 
   ((null sv-list) state-val)
   ((> (cadar sv-list) (cadr state-val))
    (max-state-rec (cdr sv-list) (car sv-list)))
   (:else (max-state-rec (cdr sv-list) state-val))))

(defun min-state (sv-list)
  (min-state-rec (cdr sv-list) (car sv-list)))

(defun min-state-rec (sv-list state-val)
  (cond ((null sv-list) state-val)
        ((< (cadar sv-list) (cadr state-val))
         (min-state-rec (cdr sv-list) (car sv-list)))
        (t (min-state-rec (cdr sv-list) state-val))))

(defun switchplay(playe)
  (cond ((equal playe 'x) 'o)
        (t 'x)))

(defun minimax (current-state depth my-move whoplay)
  (let ((new-states (naslednici current-state whoplay)));;(new-move-mm)
    (cond 
     ((or (zerop depth) (null new-states))
      (list current-state (proceni-stanje current-state)))
     ((equal my-move T)
      (max-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) whoplay)) new-states)))
     (:else 
      (min-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) (switchplay whoplay))) new-states))))))

(defun alpha-beta (current-state depth max-depth my-move alpha beta whoplay)
  (let ((new-states (naslednici current-state whoplay)))
    (cond
     ((or (null new-states) (>= depth max-depth))
      (proceni-stanje current-state whoplay))
     ((equal my-move T)
      (dolist (new-state (naslednici current-state whoplay))
        (setf  mm(alpha-beta new-state (1+ depth) max-depth (not my-move) alpha beta whoplay))
        (setf alpha (max alpha mm))
        ;(if (>= alpha beta) beta))
        (if (>= alpha beta) (return beta)))
      alpha)
     (:else
      (dolist (new-state (naslednici current-state (switchplay whoplay)))
        (setf mm (alpha-beta new-state (1+ depth) max-depth (not my-move) alpha beta (switchplay whoplay)))
        (setf beta (min beta mm))
        ;(if (>= alpha beta) alpha))
        (if (>= alpha beta) (return alpha)))
      beta))))

(defun my-minimax-alpha-beta (start-state max-depth my-move whoplay)
  (let* ((new-states (naslednici start-state whoplay)))
    (car (max-state
          (mapcar (lambda (x) 
                    (list x (alpha-beta x '0 max-depth (not my-move) -99999 99999 whoplay)))
            new-states)))))

(defun minimax-alpha-beta (start-state max-depth my-move whoplay)
  (list start-state (alpha-beta start-state '0 max-depth my-move -99999 99999 whoplay)))

(defun alphabeta (state depth alpha beta moj-potez roditelj whoplay)
    (if (or (zerop depth) (endofgame1 state whoplay))
        (proceni-stanje state whoplay)
        (if (null moj-potez)
            (min-stanje state depth alpha beta moj-potez roditelj (sledbenici state (figura_comp moj-potez)) (list '() '100) (switchplay whoplay))
            (max-stanje state depth alpha beta moj-potez roditelj (sledbenici state (figura_comp moj-potez)) (list '() '-100) whoplay)
        )
    )
)
 
(defun max-stanje (state depth alpha beta moj-potez roditelj lp v whoplay)
    (if (null lp) v
    (let* ((v1 (max2 (alphabeta (car lp) (1- depth) alpha beta (not moj-potez) (if (null roditelj) (car lp) roditelj) whoplay) v))
        (a (maxi v1 alpha))
        )
        (if (<= beta a) v1
            (max-stanje state depth a beta moj-potez roditelj (cdr lp) v1)
        )
    )
    )
)
 
(defun min-stanje (state depth alpha beta moj-potez roditelj lp v whoplay)
    (if (null lp) v
    (let* ((v1 (min2 (alphabeta (car lp) (1- depth) alpha beta (not moj-potez) (if (null roditelj) (car lp) roditelj) whoplay) v))
        (b (mini v1 beta))
        )
        (if (<= b alpha) v1
            (min-stanje state depth alpha b moj-potez roditelj (cdr lp) v1)
        )
    )
    )
)
 
(defun maxi (p d)
    (if (> (cadr p) d) (cadr p) d))
   
(defun mini (p d)
    (if (< (cadr p) d) (cadr p) d))
 
(defun max2 (p d)
    (if (> (cadr p) (cadr d)) p d))
   
(defun min2 (p d)
    (if (< (cadr p) (cadr d)) p d))
