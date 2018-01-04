(defun proceni-stanje (stanje)
  (case stanje
    ((K) 2)
    ((L) 3)
    ((M) 5)
    ((N) 9)
    ((O) 0)
    ((P) 7)
    ((Q) 4)
    ((R) 2)
    ((S) 1)
    ((T) 5)
    ((U) 6)
    (t 0)))

(defun nova-stanja (stanje)
  (case stanje
    ((A) '(B C D))
    ((B) '(E F))
    ((C) '(G H))
    ((D) '(I J))
    ((E) '(K L))
    ((F) '(M N))
    ((G) '(O))
    ((H) '(P Q))
    ((I) '(R S))
    ((J) '(T U))
    (t '())))

(defun minimax (current-state depth my-move )
  (let ((new-states (nova-stanja current-state)))
    (cond 
     ((or (zerop depth) (null new-states))
      (list current-state (proceni-stanje current-state)))
     ((equal my-move T)
      (max-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) current-state)) new-states)))
     (:else 
      (min-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) current-state)) new-states))))))

(defun minimax (current-state depth my-move start-state)
  (let ((new-states (list (naslednici current-state (new-move-mm)))))
    (cond 
     ((or (zerop depth) (null new-states))
      (list current-state (proceni-stanje current-state)))
     ((equal my-move T)
      (max-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) start-state)) new-states)))
     (:else 
      (min-state (mapcar (lambda (x)
                           (minimax x (1- depth)
                                    (not my-move) start-state)) new-states))))))

(defun proceni_triv (tabla red kol)
  
  
  (let ((xvr (1+ red)) (yvr (- realn red)))
  (cond
  ((equalp red 7) 0)

  ((equal (nth kol (nth red tabla)) 'x) 
    (if (equal (1+ kol) realN)
        (+ (proceni_triv tabla (1+ red) 0) xvr)
        (+ (proceni_triv tabla red (1+ kol)) xvr) )
   )
 
  ((equal (nth kol (nth red tabla)) 'o)
    (if (equal (1+ kol) realN)
        (- (proceni_triv tabla (1+ red) 0) yvr)
        (- (proceni_triv tabla red (1+ kol)) yvr) )
   )

  ((equal (nth kol (nth red tabla)) '-)    
    (if (equal (1+ kol) realN)
        (+ 0 (proceni_triv tabla (1+ red) 0))
        (+ 0 (proceni_triv tabla red (1+ kol))) )
   )
   
   )
    )
  
  )

(defun proc (tabla)
  (proceni_triv tabla 2 0)
  )