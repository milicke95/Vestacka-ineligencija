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



(defconstant +very-low-number+ -999999
  "Large negative number (absolute value should be larger than any score
  returned by a heuristic.")

(defconstant +very-high-number+ 999999
  "Large number (value should be larger than any score returned by a
  heuristic.")

(defun alpha-beta (state heuristic children max-depth whoplay other)
  (let* ((player whoplay)
         (enemy other))
    (labels ((maximize (states alpha beta depth)
               (if (null states)
                   alpha
                   (let* ((state (car states))
                          (alpha (max alpha (iter state (+ depth 1) alpha beta))))
                     (if (<= beta alpha)
                         alpha
                         (maximize (cdr states) alpha beta depth)))))
             (minimize (states alpha beta depth)
               (if (null states)
                   beta
                   (let* ((state (car states))
                          (beta (min alpha (iter state (+ depth 1) alpha beta))))
                     (if (<= beta alpha)
                         beta
                         (minimize (cdr states) alpha beta depth)))))
             (iter (state depth
                          &optional (alpha +very-low-number+) (beta +very-high-number+))
               (let ((next-states (funcall children state (whosemove))))
                 (cond ((null next-states) 0)
                       ((kraj-igre state player) 10)
                       ((kraj-igre state enemy) (- 10))
                       ((= depth max-depth) (funcall heuristic state player))
                       ((eq (switchplay whoplay) enemy)
                        (maximize next-states alpha beta depth))
                       (t (minimize next-states alpha beta depth))))))
      (iter state 0))))

(defun whosemove()
  (cond ((equal player 'x) (setq player 'o) 'x) 
        (t (setq player 'x) 'o)))

(defun napravi-hash()
  (defparameter *hash-table* (make-hash-table))
  (setf (gethash 'first *hash-table*) 1)
  (setf (gethash 'second *hash-table*) 2)
  (setf (gethash 'third *hash-table*) 3)
  (setf (gethash 'fourth *hash-table*) 4))

(defun sacuvaj-hash()
  (with-open-file (str "D:\\Faks\\VII semestar\\Vestacka inteligencija\\Vestacka-ineligencija\\project1\\filename.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (maphash (lambda (key value)(format str "~a~b" (list key value) #\newline)) *hash-table*)))

(defun dodaj-u-fajl(key value)
  (with-open-file (str "D:\\Faks\\VII semestar\\Vestacka inteligencija\\Vestacka-ineligencija\\project1\\hash.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str "~a~b" (list key value) #\newline)))

(maphash (lambda (key value)(write (list key value))) *hash-table*)
(maphash (lambda (key value)(format str "~a~b" (list key value) #\newline)) *hash-table*)
  

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

