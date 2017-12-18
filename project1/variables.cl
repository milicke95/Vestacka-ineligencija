(defvar tablesize 9)
(defvar table (generateTable tablesize))
(defvar played-move '())
(defvar valuemoja 5)
(defvar newtable '())
(defvar graph-of-states (add-to-graph '() table (car (gen-all-possible-states 'x '1 '1 table))))

