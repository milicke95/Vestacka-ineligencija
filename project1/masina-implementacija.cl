(setq rules '(
                           (if (kraj-igre ?x) then '(heuristika-kraj-igre ?x))
                           (if (potencijalna-opasnost-kraj-igre ?x) then '(heuristika-potencijalna-opasnost-kraj-igre ?x))
                           (if (ofanzivni-mod ?x) then (heuristika-ofanzifni-mod ?x))
                           (if (otvaranje ?x) then (heuristika-otvaranje ?x))