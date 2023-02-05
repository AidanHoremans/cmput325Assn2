(defun fl-interp (E P)
    (xeval E P nil nil)
)

; for pre-defined methods, 

(defun xeval (E P N V) ; E = expression, P = program, N = names, V = values
    ; iterate through
    (cond 
        ((null E) nil)
        ((numberp E) E)
        ((atom E) ; do we need this? this prevents lists from being used
            (xassoc E P N V)
        )
    (t ; from above cond
        (do-fun E P N V)
    )) 
)

(defun do-fun (E P N V)
    (let 
        ((name (car E)) ; function name for current value in E
        (e1 (car (cdr E)))
        (e2 (car (cdr (cdr E))))
        (e3 (car (cdr (cdr (cdr E))))))
        (cond
            ((eq name 'if)
                (if (xeval e1 P N V)
                    (xeval e2 P N V)
                    (xeval e3 P N V)
                )
            )
            ((member name '(null atom eq first rest cons equal car cdr number + - * > < = and or not)) ; built in funtions!
                (let ((ev-e1 (xeval e1 P N V)) ; evaluate their arguments
                      (ev-e2 (xeval e2 P N V)))
                (cond 
                    ((eq name 'null)
                        (null ev-e1)
                    )
                    ((eq name 'atom)
                        'goob
                        ;(atom ev-e1)
                    )
                    ((eq name 'eq)
                        (eq ev-e1 ev-e2)
                    )
                    ((eq name 'first)
                        ;(print 'awooga)
                        (car ev-e1)
                    )
                    ((eq name 'rest)
                        (cdr ev-e1)
                    )
                    ((eq name 'cons)
                        (cons ev-e1 ev-e2)
                    )
                    ((eq name 'equal)
                        (equal ev-e1 ev-e2)
                    )
                    ((eq name 'car)
                        (car ev-e1)
                    )
                    ((eq name 'cdr)
                        (cdr ev-e1)
                    )
                    ((eq name 'number)
                        (numberp ev-e1)
                    )
                    ((eq name '+)
                        (+ ev-e1 ev-e2)
                    )
                    ((eq name '-)
                        (- ev-e1 ev-e2)
                    )
                    ((eq name '*)
                        (* ev-e1 ev-e2)
                    )
                    ((eq name '>)
                        (> ev-e1 ev-e2)
                    )
                    ((eq name '<)
                        (< ev-e1 ev-e2)
                    )
                    ((eq name '=)
                        (= ev-e1 ev-e2)
                    )
                    ((eq name 'and)
                        (and ev-e1 ev-e2)
                    )
                    ((eq name 'or)
                        (or ev-e1 ev-e2)
                    )
                    ((eq name 'not)
                        (not ev-e1 ev-e2)
                    )
                ))
            )
            () ; check if element is list, if it is, return it
        (t ; if not in our default list of methods, check against P
            
        ) 
        )
    )
)

(defun islist (list)
    (if (null list)
        t
        (if ())
    )
)

(defun evlist (arg-list P N V)
    (if (null arg-list)
        nil
        (cons (xeval (car arg-list) P N V) (evlist (cdr arg-list) P N V)) ; construct a list of the evaluation of (car arg-list), with the rest of arg-list evaluated
    )
)

(defun xassoc (e P N V)
    (if (null N)
        nil
        (if (member e (car N))
            (locate e (car N) (car V)) ; locate e in N
            (xassoc e (cdr N) (cdr V)) ; iterate over 
        )
    )
)

(defun locate-function (name P)
    (if (eq name (car (car P)))
        (car (cdr (cdr P))) ; return body, drop name and =
        (locate name (cdr value))
    )
)