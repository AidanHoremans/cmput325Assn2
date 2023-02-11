(defun fl-interp (E P)
    (xeval E P nil nil)
)

(defun xeval (E P N V) ; E = expression, P = program, N = names, V = values
    ; iterate through
    (cond 
        ((null E) nil)
        ((numberp E) E)
        ((atom E) ; when given a bound variable (like X), this fetches and returns the value from V
            (xassoc E N V)
        )
    (t ; from above cond
        (let ((name (car E)) ; function name for current value in E
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
            ((member name '(null atom eq first rest cons equal car cdr number + - * > < = and or not)) ; built in functions!
                (let ((ev-e1 (xeval e1 P N V)) ; evaluate their arguments first
                      (ev-e2 (xeval e2 P N V)))
                    (run-atomic name ev-e1 ev-e2)
                )
            )
            (t ; check against P if name is a function that has been user defined
                (run-function name E P N V)
            ))
        )
    )) 
)

(defun run-atomic (name ev-e1 ev-e2)
    (cond
        ((eq name 'null)
            (null ev-e1)
        )
        ((eq name 'atom)
            (atom ev-e1)
        )
        ((eq name 'eq)
            (eq ev-e1 ev-e2)
        )
        ((eq name 'first)
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
            (not ev-e1)
        )
    )
)

(defun run-function (name E P N V)
    (let ((body (locate-function name P))
            (param-list (locate-parameters name P))
    )
        (if body ; the user defined it, evaluate function call
            (let ((evaluated-list (evlist (cdr E) P N V)) ; evaluate the list of values for the function call
            )  
                (let ((new-names (cons param-list N)) ; construct a context of names consisting of the function's parameter list and the names of all other values
                        (new-values (cons evaluated-list V)) ;construct a context of new-values, consisting of the evaluted list of values in front of all other values V
                )
                    (xeval body P new-names new-values)
                )
            )
            (evlist E P N V) ; otherwise, return as evaluated list since not a primitive and not a known function call -> different from above evaluated-list
        )
    )
)

(defun evlist (arg-list P N V)
    (if (null arg-list)
        nil
        (cons (xeval (car arg-list) P N V) (evlist (cdr arg-list) P N V)) ; construct a list of the evaluation of (car arg-list), with the rest of arg-list evaluated
    )
)

(defun xassoc (e N V)
    (if (null N)
        e ; if not in context, just return the value as though it was quoted
        (if (member e (car N))
            (locate e (car N) (car V)) ; locate e in N
            (xassoc e (cdr N) (cdr V)) ; iterate over 
        )
    )
)

(defun locate (var name-sublist value-sublist)
    (if (eq var (car name-sublist))
        (car value-sublist)
    ; else
        (locate var (cdr name-sublist) (cdr value-sublist))
    )
)

(defun locate-function (name P)
    (if (null P)
        nil
        (if (eq name (car (car P))) ; compare name against current name in P
            (car (cdr (cdr (cdr (car P))))) ; return body, drop name and =
            (locate-function name (cdr P))
        )
    )
)

(defun locate-parameters (name P)
    (if (null P)
        nil
        (if (eq name (car (car P)))
            (car (cdr (car P))) ; return body, drop name and =
            (locate-parameters name (cdr P))
        )
    )
)

; for numeric comparison methods, check if vals are numeric, if not then deal with them appropriately
(defun numeric-atomic (F e1 e2)
    (cond
        (not (numberp e1) )
        ()
        (t ())
    )
)
