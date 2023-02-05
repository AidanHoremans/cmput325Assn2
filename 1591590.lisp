(defun xassoc (e N V)
    (if (null N)
        nil
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

(defun closure (arg-list body n v) ; f (X Y) = (+ X Y) -> f (1 2) | (X Y) = arg-list, 
    (cons (cons arg-list body) (cons n v))
)

(defun closure-parameters (c)
    (car (car c))
)

(defun closure-body (c)
    (cdr (car c))
)

(defun closure-values (c)
    (cdr (cdr c))
)

(defun closure-names (c)
    (car (cdr c))
)


(defun xeval (E P N V) ; E = expression, P = program, N = names, V = values
    ; iterate through
    (cond 
        ((null E) nil)
        ((numberp E) E)
        ((or (atom E)) ; when given a bound variable (like X), this fetches and returns the value from V
            (xassoc E N V)
        )
    (t ; from above cond
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
                ))
            )
        (t ; check against P if name is a function that has been user defined
            (let ((body (locate-function name P))
            (param-list (locate-parameters name P))
            )
                (if body ; the user defined it, evaluate function call
                    (let ((evaluated-list (evlist (cdr E) P N V))
                          (clos (closure param-list body N V))) ; do we need this?
                        (let ((new-names (cons (closure-parameters clos) (closure-names clos)))
                              (new-values (cons evaluated-list (closure-values clos))))
                            (xeval body P new-names new-values) ; evaluate the body of the function
                        )
                    )
                    ;E
                    ;(xeval (car E) P N V)
                    (evlist E P N V) ; otherwise, return as list since not a primitive and not a known function call
                )
            )
        )
        )
    )
    )
    ) 
)

(defun evlist (arg-list P N V)
    (if (null arg-list)
        nil
        (cons (xeval (car arg-list) P N V) (evlist (cdr arg-list) P N V)) ; construct a list of the evaluation of (car arg-list), with the rest of arg-list evaluated
    )
)

(defun fl-interp (E P)
    (xeval E P nil nil)
)