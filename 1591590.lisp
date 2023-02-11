
"
Entry point for fl-interp
"
(defun fl-interp (E P)
    (xeval E P nil nil)
)

"
Main loop:
Returns nil for null expressions
Returns E if E is a number
If E is an atom, we iterate through the current context and return the associated value -> i.e. we assume atoms are variable names
If the first element of E is an atomic function, we call run-atomic
Otherwise, we call run-function to determine if the xeval call is on a user defined function or is just a list
"
(defun xeval (E P N V) ; E = expression, P = program, N = names, V = values
    ; iterate through
    (cond 
        ((null E) nil)
        ((numberp E) E)
        ((atom E) ; when given a bound variable (like X), this fetches and returns the value from V
            (xassoc E N V)
        )
    (t ; from above cond
        (let ((function-name (car E)) ; function function-name for current value in E
              (e1 (car (cdr E)))
              (e2 (car (cdr (cdr E))))
              (e3 (car (cdr (cdr (cdr E))))))
        (cond
            ((eq function-name 'if)
                (if (xeval e1 P N V)
                    (xeval e2 P N V)
                    (xeval e3 P N V)
                )
            )
            ((member function-name '(null atom eq first rest cons equal car cdr number + - * > < = and or not)) ; built in functions!
                (let ((ev-e1 (xeval e1 P N V)) ; evaluate their arguments first
                      (ev-e2 (xeval e2 P N V)))
                    (run-atomic function-name ev-e1 ev-e2)
                )
            )
            (t ; check against P if function-name is a function that has been user defined
                (run-function function-name E P N V)
            ))
        )
    )) 
)

"
Compare function-name against all known atomic functions and run the associated one
"
(defun run-atomic (function-name ev-e1 ev-e2)
    (cond
        ((eq function-name 'null)
            (null ev-e1)
        )
        ((eq function-name 'atom)
            (atom ev-e1)
        )
        ((eq function-name 'eq)
            (eq ev-e1 ev-e2)
        )
        ((eq function-name 'first)
            (car ev-e1)
        )
        ((eq function-name 'rest)
            (cdr ev-e1)
        )
        ((eq function-name 'cons)
            (cons ev-e1 ev-e2)
        )
        ((eq function-name 'equal)
            (equal ev-e1 ev-e2)
        )
        ((eq function-name 'car)
            (car ev-e1)
        )
        ((eq function-name 'cdr)
            (cdr ev-e1)
        )
        ((eq function-name 'number)
            (numberp ev-e1)
        )
        ((eq function-name '+)
            (+ ev-e1 ev-e2)
        )
        ((eq function-name '-)
            (- ev-e1 ev-e2)
        )
        ((eq function-name '*)
            (* ev-e1 ev-e2)
        )
        ((eq function-name '>)
            (> ev-e1 ev-e2)
        )
        ((eq function-name '<)
            (< ev-e1 ev-e2)
        )
        ((eq function-name '=)
            (= ev-e1 ev-e2)
        )
        ((eq function-name 'and)
            (and ev-e1 ev-e2)
        )
        ((eq function-name 'or)
            (or ev-e1 ev-e2)
        )
        ((eq function-name 'not)
            (not ev-e1)
        )
    )
)

"
Look for function-name in the user defined functions P by searching for the body of the function
If it's found:
    Evaluate the parameter list (cdr E) and call xeval on the body of the user defined function with the parameter list added to the context
Else if it's not found:
    treat E like a list, and return the evaluated list E
"
(defun run-function (function-name E P N V)
    (let ((body (locate-function function-name P))
          (param-list (locate-parameters function-name P))
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

"
Iterates through all values in the given arg-list and calls xeval for each, then returns each evaluated argument as a list
"
(defun evlist (arg-list P N V)
    (if (null arg-list)
        nil
        (cons (xeval (car arg-list) P N V) (evlist (cdr arg-list) P N V)) ; construct a list of the evaluation of (car arg-list), with the rest of arg-list evaluated
    )
)

"
Searches through the context N and V for a desired element e
If it's found in N
    return the associated value from V
Else if it's not in N
    just return e as though it was quoted -> this allows us to treat letters as values in lists
"
(defun xassoc (e N V)
    (if (null N)
        e ; if not in context, just return the value as though it was quoted
        (if (member e (car N))
            (locate e (car N) (car V)) ; locate e in N
            (xassoc e (cdr N) (cdr V)) ; iterate over 
        )
    )
)

(defun locate (var function-name-sublist value-sublist)
    (if (eq var (car function-name-sublist))
        (car value-sublist)
    ; else
        (locate var (cdr function-name-sublist) (cdr value-sublist))
    )
)

(defun locate-function (function-name P)
    (if (null P)
        nil
        (if (eq function-name (car (car P))) ; compare function-name against current function-name in P
            (car (cdr (cdr (cdr (car P))))) ; return body, drop function-name and =
            (locate-function function-name (cdr P))
        )
    )
)

(defun locate-parameters (function-name P)
    (if (null P)
        nil
        (if (eq function-name (car (car P)))
            (car (cdr (car P))) ; return body, drop function-name and =
            (locate-parameters function-name (cdr P))
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
