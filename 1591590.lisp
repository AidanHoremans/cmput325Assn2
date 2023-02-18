"
Main loop:
Returns nil for null expressions
Returns E if E is a number
If E is an atom, we iterate through the current context and return the associated value -> i.e. we assume atoms are variable names
If the first element of E is an atomic function, we call evaluate-simple-atomic
Otherwise, we call run-function to determine if the fl-interp call is on a user defined function or is just a list
"
(defun fl-interp (E P) ; E = expression, P = program
    ; iterate through
    (cond 
        ((null E) nil)
        ((numberp E) E)
        ((atom E) ; when given a bound variable (like X), this fetches and returns the value from V
            E
        )
    (t ; from above cond
        (let ((function-name (car E))) ; function function-name for current value in E
        (cond
            ((xmember function-name '(if and or)) ; special case atomics
                (evaluate-if-and-or function-name E P)
            )
            ((xmember function-name '(null atom eq first rest cons equal car cdr number + - * > < = not)) ; simple atomics
                (evaluate-simple-atomic function-name E P)
            )
            (t ; check against P if function-name is a function that has been user defined
                (run-function function-name E P)
            ))
        )
    ))
)

"
Compare function-name against all known atomic functions and run the associated one
"
(defun evaluate-simple-atomic (function-name E P)
    (let ((e1 (car (cdr E)))
          (e2 (car (cdr (cdr E)))))
    (let ((ev-e1 (fl-interp e1 P))
          (ev-e2 (fl-interp e2 P))) ; evaluate e1 and e2 before we apply them to the following atomics
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
        ((or (eq function-name 'equal) (eq function-name '=)) ; allows us to 
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
        ((eq function-name 'not)
            (not (is-true ev-e1))
        )
    )))
)

"
if, and, or are handled differently from other atomics. 
"
(defun evaluate-if-and-or (function-name E P)
    (let ((e1 (car (cdr E))) ; get all elements out of E, even if not all functions use them
          (e2 (car (cdr (cdr E))))
          (e3 (car (cdr (cdr (cdr E))))))
    (cond
        ((eq function-name 'if)
            (if (fl-interp e1 P) ; if ev-e1
                (fl-interp e2 P) ; then ev-e2
                (fl-interp e3 P) ; else ev-e3
            )
        )
        ((eq function-name 'and)
            (cond 
                ((not (is-true (fl-interp e1 P))) nil) ; evaluates e1, then returns false if it is false
                ((not (is-true (fl-interp e2 P))) nil) ; evaluates e2, then returns false if it is false
                (t t) ; otherwise if neither e1 or e2 evaluate to false, return true
            )
        )
        ((eq function-name 'or)
            (cond
                ((is-true (fl-interp e1 P)) t) ; evaluates e1, then returns true if it is true
                ((is-true (fl-interp e2 P)) t) ; evaluates e2, then returns true if it is true
                (t nil) ; otherwise if neither is true, return false
            )
        )
    ))
)

"
By definition for our interpreter, anything other than nil is considered true.
"
(defun is-true (e)
    (if (null e)
        nil
        t
    )
)

"
This function is used for equality booleans such as < and >, so that we return nil if either element is not a number
"
(defun numeric-equality-booleans (F ev-e1 ev-e2)
    (if (or (not (numberp ev-e1)) (not (numberp ev-e2)))
        nil
        (funcall F ev-e1 ev-e2);(cons ev-e1 (cons ev-e2 nil)))
    )
)

"
Look for function-name in the user defined functions P by searching for the body of the function
If it's found:
    Evaluate the parameter list (cdr E) and call fl-interp on the body of the user defined function with the parameter list added to the context
Else if it's not found:
    treat E like a list, and return the evaluated list E
"
(defun run-function (function-name E P)
    (let ((body-and-parameters (locate-function function-name E P)) ; body is car, parameters are (cdr (car))
    )
        (if body-and-parameters ; the user defined it, evaluate function call
            (let ((evaluated-list (evlist (cdr E) P)) ; evaluate the list of values for the function call
            )
                (fl-interp (replace-body (car body-and-parameters) (car (cdr body-and-parameters)) evaluated-list) P)
            )
            (evlist E P) ; otherwise, return as evaluated list since not a primitive and not a known function call -> different from above evaluated-list
        )
    )
)

"
replaces all occurences of parameters in body
"
(defun replace-body (old-body parameters values) ; replace variables-names in body with values
    ;go through every item in parameters and replace all instances in body with the first thing in values
    (if (null parameters)
        old-body
        (replace-body (replace-variable old-body (car parameters) (car values)) (cdr parameters) (cdr values))
    )
)


(defun replace-variable (body variable replacement) ;body to replace, variable and replacement
    (if (null body)
        nil
        (if (atom body)
            (if (equal body variable) ;if first item in body is equal to the required variable, 
                replacement
                body
            )
            (cons (replace-variable (car body) variable replacement) (replace-variable (cdr body) variable replacement))
        )
    )
)

"
Iterates through all values in the given arg-list and calls fl-interp for each, then returns each evaluated argument as a list
"
(defun evlist (arg-list P)
    (if (null arg-list)
        nil
        (cons (fl-interp (car arg-list) P) (evlist (cdr arg-list) P)) ; construct a list of the evaluation of (car arg-list), with the rest of arg-list evaluated
    )
)

"
does the same thing as member
"
(defun xmember (x member-list)
    (if (null member-list)
        nil
        (if (equal x (car member-list))
            t
            (xmember x (cdr member-list))
        )
    )
)

(defun locate-function (function-name E P) ; checks for parity of parameters, returns body AND parameters in side by side list 
    (cond
        ((null P) nil)
        ((and (eq function-name (car (car P))) (equal-length (car (cdr (car P))) (cdr E))) ; parameter length == length of variables list
            (cons (car (cdr (cdr (cdr (car P))))) (cons (car (cdr (car P))) nil))  ; body + parameters
        )
        (t (locate-function function-name E (cdr P)))
    )
)

(defun equal-length (L1 L2) ; checks if the lengths of 2 lists are equal
    (= (xlength L1) (xlength L2))
)

(defun xlength (L) ; returns length of list L
    (if (null L)
        0
        (+ 1 (xlength (cdr L)))
    )
)

;; "
;; For a function call in fl-interp, locate the function name from P and return its body
;; "
;; (defun locate-function (function-name P)
;;     (if (null P)
;;         nil
;;         (if (eq function-name (car (car P))) ; compare function-name against current function-name in P
;;             (car (cdr (cdr (cdr (car P))))) ; return body, drop function-name and =
;;             (locate-function function-name (cdr P))
;;         )
;;     )
;; )

;; "
;; Similar to locate-function, we find a funtion in P and this time return the parameter list defined for that function.
;; We use this parameter list to help build our context.
;; "
;; (defun locate-parameters (function-name P)
;;     (if (null P)
;;         nil
;;         (if (eq function-name (car (car P)))
;;             (car (cdr (car P))) ; return body, drop function-name and =
;;             (locate-parameters function-name (cdr P))
;;         )
;;     )
;; )