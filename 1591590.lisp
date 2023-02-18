"
GENERAL PROGRAM NOTES:
Due to the cyclical nature of some function calls (i.e. fl-interp calls evaluate-simple-atomic which calls fl-interp), 
I was not able to reduce the number of style warnings to 0. This ordering of functions should cut down on it though.

The 3 primary functions for function handling are located at the bottom. 
    fl-interp, evaluate-if-and-or, evaluate-simple-atomic
"

"
Returns true if x is a member of L, false otherwise.
"
(defun xmember (x L)
    (if (null L)
        nil
        (if (equal x (car L))
            t
            (xmember x (cdr L))
        )
    )
)

"
Returns the Length of L. For use with 
"
(defun xlength (L) ; returns length of list L
    (if (null L)
        0
        (+ 1 (xlength (cdr L)))
    )
)

"
Returns true if L1 and L2 have the same length -> used to allow overloading of function names with different parameter lengths.
i.e. we use this while checking against P to ensure the parameter list of E matches that in P.
"
(defun equal-length (L1 L2) ; checks if the lengths of 2 lists are equal
    (= (xlength L1) (xlength L2))
)

"
Looks through P and returns a corresponding list ((body) (parameters)) if the function function-name is found
"
(defun locate-function (function-name E P) ; checks for parity of parameters, returns body AND parameters in side by side list 
    (cond
        ((null P) nil)
        ((and 
            (eq function-name (car (car P))) 
            (equal-length (car (cdr (car P))) (cdr E))) ; parameter length == length of variables list
            (cons (car (cdr (cdr (cdr (car P))))) (cons (car (cdr (car P))) nil))  ; constructs and returns a list of ((body) (parameters))
        )
        (t (locate-function function-name E (cdr P))) ; if function-name != function name in P OR parameter length != length of variables list, keep looking through P
    )
)

"
For a given variable, go through the whole body and replace any instances of variable with replacement. Called by replace-body.

If body is null
    return nil
else if body IS NOT an atom
    return the call of replace-variable on the first element of body cons'd to replace-variable for the rest of the body
else if body IS AN atom
    if body == variable
        return replacement to be cons'd onto the new body
    else if body IS AN atom and body != variable
        return body (which is a single atom of a list) to be cons'd onto the new body
"
(defun replace-variable (body parameter replacement) ;body to replace, variable which gets overridden by replacement
    (cond
        ((null body) nil) ; end of body
        ((not (atom body)) ; if the current value of body is not an atom, iterate through the first element attached to all other elements in body
            (cons (replace-variable (car body) parameter replacement) (replace-variable (cdr body) parameter replacement))
        )
        ((equal body parameter) ; if current value IS an atom, and is equal to the variable we are replacing, return the repalcement value instead
            replacement
        )
        (t ; otherwise if the current value IS an atom, but is not equal to the variable we are replacing, return it
            body 
        )
    )
)

"
Iterates through the current parameters, and calls replace-variable for every parameter for body. This call to replace-variable should replace
all instances of (car parameters) with (car values). One the parameters list is empty, return the new body.
If the parameters list is null, just return the body as is, since there's no variables to replace.
"
(defun replace-body (body parameters values) ; replace variables-names in body with values
    (if (null parameters)
        body
        (replace-body (replace-variable body (car parameters) (car values)) (cdr parameters) (cdr values))
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
Look for function-name in the user defined functions P by searching for the body of the function
If the function is found in P:
    Evaluate the parameter list (cdr E) and call fl-interp on the body of the user defined function with the parameter list added to the context
Else if it's not found:
    treat E like a list, and return the evaluated list E
"
(defun run-function (function-name E P)
    (let ((body-and-parameters (locate-function function-name E P)) ; for body-and-parameters body is car, parameters are (cdr (car))
    )
        (if body-and-parameters ; the user defined it, evaluate function call
            (let ((evaluated-list (evlist (cdr E) P)) ; evaluate the list of values for the function call -> i.e. perform AOR
            )
                (fl-interp (replace-body (car body-and-parameters) (car (cdr body-and-parameters)) evaluated-list) P)
            )
            (evlist E P) ; otherwise, return as a list with every value evaluated since not a primitive and not a known function call -> different from above evaluated-list
        )
    )
)

"
By definition for our interpreter, anything other than nil is considered true. This function allows for easy short-circuiting of AND and OR
by returning t for anything not null.
"
(defun is-true (e)
    (if (null e)
        nil
        t
    )
)

"
MAIN LOOP:
Returns nil for null expressions
Returns E if E is a number
If E is an atom, 
    we iterate through the current context and return the associated value -> i.e. we assume atoms are variable names
If the first element of E is an atomic function, 
    we call evaluate-simple-atomic
Otherwise, 
    we call run-function to determine if the fl-interp call is on a user defined function or is just a list
"
(defun fl-interp (E P) ; E = expression, P = program
    (cond 
        ((null E) nil)
        ((atom E) E)
    (t ; handle functions, known and defined
        (let ((function-name (car E))) ; function function-name for current value in E
        (cond
            ((xmember function-name '(if and or)) ; special case atomics, using short circuiting
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
IF, AND, OR are handled differently from other atomics. we utilize short-circuiting to only evaluate specific parts of the function.
Called by fl-interp.
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
Compare function-name against all known atomic functions and run the associated one.
Called by fl-interp.
"
(defun evaluate-simple-atomic (function-name E P)
    (let ((e1 (car (cdr E)))
          (e2 (car (cdr (cdr E)))))
    (let ((ev-e1 (fl-interp e1 P))
          (ev-e2 (fl-interp e2 P))) ; evaluate e1 and e2 before we apply them to the following atomics -> AOR
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