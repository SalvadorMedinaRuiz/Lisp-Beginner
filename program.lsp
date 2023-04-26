;COSC 341 Salvador Medina-Ruiz

;Function f1 that decides whether a list has an atom inside
(defun f1 (l) ;l = list
    (cond ((atom l) nil) ;If list is JUST an atom, return false
            ((null l) nil) ;If list is empty or null, return false
            ((atom (car l)) T) ;If car of list is an atom, return true
            (T (f1 (cdr l))) ;Else, recursive call on cdr of list 
    )
)

;Function f2 that counts the number of list of length 1 in a list
(defun f2 (l) ;l = list
    (cond ((null l) 0) ;If list is empty or null, return 0
            ((atom l) 0) ;If list is JUST an atom, return 0
            ((and (listp (car l)) (equal (length (car l)) 1)) (+ 1 (f2 (cdr l)))) ;If the length of the car of list equals 1 AND it's a list, add 1 to recursive call of list 
            (T (f2 (cdr l))) ;Else, recursive call on cdr of list
    )
)

;Function f3 that takes a list of integers and returns a list containing only odd integers
(defun f3 (l) ;l = integer list
    (cond ((null l) l) ;If list is empty or null, return the list
            ((atom l) nil) ;If list is JUST an atom, return nil
            ((oddp (car l)) (cons (car l) (f3 (cdr l)))) ;If the car of list is odd, then cons the car of l and the recursive call of cdr of list
            (T (f3 (cdr l))) ;Else, recursive call on cdr of list
    )
)

;Function f4 that returns the minimum value of an integer list
(defun f4 (l) ;l = integer list
    (cond ((null l) l) ;If list is empty or null, return list
            ((atom l) nil) ;If list is JUST an atom, return nil
            ((null (cdr l)) (car l)) ;If there is only one element in the list, then return that element
            ((< (car l) (car (cdr l))) (f4 (cons (car l) (cdr (cdr l))))) ;If the second element of the list (car (cdr l)) is bigger than the first element (car l), then cons the first element and the rest of the list (which ends up being an empty list)
            (T (f4 (cdr l))) ;Else, recursive call on cdr of list
    )
)

;Function f5 that returns the reverse of a list
(defun f5 (l) ;l = list
    (cond ((null l) l) ;If list is empty or null, return list
            ((atom l) nil) ;If list is JUST an atom, return nil
            (T (append (f5 (cdr l)) (list (car l)))) ;Else, reverse cdr of list and append first element of list at the end
    )
)

;function f6 that returns the list containing every other element of a list.
(defun f6 (l) ;l = list
    (cond ((null l) l) ;If list is empty or null, return list
            ((atom l) nil) ;If list is JUST an atom, return nil
            (T(append (list (car l)) (f6 (cdr (cdr l))))) ;Else, appends the first element of l (car l) to the (cdr (cdr l)) which is every other element
    )
)

;Function f7 that returns the element at a given location of a list.
(defun f7 (l p) ;l = list, p = position
    (cond ((null l) nil) ;If list is empty or null, then invalid list was given.
            ((or (atom l) (not (numberp p))) nil) ;If list is JUST an atom, or p is NOT a number, return nil
            ((equal p 1) (car l)) ;If position is 1, then return beginning element of list.
            (T (f7 (cdr l) (- p 1))) ;Else, pass the rest of the list (cdr l), and update position by subtracting 1
    )
)

;Function f8 that returns the sum of all integers everywhere in a list
(defun f8 (l) ;l = list of integers
    (cond ((null l) 0) ;If list is empty or null, then return sum of 0
            ((atom l) 0) ;If list is JUST an atom, return 0
            ((listp (car l)) (+ (f8 (car l)) (f8 (cdr l)))) ;Checks if the first element of list is a list (EX ((2 5 8) 3 4)), then adds two recurssions of the car AND cdr of list within the list
            (T (+ (car l) (f8 (cdr l)))) ;Else, adds first element of list and passes the rest of the list to a recurssion call
    )
)

;Function f9 that removes duplicates from a list using provided member function
;Provided member function from notes 10
(defun my_member (x L)
   (cond ((null L) nil)             ;if L is empty then x is not in L
         ((equal x (car L)) T)      ;if x is first element of L then x is in L
         (T (my_member x (cdr L)))  ;else check x is in cdr of L
   )
)

;F9
(defun f9 (l) ;l = list
    (cond ((null l) nil) ;If list is empty, then return nil.
        ((atom l) nil) ;If list is JUST an atom, return nil
        ((my_member (car l) (cdr l)) (f9 (cdr l))) ;First checks if first member of list is in the rest of the list. If yes, then recurssive call f9 with cdr of list
        (T (append (list (car l)) (f9 (cdr l)))) ;Create a list with first element of list then recurssive call f9 with cdr of list
    )
)

;Function f10 that finds the intersection of two lists. Uses provided member function from above.
(defun f10 (x l) ;x, l = list
    (cond ((null x) nil) ;If list x is empty or null, then return nil.
        ((or (atom x) (atom l)) nil) ;If list x or l is JUST an atom, return nil
        ((my_member (car x) l) (append (list (car x)) (f10 (cdr x) l))) ;If first element of list x is in list l, then append first element of list x to recurssive call of cdr of list x and list l
        (T (f10 (cdr x) l)) ;Else, recursive call of the rest of list x and the entire list l
    )
)