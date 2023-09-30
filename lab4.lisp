#|
lab4:
- Author: KirilliriK
- Date: 2023-09-24
|#

#| Задача 1 |#
(defun calculate_factorial(n r)
    (
        (lambda (num rr)
            (cond
                    ((= num 0) rr)
                    (T
                        (calculate_factorial (- num 1) (* rr num))
                    )
            )
        )
        n r
    )
)

(terpri)
(write-string "Task one ")
(terpri)
(write-string "Input 5")
(terpri)
(write-string "Result ")
(write (calculate_factorial 5 1))
(terpri)

#| Задача 2 |#
(defun make_exponentiation (base exponent)    #| Problem solved thanks to training exercise |#
    (cond
        ((= exponent 0) 1)
        ((= exponent 1) base)
        (T (list '** base exponent))
    )
)

(defun is_exponentiation (x)
    (and (listp x) (eq (car x) '**))
)

(defun is_number_and_equal (exp num)
    (and (numberp exp) (= exp num))
)

(defun is_variable (x)
    (symbolp x)
)

(defun variable_is_same (v1 v2)
    (and
        (is_variable v1)
        (is_variable v2)
        (eq v1 v2)
    )
)

(defun make_product (m1 m2)
    (cond
            ((or (is_number_and_equal m1 0) (is_number_and_equal m2 0)) 0)
            ((is_number_and_equal m1 1) m2)
            ((is_number_and_equal m2 1) m1)
            ((and (numberp m1) (numberp m2)) (* m1 m2))
            (T (list '* m1 m2))
    )
)

(defun is_product (x)
    (and (listp x) (eq (car x) '*))
)

(defun make_sum (a1 a2)
    (cond
        ((is_number_and_equal a1 0) a2)
        ((is_number_and_equal a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (T (list '+ a1 a2))
    )
)

(defun is_sum (x)
    (and (listp x) (eq (car x) '+))
)

(defun deriv (ex va)
    (
      (lambda (exp var)
        (cond
            ((numberp exp) 0)
            ((is_variable exp) (if (variable_is_same exp var) 1 0))
            ((is_sum exp) (make_sum (deriv (cadr exp) var) (deriv (caddr exp) var)))
            ((is_product exp)
                (make_sum
                   (make_product (cadr exp) (deriv (caddr exp) var))
                   (make_product (deriv (cadr exp) var) (caddr exp))
                )
            )
            ((is_exponentiation exp)
                 (make_product
                      (make_product (caddr exp)
                             (make_exponentiation (cadr exp) (- (caddr exp) 1))
                      )
                      (deriv  (cadr exp) var)
                 )
            )
            (T (error "Неизвестный тип" exp))
        )
        )
        ex va
    )
)

(terpri)
(write-string "Task two ")
(terpri)
(write-string "Input x^3 or (** x 3)")
(terpri)
(write-string "Result ")
(write (deriv '(** x 3) 'x))
(terpri)

#| Задача 3 |#
(defun rev (l)
    (cond
        ((null l) '())
        (T
            (
                (lambda (lst_in)
                    (append
                        (rev (cdr lst_in))
                        (list
                            (if (atom (car lst_in))
                                    (car lst_in)
                                    (rev (car lst_in))
                            )
                        )
                    )
                )
                l
            )
        )
    )
)

(defun algh (lst)
    (let ((lst
            (cond
                ((null lst) '())
                (T
                    (append
                        (list
                            (if (atom (car lst))
                                (car lst)
                                (rev (car lst))
                            )
                        )
                        (algh (cdr lst))
                    )
                )
            )
        ))
        lst
    )
)

(terpri)
(write-string "Task three ")
(terpri)
(write-string "Input (1 ((2 3) 4) 5 6)")
(terpri)
(write-string "Result ")
(write (algh '(1 ((2 3) 4) 5 6)))
(terpri)

#| Задача 4 |#
(defun interpret (expr)
    (cond
        ((not (symbolp (car expr))) expr)
        ((not (fboundp (car expr))) expr)
        (T
            (
                (lambda (m n)
                    (apply m n)
                )
                (car expr)
                (arg_proc (cdr expr))
            )
        )
    )
)

(defun arg_proc (lst)
    (cond
        ((null lst) nil)
        ((atom (car lst)) (cons (car lst) (arg_proc (cdr lst))))
        ((equal (symbol-name (caar lst)) "QUOTE") (cdar lst))
        (T
            (cons
                (interpret (car lst))
                (arg_proc (cdr lst))
            )
        )
    )
)

(terpri)
(write-string "Task four ")
(terpri)
(write-string "Input (cons (car (cdr '(e r t w))) (cons (cdr '(g h 6)) '()))")
(terpri)
(write-string "Result ")
(write (interpret '(cons (car (cdr '(e r t w))) (cons (cdr '(g h 6)) '()))))
(terpri)

#| Задача 5 |#
(defun append_list (lst1 lst2) #| Функция для объединения списков |#
    (cond
        ((null lst2) lst1)
        (T
            (append_list (append lst1 (list (car lst2))) (cdr lst2))
        )
    )
)

(terpri)
(write-string "Task five ")
(terpri)
(write-string "Input (append_list (list e r t w) (list g h 6))")
(terpri)
(write-string "Result ")
(write (interpret '(append_list (list e r t w) (list g h 6))))
(terpri)