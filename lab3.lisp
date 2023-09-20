#|
lab3:
- Author: KirilliriK
- Date: 2023-09-20
|#

(defun get_list_element (list n)
    (cond
        ((null list) ())
        ((= n 0) (car list))
        (T
            (get_list_element (cdr list) (- n 1))
        )
    )
)

(defun replace_list_element (list n elem)
    (cond
        ((null list) ())
        ((= n 0) (cons elem (cdr list)))
        (T
            (cons (car list) (replace_list_element (cdr list) (- n 1) elem))
        )
    )
)

#| Задача 1 |#
(defun calc(s)
    (if (= (mod s 2) 0)
        (
            + (- (* 9 (expt 2 s)) (* 9 (expt 2 (floor s 2)))) 1
        )
        (
            + (- (* 8 (expt 2 s)) (* 6 (expt 2 (floor (+ s 1) 2)))) 1
        )
    )
)

(defun sedjS (s size last)
    (cond
        ((> (* 3 last) size)
            if (> s 0)
                (- s 1)
                (0)
        )
        (T
            (sedj (+ s 1) size (calc s))
        )
    )
)

(defun sedj (s size last)
    (cond
        ((> (* 3 last) size) '())
        (T
            (append
                (list (calc s))
                (sedj (+ s 1) size (calc s))
            )
        )
    )
)

(defun for_last (lst j tmp inc)
    (cond
        ((and (< j 0) (<= (get_list_element list j) tmp)) (replace_list_element lst (+ j inc) tmp))
        (T
            (for_last (replace_list_element lst (+ j inc) (get_list_element list j)) (- j inc) tmp inc)
        )
    )
)

(defun for_first (lst size i inc)
    (cond
        ((>= i size) lst)
        (T
            (for_first (for_last lst (- i inc) (get_list_element lst i) inc) size (+ i 1))
        )
    )
)

(defun shell (lst size s slst)
    (cond
        ((< s 0) lst)
        (T
            #| (shell (for_first lst size (get_list_element slst s) (get_list_element slst s)) size (- s 1) slst) |#
        )
    )
)

(terpri)
(write-string "Result ")
(write (sedj 0 16 0))
(terpri)
(write (shell '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9) 16 (sedjS 0 16 0) (sedj 0 16 0)))
(terpri)