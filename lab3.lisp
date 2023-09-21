#|
lab3:
- Author: KirilliriK
- Date: 2023-09-20
|#

(defun get_list_element (lst n) #| Функция для получения элемента из списка по индексу |#
    (cond
        ((= n 0) (car lst))
        (T
            (get_list_element (cdr lst) (- n 1))
        )
    )
)

(defun replace_list_element (lst n elem)  #| Функция для замены элемента из списка по индексу |#
    (cond
        ((= n 0) (cons elem (cdr lst)))
        (T
            (cons (car lst) (replace_list_element (cdr lst) (- n 1) elem))
        )
    )
)

(defun remove_list_element (lst n)  #| Функция для удаления элемента из списка по индексу |#
    (cond
        ((null lst) '())
        ((= n 0) (cdr lst))
        (T
            (cons (car lst) (remove_list_element (cdr lst) (- n 1)))
        )
    )
)

#| Задача 1 |#
(defun calc(s)                #| Функция для вычисления шага сортировки |#
    (if (= (mod s 2) 0)
        (
            + (- (* 9 (expt 2 s)) (* 9 (expt 2 (floor s 2)))) 1
        )
        (
            + (- (* 8 (expt 2 s)) (* 6 (expt 2 (floor (+ s 1) 2)))) 1
        )
    )
)

(defun sedjS (s size last) #| Функция для вычисления коэффициента S|#
    (cond
        ((>= (* 3 last) size) (if (> s 0) (- s 2) 0 ) )
        (T
            (sedjS (+ s 1) size (calc s))
        )
    )
)

(defun sedj (s size last) #| Функция для вычисления шагов сортировки |#
    (cond
        ((>= (* 3 last) size) '())
        (T
            (append (list (calc s)) (sedj (+ s 1) size (calc s))
            )
        )
    )
)

(defun for_last (lst j tmp inc)  #| Функция для сортировки вставками |#
    (cond
        ((or (< j 0) (<= (get_list_element lst j) tmp)) (replace_list_element lst (+ j inc) tmp))
        (T
            (for_last (replace_list_element lst (+ j inc) (get_list_element lst j)) (- j inc) tmp inc)
        )
    )
)

(defun for_first (lst size i inc) #| Функция перебора с инкрементами |#
    (cond
        ((>= i size) lst)
        (T
            (for_first (for_last lst (- i inc) (get_list_element lst i) inc) size (+ i 1) inc)
        )
    )
)

(defun shell (lst size s slst) #| Функция которая инициирует сортировку алгоритмом Шелла |#
    (cond
        ((< s 0) lst)
        (T
            (shell (for_first lst size (get_list_element slst s) (get_list_element slst s)) size (- s 1) slst)
        )
    )
)

(terpri)
(write-string "Task one ")
(terpri)
(write-string "Input ")
(write '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9))
(terpri)
(write-string "Result ")
(write (shell '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9) 16 (sedjS 0 16 0) (sedj 0 16 0)))
(terpri)

#| Задача 2 |#
(defun qsort_left (lst coeff) #| Функция возвращает список с элементами которые меньше коэффициента |#
    (cond
        ((null lst) '())
        (T
            (if (>= (car lst) coeff)
                (qsort_left (cdr lst) coeff)
                (append
                    (list (car lst))
                    (qsort_left (cdr lst) coeff)
                )
            )
        )
    )
)

(defun qsort_right (lst coeff) #| Функция возвращает список с элементами которые больше либо равны коэффициенту |#
    (cond
        ((null lst) '())
        (T
            (if (< (car lst) coeff)
                (qsort_right (cdr lst) coeff)
                (append
                    (list (car lst))
                    (qsort_right (cdr lst) coeff)
                )
            )
        )
    )
)

(defun qsort (lst) #| Функция рекурсивной сортировки методом Хоара, за коэффициент изначально берётся голова списка|#
    (cond
        ((null lst) '())
        (T
            (append
                (qsort (qsort_left (cdr lst) (car lst)))
                (list (car lst))
                (qsort (qsort_right (cdr lst) (car lst)))
            )
        )
    )
)

(terpri)
(write-string "Task two ")
(terpri)
(write-string "Input ")
(write '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9))
(terpri)
(write-string "Result ")
(write (qsort '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9)))
(terpri)

#| Задача 3 |#

(defun merge_lists (frt sec) #| Функция рекурсивного объединения двух отсортированных списков |#
    (cond
        ((null frt) sec)
        ((null sec) frt)
        (
            (< (car frt) (car sec))
            (cons (car frt) (merge_lists (cdr frt) sec))
        )
        (T
            (cons (car sec) (merge_lists frt (cdr sec)))
        )
    )
)

(terpri)
(write-string "Task three ")
(terpri)
(write-string "Input ")
(write-string "(-5 0 1 3 5 7 19) (-11 0 1 2 3 4 6 8)")
(terpri)
(write-string "Result ")
(write (merge_lists '(-5 0 1 3 5 7 19) '(-11 0 1 2 3 4 6 8)))
(terpri)

#| Задача 4 |#

(defun permutate (lst head lst_d n)
    (cond
        ((null lst) '())
        ((null head) '())
        ((null lst_d) '())
        (T
            (append
                (if (atom head)
                    (list head)
                    head
                )
                (permutate lst_d (car lst_d) (cdr lst_d) 1)
                (permutate lst (get_list_element lst n) (remove_list_element lst n) (+ n 1))
            )
        )
    )
)

(defun permutate_test (lst main_head head lst_d n)
    (cond
        ((null lst) '())
        ((null head) '())
        ((null lst_d) '())
        (T
            (append
                (if (atom head)
                    (list head)
                    head
                )
                (permutate lst_d (car lst_d) (cdr lst_d) 1)
                (permutate lst (get_list_element lst n) (remove_list_element lst n) (+ n 1))
            )
        )
    )
)

(defun permutate_first (result lst head lst_d n)
    (cond
        ((null lst) result)
        ((null head) result)
        (T
            (permutate_first
                (append
                    result
                    (list (permutate lst_d (car lst_d) (cdr lst_d) 1))
                )
                lst (get_list_element lst n) (remove_list_element lst n) (+ n 1)
            )
        )
    )
)

(defun init_permute (lst)
  (permutate_first (list) lst (car lst) (cdr lst) 1)
)

(terpri)
(write-string "Task four ")
(terpri)
(write-string "Input ")
(write-string "(-5 0 1 3 5 7 19) (-11 0 1 2 3 4 6 8)")
(terpri)
(write-string "Result ")
(write (init_permute '(a b c d)))
(terpri)