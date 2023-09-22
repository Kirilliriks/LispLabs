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

(defun cons_list(element lst) #| Функция которая объединяет один элемент с другим |#
    (cond
        ((null lst) '())
        (T
            (cons
                (cons element (car lst))
                (cons_list element (cdr lst))
            )
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
(write-string "Input (12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9)")
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
(write-string "Input (12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9)")
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
(write-string "Input (-5 0 1 3 5 7 19) (-11 0 1 2 3 4 6 8)")
(terpri)
(write-string "Result ")
(write (merge_lists '(-5 0 1 3 5 7 19) '(-11 0 1 2 3 4 6 8)))
(terpri)

#| Задача 4 |#

#| Адаптированный алгоритм пользователя VH (https://www.cyberforum.ru/lisp/thread393117.html)|#
(defun permutationVH(lst)
    (if lst
        (if (atom lst)
            lst
            (apply #'append
                 (mapcar #'(lambda (elem result)
                             (if (atom elem)
                                 (if result
                                     (mapcar #'(lambda (seq)
                                                 (cons elem seq))
                                             result)
                                     (list (list elem))
                                 )
                                 (if result
                                     (apply #'append
                                            (mapcar #' (lambda (seq)
                                                        (mapcar #'(lambda (e)
                                                                    (cons e (cons seq nil)))
                                                                elem)
                                                        )
                                                    result
                                            )
                                     )
                                     elem
                                 )
                             )
                         )
                         (mapcar #'permutationVH lst)
                         (mapcar #'permutationVH (mapcar #'(lambda (e) (remove e lst)) lst))
                )
            )
        )
    )
)

(terpri)
(write-string "Task four ")
(terpri)
(write-string "Input ((A B) (C D))")
(terpri)
(write-string "Result ")
(write (permutationVH '((A B) (C D))))
(terpri)