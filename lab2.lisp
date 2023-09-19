#|
lab2:
- Author: KirilliriK
- Date: 2023-09-19
|#

(defun rev (l)          #| Объявляем функцию rev принимающую в качестве параметра список l, функция будет переворачивать списки|#
    (cond               #| Используем оператор cond для проверки двух вариантов, когда список l пуст или нет |#
        ((null l) '())  #| Если l пуст то возвращаем пустое |#
        (T              #| В ином случае выполняем дальнейшие инструкции |#
            (append     #| Используем оператор append для объединения списков |#
                (rev (cdr l)) #| Рекурсивный вызов функции rev в качестве входящего параметра используем список БЕЗ головы, который получаем с помощью CDR |#
                (list         #| Формируем список с помощью оператора list |#
                    (if (atom (car l)) #| Если голова списка l является атомом |#
                            (car l)    #| То возвращаем голову |#
                            (rev (car l)) #| Иначе рекурсивно переворачиваем список |#
                    )
                )
            )
        )
    )
)

(defun algh (l)
    (cond
        ((null l) '())
        (T
            (append
                (list
                    (if (atom (car l))
                        (car l)
                        (rev (car l))
                    )
                )
                (algh (cdr l))
            )
        )
    )
)

(terpri)
(write-string "Task ")
(write (algh '(1 ((2 3) 4) 5 6)))
(terpri)