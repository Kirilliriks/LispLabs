#|
test:
- Author: KirilliriK
- Date: 2023-09-18
|#

#| Задание 1 |#
(write-string "Task one ")
(write
    (
        (lambda (x y z)
            (
                cons (car x) (cons (car y) (car z))
            )
        )
        '(T Y D E F (NL KM LM) JL)  '(+ 2 3) '(*(+ 6 8) (- 70 8))
    )
)
(terpri)

(terpri)
#| Задание 2 |#
(write-string "Task two ")
(defun seco (x y z)
            (
                cons (elt x 5) (cons (elt y 1) (elt z 1))
            )
        )

(write (seco '(T Y D E F (NL KM LM) JL)  '(+ 2 3) '(*(+ 6 8) (- 70 8))))
(terpri)

(terpri)
#| Задание 3 |#
(write-string "Task three ")
(defun threee (x)
            (
                list (if (= (signum x) -1) '- '+) (abs x) (round x)
            )
        )

(write (threee -3.14))
(terpri)