#|
lab4:
- Author: KirilliriK
- Date: 2023-09-24
|#

(defun calculate_factorial(n r)
    (
        (lambda (n r)
            (cond
                    ((= n 0) r)
                    (T
                        (calculate_factorial (- n 1) (* r n))
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