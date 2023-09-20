#|
lab3:
- Author: KirilliriK
- Date: 2023-09-20
|#

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

(defun sedj (s size last)
    (cond
        (
            (>
                (* 3 last)
                size
            )
            '()
        )
        (T
            (append
                (sedj (+ s 1) size (calc s))
                (list (calc s))
            )
        )
    )
)
(terpri)
(write-string "RS ")
(write (calc 2))
(terpri)
(write-string "Result ")
(write (sedj 0 16 0))
(terpri)