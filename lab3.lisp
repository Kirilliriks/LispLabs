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
        ((> (* 3 last) size) '() )
        (T
            (append
                (list (calc s))
                (sedj (+ s 1) size (calc s))
            )
        )
    )
)

(terpri)
(write-string "Result ")
(write (sedj 0 16 0))
(terpri)