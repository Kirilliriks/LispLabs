#|
lab3:
- Author: KirilliriK
- Date: 2023-09-20
|#

(defun sedj (s size lst)
    (cond
        ((= s 10) '())
        (
            (and
                (> (length lst) 0)
                (<
                    (* 3 (elt lst s))
                    size
                )
            )
            '()
        )
        (T
            (
                if (= (mod (+ s 1) 2) 0)
                    (append
                        (list (sedj (+ s 1) size lst))
                        (
                            + (- (* 9 (expt 2 s))) (* 9 (expt 2 (/ s 2))) 1
                        )
                    )
                    (append
                        (list (sedj (+ s 1) size lst))
                        (
                            + (- (* 8 (expt 2 s))) (* 6 (expt 2 (/ (+ s 1) 2))) 1
                        )
                    )
            )
        )
    )
)

(terpri)
(write-string "Result ")
(write (sedj 0 16 '()))
(terpri)