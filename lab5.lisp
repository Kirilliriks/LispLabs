#|
lab5:
- Author: KirilliriK
- Date: 2023-10-18
|#

(defun algh (find temp l)

    (cond
        ((null l) '())
        (T
            (append
                (list
                    (if (string= find (car l))
                        temp
                        (car l)
                    )
                )
                (algh find temp (cdr l))
            )
        )
    )
)

(terpri)
(write-string "Task one ")
(terpri)
(write-string "Input book, cd, (i take book to my book shelf)")
(terpri)
(write-string "Result ")
(write (algh 'book 'cd '(i take book to my book shelf)))
(terpri)
