#|
lab5:
- Author: KirilliriK
- Date: 2023-10-18
|#

#| Вспомогательные функции |#
(defun correctChars(val)
    (cond
        ((or (eq val 'q) (eq val #\q)) 'q)
        ((or (eq val 'w) (eq val #\w)) 'w)
        ((or (eq val 'e) (eq val #\e)) 'e)
        ((or (eq val 'r) (eq val #\r)) 'r)
        ((or (eq val 't) (eq val #\t)) 't)
        ((or (eq val 'y) (eq val #\y)) 'y)
        ((or (eq val 'u) (eq val #\u)) 'u)
        ((or (eq val 'i) (eq val #\i)) 'i)
        ((or (eq val 'o) (eq val #\o)) 'o)
        ((or (eq val 'p) (eq val #\p)) 'p)
        ((or (eq val 'a) (eq val #\a)) 'a)
        ((or (eq val 's) (eq val #\s)) 's)
        ((or (eq val 'd) (eq val #\d)) 'd)
        ((or (eq val 'f) (eq val #\f)) 'f)
        ((or (eq val 'g) (eq val #\g)) 'g)
        ((or (eq val 'h) (eq val #\h)) 'h)
        ((or (eq val 'j) (eq val #\j)) 'j)
        ((or (eq val 'k) (eq val #\k)) 'k)
        ((or (eq val 'l) (eq val #\l)) 'l)
        ((or (eq val 'z) (eq val #\z)) 'z)
        ((or (eq val 'x) (eq val #\x)) 'x)
        ((or (eq val 'c) (eq val #\c)) 'c)
        ((or (eq val 'v) (eq val #\v)) 'v)
        ((or (eq val 'b) (eq val #\b)) 'b)
        ((or (eq val 'n) (eq val #\n)) 'n)
        ((or (eq val 'm) (eq val #\m)) 'm)
        ((or (eq val 'й) (eq val #\й)) 'й)
        ((or (eq val 'ц) (eq val #\ц)) 'ц)
        ((or (eq val 'у) (eq val #\у)) 'у)
        ((or (eq val 'к) (eq val #\к)) 'к)
        ((or (eq val 'е) (eq val #\е)) 'е)
        ((or (eq val 'н) (eq val #\н)) 'н)
        ((or (eq val 'г) (eq val #\г)) 'г)
        ((or (eq val 'ш) (eq val #\ш)) 'ш)
        ((or (eq val 'щ) (eq val #\щ)) 'щ)
        ((or (eq val 'з) (eq val #\з)) 'з)
        ((or (eq val 'х) (eq val #\х)) 'х)
        ((or (eq val 'ъ) (eq val #\ъ)) 'ъ)
        ((or (eq val 'ф) (eq val #\ф)) 'ф)
        ((or (eq val 'ы) (eq val #\ы)) 'ы)
        ((or (eq val 'в) (eq val #\в)) 'в)
        ((or (eq val 'а) (eq val #\а)) 'а)
        ((or (eq val 'п) (eq val #\п)) 'п)
        ((or (eq val 'р) (eq val #\р)) 'р)
        ((or (eq val 'о) (eq val #\о)) 'о)
        ((or (eq val 'л) (eq val #\л)) 'л)
        ((or (eq val 'д) (eq val #\д)) 'д)
        ((or (eq val 'ж) (eq val #\ж)) 'ж)
        ((or (eq val 'э) (eq val #\э)) 'э)
        ((or (eq val 'я) (eq val #\я)) 'я)
        ((or (eq val 'ч) (eq val #\ч)) 'ч)
        ((or (eq val 'с) (eq val #\с)) 'с)
        ((or (eq val 'м) (eq val #\м)) 'м)
        ((or (eq val 'и) (eq val #\и)) 'и)
        ((or (eq val 'т) (eq val #\т)) 'т)
        ((or (eq val 'ь) (eq val #\ь)) 'ь)
        ((or (eq val 'б) (eq val #\б)) 'б)
        ((or (eq val 'ю) (eq val #\ю)) 'ю)
        ((or (eq val 'ё) (eq val #\ё)) 'ё)
        ((or (eq val 'Q) (eq val #\Q)) 'Q)
        ((or (eq val 'W) (eq val #\W)) 'W)
        ((or (eq val 'E) (eq val #\E)) 'E)
        ((or (eq val 'R) (eq val #\R)) 'R)
        ((or (eq val 'T) (eq val #\T)) 'T)
        ((or (eq val 'Y) (eq val #\Y)) 'Y)
        ((or (eq val 'U) (eq val #\U)) 'U)
        ((or (eq val 'I) (eq val #\I)) 'I)
        ((or (eq val 'O) (eq val #\O)) 'O)
        ((or (eq val 'P) (eq val #\P)) 'P)
        ((or (eq val 'A) (eq val #\A)) 'A)
        ((or (eq val 'S) (eq val #\S)) 'S)
        ((or (eq val 'D) (eq val #\D)) 'D)
        ((or (eq val 'F) (eq val #\F)) 'F)
        ((or (eq val 'G) (eq val #\G)) 'G)
        ((or (eq val 'H) (eq val #\H)) 'H)
        ((or (eq val 'J) (eq val #\J)) 'J)
        ((or (eq val 'K) (eq val #\K)) 'K)
        ((or (eq val 'L) (eq val #\L)) 'L)
        ((or (eq val 'Z) (eq val #\Z)) 'Z)
        ((or (eq val 'X) (eq val #\X)) 'X)
        ((or (eq val 'C) (eq val #\C)) 'C)
        ((or (eq val 'V) (eq val #\V)) 'V)
        ((or (eq val 'B) (eq val #\B)) 'B)
        ((or (eq val 'N) (eq val #\N)) 'N)
        ((or (eq val 'M) (eq val #\M)) 'M)
        ((or (eq val 'Й) (eq val #\Й)) 'Й)
        ((or (eq val 'Ц) (eq val #\Ц)) 'Ц)
        ((or (eq val 'У) (eq val #\У)) 'У)
        ((or (eq val 'К) (eq val #\К)) 'К)
        ((or (eq val 'Е) (eq val #\Е)) 'Е)
        ((or (eq val 'Н) (eq val #\Н)) 'Н)
        ((or (eq val 'Г) (eq val #\Г)) 'Г)
        ((or (eq val 'Ш) (eq val #\Ш)) 'Ш)
        ((or (eq val 'Щ) (eq val #\Щ)) 'Щ)
        ((or (eq val 'З) (eq val #\З)) 'З)
        ((or (eq val 'Х) (eq val #\Х)) 'Х)
        ((or (eq val 'Ъ) (eq val #\Ъ)) 'Ъ)
        ((or (eq val 'Ф) (eq val #\Ф)) 'Ф)
        ((or (eq val 'Ы) (eq val #\Ы)) 'Ы)
        ((or (eq val 'В) (eq val #\В)) 'В)
        ((or (eq val 'А) (eq val #\А)) 'А)
        ((or (eq val 'П) (eq val #\П)) 'П)
        ((or (eq val 'Р) (eq val #\Р)) 'Р)
        ((or (eq val 'О) (eq val #\О)) 'О)
        ((or (eq val 'Л) (eq val #\Л)) 'Л)
        ((or (eq val 'Д) (eq val #\Д)) 'Д)
        ((or (eq val 'Ж) (eq val #\Ж)) 'Ж)
        ((or (eq val 'Э) (eq val #\Э)) 'Э)
        ((or (eq val 'Я) (eq val #\Я)) 'Я)
        ((or (eq val 'Ч) (eq val #\Ч)) 'Ч)
        ((or (eq val 'С) (eq val #\С)) 'С)
        ((or (eq val 'М) (eq val #\М)) 'М)
        ((or (eq val 'И) (eq val #\И)) 'И)
        ((or (eq val 'Т) (eq val #\Т)) 'Т)
        ((or (eq val 'Ь) (eq val #\Ь)) 'Ь)
        ((or (eq val 'Б) (eq val #\Б)) 'Б)
        ((or (eq val 'Ю) (eq val #\Ю)) 'Ю)
        ((or (eq val 'Ё) (eq val #\Ё)) 'Ё)
    )
)

(defun listFromStr(str)
    (if (symbolp str)
        (listFromStr (string str))
        (mapcar 'correctChars (coerce str 'list))
    )
)

(defun listToStr(lst) (format nil "~{~a~}" lst))

#| Конец вспомогательных функций |#

#| Задача 1 |#
(defun algh (find temp word)
    (if (string= find word) temp word)
)

(defun wrap (str)
    (mapcar (lambda (word) (if (string= 'book word) (listToStr '(cd)) (listToStr (list word)))) str)
)

(terpri)
(write-string "Task one ")
(terpri)
(write-string "Input book, cd, (i take book to my book shelf)")
(terpri)
(write-string "Result ")
(write (wrap '(i take book to my book shelf)))
(terpri)

#| Задача 2 |#
(defun isGlas(char)
    (find char '(а о у и ы э я ю ё е))
)

(defun isSoglasFirst(char)
    (find char '(м н л р))
)

(defun isSoglasSecond(char)
    (find char '(б в г д ж з й п ф к т ш с х ц ч))
)

(defun getCharType(char)
    (cond
		((isGlas char) 3)
        ((isSoglasFirst char) 2)
        ((isSoglasSecond char) 1)
        (T 0)
    )
)

(defun countGlas(word)
    (cond
        ((null word) 0)
        ((isGlas (car word)) (+ 1 (countGlas (cdr word))))
        (T
            (countGlas (cdr word))
        )
    )
)

(defun slogSplitWord (word &optional (result Nil) (previousType 0) (slogs (countGlas word)))
    (let
        (
            (char (car word))
            (nextChar (car (cdr word)))
            (nextNextChar (car (cdr (cdr word))))
			(charType (getCharType (car word)))
        )
        (cond
            ((null word) result)
			((and (= (getCharType nextChar) 2) (= (getCharType nextNextChar) 1))
                (slogSplitWord (cdr (cdr (cdr word))) (append result (list char nextChar '- nextNextChar)) 0 (- slogs 1))
            )
			((or (= slogs 1) (and (<= previousType charType) (not (= previousType 3))))
				(slogSplitWord (cdr word) (append result (list char)) charType slogs)
            )
			((or (eq char 'ъ) (eq char 'й))
				(slogSplitWord (cdr word) (append result (list char '-)) 0 (- slogs 1))
            )
			(T
				(slogSplitWord (cdr word) (append result (list '- char)) 0 (- slogs 1))
            )
        )
    )
)

(defun slogSplitText (text)
    (cond
        ((atom text) (listToStr (slogSplitWord (listFromStr text))))
        (T (mapcar 'slogSplitText text))
    )
)

(terpri)
(write-string "Task two ")
(terpri)
(write-string "Input здравствуйте как ваши дела это строка для проверки слогов")
(terpri)
(write-string "Result ")
(write (slogSplitText '(здравствуйте как ваши дела это строка для проверки слогов)))
(terpri)

#| Задача 3 |#
(defun getSlogsFromWordExact(word &optional (slogResult Nil))
	(let
		(
			(char (car word))
			(nextChars (cdr word))
		)
		(cond
			((null char) slogResult)
			((eq char '-) (list slogResult (getSlogsFromWord nextChars)))
			(T (getSlogsFromWordExact nextChars (append slogResult (list char))))
		)
    )
)

(defun getSlogsFromWord(word)
    (getSlogsFromWordExact (slogSplitWord (listFromStr word)))
)

(defun swapSlogs(firstWord secondWord)
	(let
		(
			(firstSlogs (getSlogsFromWord firstWord))
			(secondSlogs (getSlogsFromWord secondWord))
		)
		(list 
		    (listToStr (append (car secondSlogs) (nth 1 firstSlogs))) 
            (listToStr (append (car firstSlogs) (nth 1 secondSlogs)))
		)
	)
)

(defun spletnya(words keyWord)
    (cond
		((null words) '())
		((null (cdr words)) (swapSlogs (car words) keyWord))
		(T
			(append (swapSlogs (car words) keyWord) (spletnya (cdr words) keyWord))
		)
	)
)

(terpri)
(write-string "Task three ")
(terpri)
(write-string "Input (слово мама папа) сплетня")
(terpri)
(write-string "Result ")
(write (spletnya '(слово мама папа) 'сплетня))
(terpri)

#| Задача 4 |#
(defun textIterating (text dictionary)
    (mapcar (lambda (word) 
		(if (string= (car dictionary) word) 
			(listToStr (list (nth 2 dictionary)))
			(listToStr (list word)))
		) 
		text
    )
)

(terpri)
(write-string "Task four ")
(terpri)
(write-string "Input (И днём и ночью кот знающий) (знающий magna учёный)")
(terpri)
(write-string "Result ")
(write (textIterating '(И днём и ночью кот знающий) '(знающий magna учёный)))
(terpri)