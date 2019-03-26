(load "csv-parser.lisp")
(in-package :csv-parser)

;; (read-from-string STRING)
;; This function converts the input STRING to a lisp object.
;; In this code, I use this function to convert lists (in string format) from csv file to real lists.

;; (nth INDEX LIST)
;; This function allows us to access value at INDEX of LIST.
;; Example: (nth 0 '(a b c)) => a

;; !!! VERY VERY VERY IMPORTANT NOTE !!!
;; FOR EACH ARGUMENT IN CSV FILE
;; USE THE CODE (read-from-string (nth ARGUMENT-INDEX line))
;; Example: (mypart1-funct (read-from-string (nth 0 line)) (read-from-string (nth 1 line)))

;; DEFINE YOUR FUNCTION(S) HERE

(
	defun list-leveller(lst)
	(if (and (listp (car lst)) (eql (null lst) nil)) ; nil liste sayıldığından (car lst) 'den true olmasın diye(yoksa sürekli if e giriyor.) and ile lst 'nin nil olmadığına bakıldı
		(append (list-leveller (car lst)) (list-leveller (cdr lst)))
		(if (null lst)
			nil
			(cons (car lst) (list-leveller (cdr lst)))
		)
	)
)


;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part1.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      ;; CALL YOUR (MAIN) FUNCTION HERE
      	(list-leveller (read-from-string (nth 0 line)))
      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
