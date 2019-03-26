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
(defun merge-list(list1 list2)
	(if (and (null list1) (null list2)) 
		nil 							; 2 list'te null'sa nil döndür
		(if (null list1) 
			(merge-list list2 nil) 		; list1 null'sa list2'den devam et
			(cons (car list1) (merge-list (cdr list1) list2))  ; list1'in ilk elemanında, list1'in kalanını ve list2 yi recursion' la ekle
		)
	)
)


;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part2.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      ;; CALL YOUR (MAIN) FUNCTION HERE
      (merge-list (read-from-string (nth 0 line)) (read-from-string (nth 1 line)))


      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
