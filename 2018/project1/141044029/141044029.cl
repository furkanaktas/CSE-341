; ************************************************************************************************
; *  341 Programming Languages                													 *
; *  Fall 20181                               													 *
; *  Author: Furkan Aktaş, No: 141044029      													 *
; ************************************************************************************************
;																							     *		
;																							     *		
; Regular expresiion olarak pdf' deki bu yapı esas alındı									     *
;																							     *	
; Keywords     ->  and, or, not, equal, append, concat, set, deffun, for, while, if, exit	     *
; Operators    ->  +, -, /, *, (, ), **														     *
; BinaryValue  ->  true | false 	    (true yada false)									     *	
; IntegerValue ->  [-]*[1-9]*[0-9]+ 	(yalnızca sayı (negatif pozitif))					     *	
; Identifiers  ->  [a-zA-z]+	        (yalnızca harf)										     *	 
;*************************************************************************************************																


; dosyadan verileri tek liste olacak şeklinde okur,   https://www.tutorialspoint.com/lisp/lisp_file_io.htm  'daki tutorial'dan yardım alındı
(defun get-file (filename)
	(let ((in (open filename :if-does-not-exist nil)))
	   (when in
	      (loop for line = (read-line in nil)
			while line 
	      		collect line))))

; verilen string'i,  verilen regex'e göre, regex'i silerek veya silmeyerek (deleteReg? 'e bağlı)  parçalara ayırır. 
(defun split-by-regex (str regex deleteReg?)
	(if (equal str "")
		nil
		(let ((index (search regex str)))
			(if (equal index nil)
				; eğer  kalan string regex'i içermiyorsa 
				(list str)					
				(if (equal deleteReg? t)
					; regex'e kadar olan string'e,  kalan string recursive olarak eklenir,  "abc"  regex b  --->  çıktı ("a" "c") 
					(append (list (subseq str 0 index)) (split-by-regex (subseq str (+ index 1) (length str)) regex deleteReg?)) 	; regex listeye eklenmez
					; regex'e kadar olan string'e, regex eklenir ve kalan string recursive olarak eklenir, "ab(c"  regex  (  --> çıktı ("ab" "(" "c") 
					(append (append (list (subseq str 0 index)) (list regex)) (split-by-regex (subseq str (+ index 1) (length str)) regex deleteReg?)) ; regex eklenir
				)	
			)
		)
	)
)

; split-by-regex fonksiyonunu listedeki her string için uygular
(defun splitter (lst regex deleteReg?)
	(if (null lst)
		nil
		(if (atom (car lst))
			(if (equal deleteReg? t)
				(append (split-by-regex (string-trim regex (car lst)) regex deleteReg?) (splitter (cdr lst) regex deleteReg?)) ; her string trim edildi, parçalandı, regex silindi
				(append (split-by-regex (car lst) regex deleteReg?) (splitter (cdr lst) regex deleteReg?)) ; regex silinmeden parçalandı
			)
			(append (splitter (car lst) regex deleteReg?) (splitter (cdr lst) regex deleteReg?))
		)
	)
)

; verilen string liste listesini map eder, boş elamanları("") yok sayarak tek liste haline getirir.
; buradaki girdi liste,  (("string") ("") ("string2"))  gibidir
; çıktı ("string" "string2")  gibidir.
(defun merger (lst)
	(if (null lst)
		nil
		(if (atom (car lst))
			(if (equal (car lst) "")
				(merger (cdr lst))
				(cons (car lst) (merger (cdr lst)))
			)
			(append (merger (car lst)) (merger (cdr lst)))
		)
	)
)

; boşluksuz, boş string'siz temiz bir string listesi return eder.
; space ve tab silinerek parçalandı, daha sonra parantezlere göre (parantezler silinmeden) parçalandı
; her bir adımda , parçalandıktan sonra merger fonksiyonu ile boş elemanlar temizlenip, birleştirildi
(defun get-words (lst)
	(merger (splitter 
		(merger (splitter 
			(merger (splitter 
				(merger (splitter lst " " t)) 							; boşluk      " "  ve sil (t) 
										   "	" t)) 					; tab      "	"  ve sil (t)
										    		 ")" nil)) 			; kapa   parantez  ve silme (nil)
										    		 		  "(" nil)) ; aç     parantez  ve silme (nil) 
)


; verilen string'in tamamının sayılardan oluşup oluşmadığına bakar, T yada NIL
(defun isNumber (str num)
	(if (or (and (equal num 0) (equal str "")) (and (equal num 0) (equal (subseq str 0 1) "0") (> (length str) 1))) ; ilk verilen string boşsa veya  sayı 0 ile başlıyorsa ve 1'den fazla basamaklı ise (01 0212)
		nil
		(if (equal str "")
			t
			(if (digit-char-p (coerce (subseq str 0 1) 'character)) 
				(isNumber (subseq str 1 (length str)) (+ num 1))
				nil
			)
		)
	)
)

; helper isNumber fonksiyonunu çağırır, T yada NIL
(defun number? (str)
	(isNumber str 0)
)

; verilen string'in tamamının harflerden oluşup oluşmadığına bakar, T yada NIL
(defun isLetter (str num)
	(if (and (equal num 0) (equal str ""))
		nil
		(if (equal str "")
			t
			(if (alpha-char-p (coerce (subseq str 0 1) 'character)) 
				(isLetter (subseq str 1 (length str)) (+ num 1))
				nil
			)
		)
	)
)

; helper isLetter fonksiyonunu çağırır, T yada NIL
(defun letter? (str)
	(isLetter str 0)
)

; ilk charachter'i - olduğu bilinen string'in sayı olup olmadığını kontrol eder
(defun negativeNumber(str)
	(if (and (= (length str) 2) (equal (subseq str 1 2) "0") )  ; -0 ise 
		nil 											
		(if (number? (subseq str 1 (length str))) 		; - hariç kalan sayı yollandı
			(list "integerValue" str)
			nil
		)
	)
)


; verilen string'i  uygun token olarak listeler
(defun tokenzier (str) 
	(if (or (equalp str "(") (equalp str ")") (equalp str "+") (equalp str "-") (equalp str "*") (equalp str "/") (equalp str "**"))
		(list "operator" str)
		( if (or (equalp str "and") (equalp str "or") (equalp str "append") (equalp str "concat") (equalp str "for") (equalp str "if") (equalp str "not")
		    						 (equalp str "equal") (equalp str "set") (equalp str "deffun") (equalp str "while") (equalp str "exit") )
			(list "keyword" str)
			(if (or (equalp str "true") (equalp str "false"))
				(list "binaryValue" str)
				(if (letter? str)
					(list "identifier" str)
					(if (number? str)
						(list "integerValue" str)
						(if (equalp (subseq str 0 1) "-")
							(negativeNumber str)
							nil
						)
					)
				)
			)			
		)
	)
)

; listedeki her bir string'in tokenzier fonksiyonu ile token'ini belirler
; eğer token' de sıkıntı varsa  mesajı basar ve durur.
(defun lexer-helper(lst)
	(if (null lst)
		nil
		(let ((token (tokenzier (car lst))))
			(if (equal token nil)
				(format nil "Don't fit the regular expression : ~A" (car lst))
				(cons token (lexer-helper (cdr lst)) )
			)
		)
	)
)

; dosyadan listeye okunur (get-file)
; listeden stringler parçalara ayrılır (get-words)
; ayrılan bu parçalar token yapılır (lexer-helper)
(defun lexer (filename)
	(let ((lst (get-file filename)))
		(if (equal lst nil)
			(format nil "Error while opening file : ~A" filename)
			(lexer-helper (get-words lst))
		)
	)
)

(defconstant filename "test.g++")
(write (lexer filename))

