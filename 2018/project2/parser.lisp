; Ödeve başlayalı uzun zaman oldu. Bu yüzden başlangıçta CFG nin eksiklerini kendim tamamladım. 
; Parser için aşağıdaki CFG yi uyguladım.   

;	START -> INPUT
;	
;	INPUT -> EXPI 		  |
;			 EXPLISTI
;	
;	
;	
;	EXPI -> (exit)      				  |
;			(+  EXPI EXPI) 				  | 
;			(-  EXPI EXPI) 				  | 
;			(*  EXPI EXPI) 				  | 
;			(/  EXPI EXPI) 				  | 
;			(** EXPI EXPI) 				  | 
;			(set    Id EXPI)			  | 
;			(defvar Id EXPI)			  |
;			(Id EXPLISTI) 				  |
;			(while (EXPB)  EXPLISTI)	  |
;			(if EXPB EXPLISTI)			  |
;			(if EXPB EXPLISTI EXPLISTI)	  |
;			(deffun Id IDLIST EXPLISTI)   |
;			(for (Id EXPI EXPI) EXPLISTI) |
;			Id 			  				  | 
;			IntegerValue                  
;	
;	
;	
;	EXPB ->	 (not EXPB)        | 
;			 (or EXPB EXPB)    | 
;		     (and EXPB EXPB)   |
;			 (equal EXPB EXPB) |
;			 (equal EXPI EXPI) | 
;			 BinaryValue
;	
;	
;	EXPLISTI ->  (concat EXPLISTI EXPLISTI) |
;				 (append EXPI EXPLISTI)     | 
;				 LISTVALUE                  | 
;				 null
;	
;	
;	EXPLISTI -> ( EXPLISTELEMENTS )
;	
;	EXPLISTELEMENTS ->	EXPI 				 |
;						EXPLISTELEMENTS EXPI | 
;						null
;	
;	
;	LISTVALUE -> ‘() 		 |  
;				 ‘( VALUES ) | 
;				 null		 
;	
;	VALUES -> 	VALUES IntegerValue | 
;				IntegerValue
;	
;	
;	
;	IDLIST ->  		() 		      |  
;			   		( IDVALUES )  
;	
;	IDVALUES -> 	IDVALUES Id  | 
;					Id
	



; verilen listenin ilk elamanlarını kontrol ederek 
; EXPI olup olmadığına bakar
(defun isEXPI? (lst)
	(let ((el (car lst)))
		(if (or (equal (car el) "identifier") (equal (car el) "integer"))
			t
			(if (equal (car (cdr el)) "(")
				(let ((el2 (car (cdr lst))))
					(if (equal (car el2) "identifier")
						(isEXPLISTI? (cdr (cdr lst)))
						(let ((el3 (car (cdr el2))))
							(if (or (equal el3 "+") (equal el3 "-") (equal el3 "*")
									(equal el3 "/") (equal el3 "**") (equal el3 "set") 
									(equal el3 "defvar") (equal el3 "if") (equal el3 "while") 
									(equal el3 "deffun") (equal el3 "for") (equal el3 "exit"))
								t
								nil
							)
						)
					)	
				)
				nil
			)
		)		
	)
)

; verilen listenin ilk elamanlarını kontrol ederek 
; EXPLISTI olup olmadığına bakar
(defun isEXPLISTI? (lst)
	(let ((el (car lst)))
		(if (equal (car (cdr el)) "null")
			t
			(if (isLISTVALUE? lst)
				t
				(if (equal (car (cdr el)) "(")
					(let ((el2 (car (cdr lst))))
						(let ((el3 (car (cdr el2))))
							(if (or (equal el3 "concat") (equal el3 "append") )
								t
								(if (isEXPLISTELEMENT? (cdr lst))
									t
									nil
								)
							)
						)	
					)
					nil
				)
			)
		)		
	)
)

; verilen listenin ilk elamanlarını kontrol ederek 
; EXPLISTELEMENT olup olmadığına bakar
(defun isEXPLISTELEMENT? (lst)
	(if (isEXPI? lst)
		t		
		(equal (car (cdr (car lst))) "null")
	)

)

; verilen listenin ilk elamanlarını kontrol ederek 
; LISTVALUE olup olmadığına bakar
(defun isLISTVALUE? (lst) 
	(if (equal (car (cdr (car lst))) "‘")
		(if (equal (car (cdr (car (cdr lst)))) "(")
			(if (isVALUES? (cdr (cdr lst)))
				t
				(if (equal (car (cdr (car (cdr (cdr lst))))) ")")
					t
					nil
				)
			)
			nil
		)
		(equal (car (cdr (car lst))) "null")
	)
)

; verilen listenin ilk elamanlarını kontrol ederek 
; VALUES olup olmadığına bakar
(defun isVALUES? (lst)
	(equal (car (car lst)) "integer")
)


; verilen listenin ilk elamanlarını kontrol ederek 
; EXPB olup olmadığına bakar
(defun isEXPB? (lst)
	(let ((el (car lst)))
		(if (equal (car el) "binary")
			t
			(if (equal (car (cdr el)) "(")
				(let ((el2 (car (cdr lst))))
					(let ((el3 (car (cdr el2))))
						(if (or (equal el3 "not") (equal el3 "or") (equal el3 "and") (equal el3 "equal") )
							t
							nil
						)
					)	
				)
				nil
			)
		)		
	)
)

; verilen listenin ilk elamanlarını kontrol ederek 
; IDLIST olup olmadığına bakar
(defun isIDLIST? (lst)
	(if (equal (car (cdr (car lst))) "(")
		(if (isIDVALUES? (cdr lst))
			t
			(equal (car (cdr (car (cdr lst)))) ")")
		)
	)
)

; verilen listenin içeriğinin tamamına bakar eğer hepsi "identifier" ise ve kapa parantezle sonlanıyorsa IDVALUES 'dur
(defun isIDVALUES? (lst)
	(if (equal (car (cdr (car lst))) ")")
		t
		(and (equal (car (car lst)) "identifier") (isIDVALUES? (cdr lst)))	
	)	
)

; açılan her parantezin kapatışıp kapatılamdığına bakar. 
(defun checkPar (lst parNum)
	(if (null lst)
		(equal parNum 0)
		(if (equal (car (cdr (car lst))) "(")
			(checkPar (cdr lst) (+ parNum 1))
			(if (equal (car (cdr (car lst))) ")")
				(checkPar (cdr lst) (- parNum 1))
				(checkPar (cdr lst) parNum)				
			)
		)
	)
)

; bu fonksiyon ilgili işlemin cfg ye uygun olup olamdığına bakar.
; örneğin + işlemi yapılıyorsa ,  + EXPI EXPI yapısında olmalıdır, bunun kontrolünü yapar.
(defun isSuitable? (lst parNum  EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
	(if (or (equal currOp "+") (equal currOp "-") (equal currOp "*") (equal currOp "/") (equal currOp "**") (equal currOp "equal1"))
		(if (equal (car (cdr (car lst))) "(")
			(if (>= parNum 2)
				(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
				(if (equal (+ parNum 1) 2)
					(if (isEXPI? lst)
						(if (equal (+ EXPINum 1) 2)
							t
							(isSuitable? (cdr lst) (+ parNum 1) (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)
						)	
						nil ;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum currOp)
					)
					(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
				)
			)		
			(if (equal (car (cdr (car lst))) ")")
				(if (equal (- parNum 1) 0) 
					(if (equal EXPINum 2)
						t
						nil
					)
					(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
				)
				(if (and (or (equal (car (car lst)) "identifier") (equal (car (car lst)) "integer")) (< parNum 2))
					(if (equal (+ EXPINum 1) 2)
						t
						(isSuitable? (cdr lst) parNum (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)	
					)
					(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
				)
			)
		)
		(if (or (equal currOp "set") (equal currOp "defvar")) 
			(if (equal (car (cdr (car lst))) "(")
				(if (>= parNum 2)
					(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
					(if (equal (+ parNum 1) 2)
						(if (< IdNum 1)
							nil
							(if (isEXPI? lst)
								(and (equal (+ EXPINum 1) 1) (equal IdNum 1))	
								nil ;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum currOp)
							)
						)
						(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
					)
				)		
				(if (equal (car (cdr (car lst))) ")")
					(if (equal (- parNum 1) 0) 
						(or (and (equal EXPINum 1) (equal IdNum 1))  (equal IdNum 2))
						(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
					)
					(if (and (equal (car (car lst)) "identifier") (< parNum 2))
						(if (equal (+ IdNum 1) 2)
							t
							(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum (+ IdNum 1) IdListNum currOp)	
						)
						(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
					)
				)
			)
			(if (equal currOp "id") 
				(if (equal (car (cdr (car lst))) "(")
					(if (>= parNum 2)
						(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
						(if (equal (+ parNum 1) 2)
							(and (isEXPLISTI? lst) (equal IdNum 1))
							(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
						)
					)
					(if (equal (car (cdr (car lst))) ")")
						(if (equal (- parNum 1) 0) 
							(and (equal EXPLISTINum 1) (equal IdNum 1)) 
							(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
						)
						(if (and (equal (car (car lst)) "identifier") (< parNum 2))
							(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum (+ IdNum 1) IdListNum currOp)
							(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
								(if (isEXPLISTI? lst)
									(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
									nil
								)
								(if (equal (car (cdr (car lst))) "‘")
									(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
									(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
								)
							)
						)
					)	
				)
				(if (equal currOp "if") 
					(if (equal (car (cdr (car lst))) "(")
						(if (>= parNum 2)
							(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
							(if (equal (+ parNum 1) 2)
								(if (equal EXPBNum 0)
									(if (isEXPB? lst)
										(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)	
										nil
									)
									(if (isEXPLISTI? lst)
										(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
										nil
									)
								)
								(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
							)
						)
						(if (equal (car (cdr (car lst))) ")")
							(if (equal (- parNum 1) 0) 
								(and (equal EXPBNum 1) (or (equal EXPLISTINum 1)  (equal EXPLISTINum 2)) )
								(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
							)
							(if (and (equal (car (car lst)) "binary") (< parNum 2))
								(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)	
								(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
									(if (isEXPLISTI? lst)
										(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
										nil
									)
									(if (equal (car (cdr (car lst))) "‘")
										(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
										(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
									)
								)
								;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
							)
						)
					)
					(if (equal currOp "while") 
						(if (equal (car (cdr (car lst))) "(")
							(if (>= parNum 2)
								(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
								(if (equal (+ parNum 1) 2)
									(if (equal EXPBNum 0)
										(if (isEXPB? (cdr lst))
											(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)	
											nil
										)
										(if (isEXPLISTI? lst)
											(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
											nil
										)
									)
									(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
								)
							)
							(if (equal (car (cdr (car lst))) ")")
								(if (equal (- parNum 1) 0) 
									(and (equal EXPBNum 1) (equal EXPLISTINum 1) )
									(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
								)
								(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
									(if (isEXPLISTI? lst)
										(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
										nil
									)
									(if (equal (car (cdr (car lst))) "‘")
										(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
										(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
									)
								)
								;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
							)
						)
						(if (equal currOp "deffun")
							(if (equal (car (cdr (car lst))) "(")
								(if (>= parNum 2)
									(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
									(if (equal (+ parNum 1) 2)
										(if (< IdNum 1)
											nil
											(if (< IdListNum 1)
												(if (isIDLIST? lst)
													(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum (+ IdListNum 1) currOp)	
													nil
												)
												(if (isEXPLISTI? lst)
													(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
													nil
												)
											)
										)
										(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
									)
								)
								(if (equal (car (cdr (car lst))) ")")
									(if (equal (- parNum 1) 0) 
										(and (equal IdNum 1) (equal IdListNum 1) (equal EXPLISTINum 1) )
										(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
									)
									(if (and (equal (car (car lst)) "identifier") (< parNum 2))
										(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum (+ IdNum 1) IdListNum currOp)	
										(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
											(if (isEXPLISTI? lst)
												(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
												nil
											)
											(if (equal (car (cdr (car lst))) "‘")
												(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
												(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
											)
										)
										;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
									)
								)
							)
							(if (equal currOp "for")
								(if (equal (car (cdr (car lst))) "(")
									(if (>= parNum 3)
										(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
										(if (equal (+ parNum 1) 2)
											(if (and (>= IdNum 1) (>= EXPINum 2) )
												(if (isEXPLISTI? lst)
													t;(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
													nil
												)
												(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
											)
											(if (equal (+ parNum 1) 3)
												(if (< IdNum 1)
													nil
													(if (isEXPI? lst)
														(isSuitable? (cdr lst) (+ parNum 1) (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)	
														nil
													)
												)
												(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
											)
										)
									)
									(if (equal (car (cdr (car lst))) ")")
										(if (equal (- parNum 1) 0) 
         									(and (equal IdNum 1) (equal EXPINum 2) (equal EXPLISTINum 1) )
											(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
										)
										(if (and (equal parNum 2) (equal (car (car lst)) "identifier"))
											(if (equal IdNum 1)
												(isSuitable? (cdr lst) parNum (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)
           										(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum (+ IdNum 1) IdListNum currOp)
           									)								
           									(if (and (equal parNum 2) (equal (car (car lst)) "integer") (equal IdNum 1))
												(isSuitable? (cdr lst) parNum (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)
           										(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
													(if (isEXPLISTI? lst)
														(and (equal IdNum 1) (equal EXPINum 2) (equal (+ EXPLISTINum 1) 1) );(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
														nil
													)
													(if (equal (car (cdr (car lst))) "‘")
														(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
														(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
													)
												)		
                        				   )
											
										)
									)
								)
								(if (or (equal currOp "or") (equal currOp "and") (equal currOp "equal2"))
									(if (equal (car (cdr (car lst))) "(")
										(if (>= parNum 2)
											(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
											(if (equal (+ parNum 1) 2)
												(if (isEXPB? lst)
													(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)
													nil ;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum currOp)
												)
												(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
											)
										)		
										(if (equal (car (cdr (car lst))) ")")
											(if (equal (- parNum 1) 0) 
												(equal EXPBNum 2)
												(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
											)
											(if (and (equal (car (car lst)) "binary") (< parNum 2))
												(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)	
												(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
											)
										)
									)
									(if (equal currOp "not")
										(if (equal (car (cdr (car lst))) "(")
											(if (>= parNum 2)
												(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
												(if (equal (+ parNum 1) 2)
													(if (isEXPB? lst)
														(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)
														nil ;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum currOp)
													)
													(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
												)
											)		
											(if (equal (car (cdr (car lst))) ")")
												(if (equal (- parNum 1) 0) 
													(equal EXPBNum 1)
													(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
												)
												(if (and (equal (car (car lst)) "binary") (< parNum 2))
													(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum (+ EXPBNum 1) IdNum IdListNum currOp)	
													(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
												)
											)
										)
										(if (equal currOp "append") 
											(if (equal (car (cdr (car lst))) "(")
												(if (>= parNum 2)
													(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
													(if (equal (+ parNum 1) 2)
														(if (equal EXPINum 0)
															(if (isEXPI? lst)
																(isSuitable? (cdr lst) (+ parNum 1) (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)	
																nil
															)
															(if (isEXPLISTI? lst)
																(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
																nil
															)
														)
														(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
													)
												)
												(if (equal (car (cdr (car lst))) ")")
													(if (equal (- parNum 1) 0) 
														(and (equal EXPINum 1) (equal EXPLISTINum 1) )
														(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
													)
													(if (and (or (equal (car (car lst)) "identifier") (equal (car (car lst)) "integer")) (< parNum 2))
														(isSuitable? (cdr lst) parNum (+ EXPINum 1) EXPLISTINum EXPBNum IdNum IdListNum currOp)	
														(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
															(if (isEXPLISTI? lst)
																(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
																nil
															)
															(if (equal (car (cdr (car lst))) "‘")
																(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
																(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
															)
														)
														;(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
													)
												)
											)
											(if (equal currOp "concat") 
												(if (equal (car (cdr (car lst))) "(")
													(if (>= parNum 2)
														(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
														(if (equal (+ parNum 1) 2)
															(if (isEXPLISTI? lst)
																(isSuitable? (cdr lst) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)	
																nil
															)
															(isSuitable? (cdr lst) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)	
														)
													)
													(if (equal (car (cdr (car lst))) ")")
														(if (equal (- parNum 1) 0) 
															(equal EXPLISTINum 2) 
															(isSuitable? (cdr lst) (- parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
														)
														(if (and (equal parNum 1) (equal (car (cdr (car lst))) "‘"))
															(if (isEXPLISTI? lst)
																(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum (+ EXPLISTINum 1) EXPBNum IdNum IdListNum currOp)
																nil
															)
															(if (equal (car (cdr (car lst))) "‘")
																(isSuitable? (cdr (cdr lst)) (+ parNum 1) EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
																(isSuitable? (cdr lst) parNum EXPINum EXPLISTINum EXPBNum IdNum IdListNum currOp)
															)
														)
													)	
												)
											)	
										)
									)
								)
							)
						)
					)	
				)				
			)	
		)
	)
)

; Bu işlem parse işlemini yapar ve dosyaya yazar.
; lst      :  parse yapılan listedir.
; rule     :  anlık uygulanan rule dur. Parse işlemi rule'a göre ilerler. Örneğin rule EXPI ise EXPI den oluşabilecek ihtimaller üzerinde ilerler.
; ruleVect :  liste üzerinde ilerlerken geçiş yapılan rule buraya eklenir ve bu rule daki işlem ler bitince stack mantığı ile bir önceki rule'a döner ve işlem kaldığı yerden devam etmiş olur.
; spaceNum :  dosyaya yazarken atılacak boşluk sayısıdır.
; keyword  :  bazı rule'lar için  keyword' ün ne olduğuna göre bazı durumları handle eder. Örneğin   (set    Id EXPI) burada Id,   EXPI yazılmadan yazılması lazım  
; writer   :  dosya pointer'ıdır. yazma işlemi için kullanılır.
(defun parser-helper (lst rule ruleVect spaceNum keyword writer isFor? parNum lastPar)
	(if (null lst)
		(write-to-file writer "PARSE SUCCESSFULLY COMPLETED" t)
		(if (equal rule "START")
			(progn
				(write-to-file writer "START" t)
				(parser-helper lst "INPUT" ruleVect (+ spaceNum 2) keyword writer isFor? parNum lastPar)
			)
			(if (equal rule "INPUT")
				(progn
					(print-until spaceNum writer) ; spaceNum kadar boşluk
					(write-to-file writer "INPUT" t)
				    (if (isEXPLISTI? lst)
				    	(parser-helper lst "EXPLISTI" (cons "EXPLISTI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
						(if (isEXPI? lst) 
							(parser-helper lst "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
				    		(write "parse error !!")
						)
				    )
				)
				(if (equal rule "EXPI")
					(progn
						(print-until spaceNum writer) ; spaceNum kadar boşluk
						(if (equal (car (cdr (car lst))) "(")
							(if (and (isIDLIST? lst) (equal keyword "deffun"))
								(progn 
									(write-to-file writer "IDLIST" t)
									(print-until spaceNum writer) ; spaceNum kadar boşluk
									(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
									(parser-helper (cdr lst) "IDLIST" (cons "IDLIST" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
								)
								(if (and (equal (car (car (cdr lst))) "identifier") (isEXPI? (cdr (cdr lst)))) ; for içindeki  (Id EXPI EXPI) case i
									(progn
										(write-to-file writer (car (cdr (car lst))) t)
										(print-until spaceNum writer) ; spaceNum kadar boşluk
										(write-to-file writer (car (cdr (car (cdr lst)))) t)
										(parser-helper (cdr (cdr lst)) (car ruleVect) ruleVect spaceNum keyword writer t (+ parNum 1) (cons parNum lastPar))										
									)
									(if (isEXPI? lst)
										(if (or (isSuitable? lst 0 0 0 0 0 0 "id") (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
											(progn 
												(write-to-file writer "EXPI" t)
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
												(if isFor?
													(if (equal (car (cdr (car (cdr lst)))) "deffun")
														(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "deffun" writer isFor? (+ parNum 1) lastPar)
														(if (equal (car (car (cdr lst))) "identifier")  ; EXPI (Id EXPLISTI) case'i
															(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "id" writer isFor? (+ parNum 1) lastPar)
															(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? (+ parNum 1) lastPar)
														)	
													)
													(if (equal (car (cdr (car (cdr lst)))) "deffun")
														(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "deffun" writer isFor? parNum lastPar)
														(if (equal (car (car (cdr lst))) "identifier")  ; EXPI (Id EXPLISTI) case'i
															(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "id" writer isFor? parNum lastPar)
															(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
														)	
													)
												)
											)
											(write "parse error !!")
										)									
										(if (isEXPLISTELEMENT? (cdr lst))
											(progn
												(write-to-file writer "EXPLISTI" t)
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												(write-to-file writer "  EXPLISTELEMENT" t)
												(if (or (equal (car (car (cdr lst))) "identifier") (equal (car (car (cdr lst))) "integer"))
													(progn
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														(write-to-file writer "    EXPI" t)
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														(write-to-file writer (concatenate 'string "      " (car (cdr (car (cdr lst))))) t)
														(parser-helper (cdr (cdr lst)) "EXPLISTELEMENT" (cons "EXPLISTELEMENT" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
													)	
													(parser-helper (cdr lst) "EXPLISTELEMENT" (cons "EXPLISTELEMENT" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
												)
											)
											(if (isEXPLISTI? lst)
												(if (or (equal (car (cdr (car (cdr lst)))) "concat") (equal (car (cdr (car (cdr lst)))) "append"))
													(if (or (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
														(progn 
															(write-to-file writer "EXPLISTI" t)
															(print-until spaceNum writer) ; spaceNum kadar boşluk
															(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
															(parser-helper (cdr lst) "EXPLISTI" (cons "EXPLISTI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
														)
														(write "parse error !!")
													)
													(progn
														(write-to-file writer "EXPLISTI" t)
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
														(parser-helper (cdr lst) "EXPLISTI" (cons "EXPLISTI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
													)
												)
												(if (isEXPB? lst)
													(if (or (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ) (isSuitable? lst 0 0 0 0 0 0 "equal1") (isSuitable? lst 0 0 0 0 0 0 "equal2"))
														(progn 
															(write-to-file writer "EXPB" t)
															(print-until spaceNum writer) ; spaceNum kadar boşluk
															(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
															(parser-helper (cdr lst) "EXPB" (cons "EXPB" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
														)
														(write "parse error !!")
													)
													(if (isEXPB? (cdr lst))
														(progn
															(write-to-file writer (car (cdr (car lst))) t)
															(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum "while" writer isFor? parNum lastPar)
														)
														(write "parse error !!")
													)
												)		
											)
										)		
									)									
								)
							)
							(if (equal (car (cdr (car lst))) "‘")
								(progn
									(write-to-file writer "EXPLISTI" t)
									(print-until spaceNum writer) ; spaceNum kadar boşluk
									
									(write-to-file writer "  LISTVALUE" t)
									(print-until spaceNum writer) ; spaceNum kadar boşluk
									
									(write-to-file writer "    ‘" t)
									(print-until spaceNum writer) ; spaceNum kadar boşluk
									
									(write-to-file writer "    (" t)
									(parser-helper (cdr (cdr lst)) "LISTVALUE" (cons "LISTVALUE" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
								)
								(if (or (equal (car (cdr (car lst))) "set") (equal (car (cdr (car lst))) "defvar"))
									(progn
										(write-to-file writer (car (cdr (car lst))) t)
										(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum "id" writer isFor? parNum lastPar)
									)
									(if (isEXPB? lst)
										(progn
											(write-to-file writer "EXPB" t)
											
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											
											(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
										)
										(progn 
											(if (or (and (equal (car (car lst)) "identifier") (not (or (equal keyword "deffun") (equal keyword "id")))) (equal (car (car lst)) "integer"))
												(progn
													(write-to-file writer "EXPI" t)
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
												)
												(write-to-file writer (car (cdr (car lst))) t)		
											)											
											(if (equal (car (cdr (car lst))) ")")
												(if isFor?
													(if (equal (- parNum 1) (car lastPar))
														(if (null (cdr lastPar))
															(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer nil 0 '())
															(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? (- parNum 1) (cdr lastPar))
														)
														(if (equal keyword "while")
															(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum "" writer isFor? (- parNum 1) lastPar)
															(parser-helper (cdr lst) (car (cdr ruleVect)) (cdr ruleVect) (- spaceNum 2) keyword writer isFor? (- parNum 1) lastPar)
														)
													)
													(if (equal keyword "while")
														(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum "" writer isFor? parNum lastPar)
														(parser-helper (cdr lst) (car (cdr ruleVect)) (cdr ruleVect) (- spaceNum 2) keyword writer isFor? parNum lastPar)
													)
												)
												(if (equal keyword "id")
													(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum "" writer isFor? parNum lastPar)
													(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
												)
											)
										)
									)									
								)
							)							
						)
					)
					(if (equal rule "EXPB")
						(progn
							(print-until spaceNum writer) ; spaceNum kadar boşluk
							(if (equal (car (cdr (car lst))) "(") 
								(if (isEXPB? lst) 
									(if (or (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ) (isSuitable? lst 0 0 0 0 0 0 "equal1") (isSuitable? lst 0 0 0 0 0 0 "equal2"))
										(progn 
											(write-to-file writer "EXPB" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											(parser-helper (cdr lst) "EXPB" (cons "EXPB" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
										)
										(write "parse error !!")
									)
									(if (isEXPI? lst) 
										(if (or (isSuitable? lst 0 0 0 0 0 0 "id") (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
											(progn 
												(write-to-file writer "EXPI" t)
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												
												(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
												(if (equal (car (cdr (car (cdr lst)))) "deffun")
													(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "deffun" writer isFor? parNum lastPar)
													(if (equal (car (car (cdr lst))) "identifier")
														(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "id" writer isFor? parNum lastPar)
														(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
													)	
												)
											)
											(write "parse error !!")
										)
										(write "parse error !!")
									)	
								)
								(progn
									(if (equal (car (car lst)) "binary")
										(progn
											(write-to-file writer "EXPB" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
										)
										(if (or (equal (car (car lst)) "identifier") (equal (car (car lst)) "integer"))
											(progn
												(write-to-file writer "EXPI" t)
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											)
											(write-to-file writer (car (cdr (car lst))) t)
										)
									) 
									(if (equal (car (cdr (car lst))) ")")
										(parser-helper (cdr lst) (car (cdr ruleVect)) (cdr ruleVect) (- spaceNum 2) keyword writer isFor? parNum lastPar)
										(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
									)
								)
							)
						)
						(if (equal rule "EXPLISTI")
							(progn 
								(print-until spaceNum writer) ; spaceNum kadar boşluk
								(if (equal (car (cdr (car lst))) "(") 
									(if (isEXPLISTELEMENT? (cdr lst))
										(progn
											(write-to-file writer "EXPLISTI" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer "  EXPLISTELEMENT" t)
											
											(if (or (equal (car (car (cdr lst))) "identifier") (equal (car (car (cdr lst))) "integer"))
												(progn
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													(write-to-file writer "    EXPI" t)
													
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													(write-to-file writer (concatenate 'string "      " (car (cdr (car (cdr lst))))) t)
													(parser-helper (cdr (cdr lst)) "EXPLISTELEMENT" (cons "EXPLISTELEMENT" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
												)	
												(parser-helper (cdr lst) "EXPLISTELEMENT" (cons "EXPLISTELEMENT" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
											)
										)
										(if (isEXPLISTI? lst)
											(if (or (equal (car (cdr (car (cdr lst)))) "concat") (equal (car (cdr (car (cdr lst)))) "append"))
												(if (or (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
													(progn 
														(write-to-file writer "EXPLISTI" t)
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														
														(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
														(parser-helper (cdr lst) "EXPLISTI" (cons "EXPLISTI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
													)
													(write "parse error !!")
												)
												(progn
													(write-to-file writer "EXPLISTI" t)
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													
													(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
													(parser-helper (cdr lst) "EXPLISTI" (cons "EXPLISTI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
												)
											)
											(if (isEXPI? lst)
												(if (or (isSuitable? lst 0 0 0 0 0 0 "id") (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
													(progn 
														(write-to-file writer "EXPI" t)
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														
														(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
														(if (equal (car (cdr (car (cdr lst)))) "deffun")
															(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "deffun" writer isFor? parNum lastPar)
															(if (equal (car (car (cdr lst))) "identifier")
																(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "id" writer isFor? parNum lastPar)
																(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
															)	
														)
													)
													(write "parse error !!")
												)
												(write "parse error !!")
											)
										)
									)
									(if (isLISTVALUE? lst)
										(progn
											(write-to-file writer "EXPLISTI" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer "  LISTVALUE" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer "    ‘" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer "    (" t)
											(parser-helper (cdr (cdr lst)) "LISTVALUE" (cons "LISTVALUE" (cons "EXPLISTI" ruleVect)) (+ spaceNum 4) keyword writer isFor? parNum lastPar)
										)
										(progn 
											(if (or (equal (car (car lst)) "identifier") (equal (car (car lst)) "integer"))
												(progn
													(write-to-file writer "EXPI" t)
													(print-until (+ spaceNum 2) writer)
												)
											)
											(write-to-file writer (car (cdr (car lst))) t)
											(if (equal (car (cdr (car lst))) ")")
												(parser-helper (cdr lst) (car (cdr ruleVect)) (cdr ruleVect) (- spaceNum 2) keyword writer isFor? parNum lastPar)
												(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
											)
										)
									)									
								)
							)
							(if (equal rule "LISTVALUE")
								(progn 
									(print-until spaceNum writer) ; spaceNum kadar boşluk
									(if (equal (car (cdr (car lst))) ")")
										(progn
											(write-to-file writer (car (cdr (car lst))) t)
											(parser-helper (cdr lst) (car (cdr (cdr ruleVect))) (cdr (cdr ruleVect)) (- spaceNum 4) keyword writer isFor? parNum lastPar) ; 2
										)
										(progn
											(write-to-file writer "VALUES" t)
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											
											(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
											(parser-helper (cdr lst) "VALUES" (cons "VALUES" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
										)
									)											
								)
								(if (equal rule "VALUES")
									(if (equal (car (cdr (car lst))) ")") 
										(progn 
											(print-until (- spaceNum 2) writer) ; spaceNum kadar boşluk
											
											(write-to-file writer (car (cdr (car lst))) t)
											(parser-helper (cdr lst) (car (cdr (cdr (cdr ruleVect)))) (cdr (cdr (cdr ruleVect))) (- spaceNum 6) keyword writer isFor? parNum lastPar)
										)
										(progn
											(print-until spaceNum writer) ; spaceNum kadar boşluk
											(write-to-file writer (car (cdr (car lst))) t)
											(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
										)
									)
									(if (equal rule "EXPLISTELEMENT")
										(if (equal (car (cdr (car lst))) ")") 
											(progn 
												(print-until (- spaceNum 2) writer) ; spaceNum kadar boşluk
												(write-to-file writer (car (cdr (car lst))) t)
												(parser-helper (cdr lst) (car (cdr (cdr ruleVect))) (cdr (cdr ruleVect)) (- spaceNum 4) keyword writer isFor? parNum lastPar)
											)
											(if (isEXPI? lst)
												(progn
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													(write-to-file writer "EXPI" t)
													(print-until spaceNum writer) ; spaceNum kadar boşluk
													(if (equal (car (cdr (car lst))) "(")
														(if (or (isSuitable? lst 0 0 0 0 0 0 "id") (isSuitable? lst 0 0 0 0 0 0  (car (cdr (car (cdr lst)))) ))
															(progn 
																(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
																(if (equal (car (cdr (car (cdr lst)))) "deffun")
																	(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "deffun" writer isFor? parNum lastPar)
																	(if (equal (car (car (cdr lst))) "identifier")
																		(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) "id" writer isFor? parNum lastPar)
																		(parser-helper (cdr lst) "EXPI" (cons "EXPI" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
																	)	
																)
															)
															(write "parse error !!")
														)
														(progn
															(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
															(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer)
														)
													)
												)
												(if (equal (car (cdr (car lst))) "null") 
													(progn 
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														(write-to-file writer (car (cdr (car lst))) t)
														(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
													)
													(write "parse error !!")
												)
											) 
										)
										(if (equal rule "IDLIST")
											(progn 
												(print-until spaceNum writer) ; spaceNum kadar boşluk
												(if (equal (car (cdr (car lst))) ")")
													(progn
														(write-to-file writer (car (cdr (car lst))) t)
														(parser-helper (cdr lst) (car (cdr ruleVect)) (cdr ruleVect) (- spaceNum 2) "" writer isFor? parNum lastPar)
													)
													(progn
														(write-to-file writer "IDVALUES" t)
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														
														(write-to-file writer (concatenate 'string "  " (car (cdr (car lst)))) t)
														(parser-helper (cdr lst) "IDVALUES" (cons "IDVALUES" ruleVect) (+ spaceNum 2) keyword writer isFor? parNum lastPar)
													)
												)											
											)
											(if (equal rule "IDVALUES")
												(if (equal (car (cdr (car lst))) ")") 
													(progn 
														(print-until (- spaceNum 2) writer) ; spaceNum kadar boşluk
														(write-to-file writer (car (cdr (car lst))) t)
														(parser-helper (cdr lst) (car (cdr (cdr ruleVect))) (cdr (cdr ruleVect)) (- spaceNum 4) "" writer isFor? parNum lastPar)
													)
													(progn
														(print-until spaceNum writer) ; spaceNum kadar boşluk
														(write-to-file writer (car (cdr (car lst))) t)
														(parser-helper (cdr lst) (car ruleVect) ruleVect spaceNum keyword writer isFor? parNum lastPar)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

; writer   : dosya pointer'ı 
; str      : yazılacak metin
; newLine? : yeni satıra geçilecek mi ?
(defun write-to-file (writer str newLine?)
	(if newLine?
		(format writer "~A~%" str)
		(format writer "~A" str)
	)	
)

; writer ile num kadar boşluk atar. 
(defun print-until(num writer)
	(if (equal num 0)
		t
		(progn
			(write-to-file writer " " nil)
			(print-until (- num 1) writer)
		)
	)
)

; yukarıdaki fonksiyonları abstractlaştırır.
(defun parser (lst)
	(with-open-file (writer "141044029.tree"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
  
		(if (checkPar lst 0)
			(progn
				(write-to-file writer "; DIRECTIVE parse tree" t)
				(parser-helper lst "START" '() 0 "" writer nil 0 '())
				(write-to-file writer "" t)
				(write-to-file writer "" t)
				(write "DONE")
			)
			(write "PARANTHESIS ERROR")
		)
		(close writer)
	)
)



; (deffun sumup (x x)
;     ((if (equal x 0)
;         (1)
; 		  ((+ x (sumup ((- x 1)))))
;     ))
; )  		aşağıdaki örnek 	(lst)

(defvar lst '(("operator" "(") ("keyword" "deffun") ("identifier" "sumup") ("operator" "(") ("identifier" "x") ("identifier" "x")
 ("operator" ")") ("operator" "(") ("operator" "(") ("keyword" "if") ("operator" "(") ("keyword" "equal")
 ("identifier" "x") ("integer" "0") ("operator" ")") ("operator" "(") ("identifier" "1") ("operator" ")") ("operator" "(") ("operator" "(")
 ("operator" "+") ("identifier" "x") ("operator" "(") ("identifier" "sumup") ("operator" "(") ("operator" "(")
 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")") ("operator" ")")
 ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")")))



; (deffun sumup (x x)
;     ((if true
; 		  ((+ x (sumup ((- x 1)))))
;     ))
; )  		aşağıdaki örnek 	(lst1)

(defvar lst1 '(("operator" "(") ("keyword" "deffun") ("identifier" "sumup") ("operator" "(") ("identifier" "x") ("identifier" "x")
 ("operator" ")") ("operator" "(") ("operator" "(") ("keyword" "if")  ("binary" "true")  ("operator" "(") ("operator" "(")
 ("operator" "+") ("identifier" "x") ("operator" "(") ("identifier" "sumup") ("operator" "(") ("operator" "(")
 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")") ("operator" ")")
 ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")") ("operator" ")")))


;(while (true) 
;	‘(2 2)
;) 			aşağıdaki örnek 	(lst2)				
(defvar lst2 '( ("operator" "(")  ("keyword" "while") ("operator" "(") ("binary" "true") ("operator" ")") ("operator" "‘") ("operator" "(") ("integer" "2") ("integer" "2") ("operator" ")") ("operator" ")")) )



;(while ((or false true)) 
;	((+ x (sumup ((- x 1)))))
;) 			aşağıdaki örnek 	(lst3)				
(defvar lst3 '( ("operator" "(")  ("keyword" "while") ("operator" "(") ("operator" "(") ("keyword" "or") ("binary" "false") ("binary" "true") ("operator" ")") ("operator" ")") ("operator" "(") ("operator" "(")
 ("operator" "+") ("identifier" "x") ("operator" "(") ("identifier" "sumup") ("operator" "(") ("operator" "(")
 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")") ("operator" ")") ("operator" ")")
 ("operator" ")") ("operator" ")") ("operator" ")")) )


;(for (x (- x 1) 
;        (for (x (- x 1) x) 
;     		(1)
;		)
;   	 ) 
;     (1)
;)

(defvar lst4 '( ("operator" "(")  ("keyword" "for") ("operator" "(") ("identifier" "x") ("operator" "(")
 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")")  ("operator" "(")  ("keyword" "for") ("operator" "(") ("identifier" "x") ("operator" "(")
 ("operator" "-") ("identifier" "x") ("integer" "1") ("operator" ")")  ("identifier" "x") ("operator" ")") 
               ("operator" "‘") ("operator" "(") ("integer" "1") ("operator" ")") ("operator" ")") ("operator" ")") 
               ("operator" "‘") ("operator" "(") ("integer" "1") ("operator" ")") ("operator" ")") ) )



(parser lst)
(parser lst1)
(parser lst2)
(parser lst3)
(parser lst4)
