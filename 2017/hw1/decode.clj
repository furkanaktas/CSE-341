; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc                       *
; *  Reworked: Furkan Aktaş -141044029        *
; *********************************************
; *  kelimeler paragrafta normal girilmeli,   *
; *  dictionary'de alt alta girilmeli	        *
; *********************************************

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"


(use 'clojure.java.io )
(require '[clojure.string :as str])

(defn read-as-list
	"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	[filename]
	; Implement this function...
	(def all-string "")

	(with-open [rdr (reader (str filename ".txt"))]
		(doseq [line (line-seq rdr)]
			(def all-string (str all-string line "\n"))))

	(def vect  (str/split all-string #"\n") )


	(def size (count vect))
	
	(def lis ())


	(loop [x (- size 1)]

		(when (>= x 0 )
			(def lis (conj lis ( seq (nth vect x))))
			(recur (- x 1))
			)
		)

	lis

	)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***


(defn spell-checker-0 		                                    ;with linear
	[word]

	(def dict (read-as-list "dictionary1"))
	(def size (count dict))
	(def tempList (seq word))
	(def sonuc false)
	(loop [x 0]

		(when (< x size )
			(if (= tempList (nth dict x))  (do (def sonuc true)))
			(recur (+ x 1))
			)
		)

	sonuc

)

(defn spell-checker-1					                                  ; with index of
	[word]

	(def dict (read-as-list "dictionary1"))
	(def sonuc false)

	(if (not= (.indexOf dict word) -1) (do (def sonuc true)) )
	sonuc


)

(defn Decode-Word
	[word]

	(def alphabet {})
	(def alphabet (map char (range 97 123)))


	(def stop false)
	(while (= stop false)

		(def shuffAlph (shuffle alphabet))
		(def map-word (zipmap alphabet shuffAlph ))

		(def temp "")
		(loop [z 0]

			(when (< z (count word))
				(def temp (str temp (get map-word (nth word z))))
				(recur (+ z 1))
				)
			)

		(def ret-word (seq temp))
		(if (spell-checker-1 ret-word) (do (def stop true)))


		)

	ret-word
	)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A 
	[paragraph]

	Decode-Word 	                                              ;return function: Decode-Word

)

(defn Gen-Decoder-B-0 
	[paragraph]
  	;you should implement this function
)

(defn Gen-Decoder-B-1 
	[paragraph]
  	;you should implement this function
)

(defn Code-Breaker 
	[document decoder]

	(def doc "")

	(with-open [rdr (reader (str document ".txt"))]
		(doseq [line (line-seq rdr)]
			(def doc (str doc line))
			))

	(def doc  (str/split doc #"\s+") )
	

	(def len (count doc))
	(def encoded ())

	(loop [y (- len 1)]

		(when (>= y 0 )
			(def encoded (conj encoded ( seq (nth doc y)))) 	        ;metindeki kelimeler listeye eklendi (encoded())
			(recur (- y 1))
			)
		)

	(def flag true)
	(def result-map {})
	(while (= flag true)

		(def decrypt ())			  ;çözülen kelimeler listesi (decrypt)
		(loop [y (- len 1)]

			(when (>= y 0 )
				(def decrypt (conj decrypt ((decoder encoded) (nth encoded y))))  ;çözülen kelime listeye eklendi
				(recur (- y 1))
				)
			)

		(def highest (count (nth decrypt 0)))
		(def index 0)
		(loop [y 0]
																			;en uzun kelimenin index'i bulundu
			(when (< y (count decrypt))
				(if (< highest (count (nth decrypt y))) (do (def index y) (def highest (count (nth decrypt y)))  ))
				(recur (+ y 1))
				)
			)
		(def result-map (zipmap (nth encoded index) (nth decrypt index))) ; en uzun kelimeden  şifre üretildi

		(def b -10)
		(loop [x 0]
			(def temp-word "")
			(when (< x (count decrypt))
				(if (not= x index) (do
														 (loop [y 0]

															 (when (< y (count (nth decrypt x)))			 ;en uzun kelimeden üretilmiş şifreye göre diğer kelimeler oluşturuldu
																 (if (not= nil (get result-map (nth (nth encoded x) y))) (def temp-word (str temp-word (get result-map (nth (nth encoded x) y) )))
																																												 (def temp-word (str temp-word (nth (nth decrypt x) y))))

																 (recur (+ y 1))
																 )
															 )
															;eğer çözülen kelimeler listesiyle, oluşturulan kelime aynı değilse demekki bulunan şifre yanlış
														 (if (not= (seq temp-word) (nth decrypt x)) (do (def b 10)))
														 )

													 )
				(recur (+ x  1))
				)
			)


		(def enc (nth encoded index))
		(def deco (nth decrypt index))
		(if (= b -10 ) (do (def flag false)   ;şifre doğru bulunduysa döngü bozuluyor
											 (loop [x 0]
												 (when (< x (count decrypt))
													 (if (not= x index) (do
																								(loop [y 0]

																									(when (< y (count (nth decrypt x)))		 ; map te olamayan harfler eklendi
																										(if (= nil (get result-map (nth (nth encoded x) y))) (do (def enc (conj enc (nth (nth encoded x) y)))
																																																						 (def deco (conj deco (nth (nth decrypt x) y)))))

																										(recur (+ y 1))
																										)
																									)
																								)
																							)
													 (recur (+ x  1))
													 )
												 )
											 (println "Decode Keys: " (zipmap enc  deco)) (println encoded)))

		)


	decrypt
)


;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]

	(println (Code-Breaker "document1" Gen-Decoder-A) )
)


;; test code...
(test_on_test_data)
;(read-as-list "document1")

