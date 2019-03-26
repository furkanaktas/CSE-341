; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Furkan Aktaş, No: 141044029      *
; *********************************************
; *  dosya sadece isimle verilmeli            *
;                         (.coffee eklenir)   *
; *********************************************

(use 'clojure.java.io)
(require '[clojure.string :as str])


(defn digit? [c] (and (>= 0 (compare \0 c)) (>= 0 (compare c \9)))) ; character in sayı olup olamadığına bakar

(defn letter? [c] (or (and (>= 0 (compare \A c)) (>= 0 (compare c \Z))) (and (>= 0 (compare \a c)) (>= 0 (compare c \z))) )) ;character in  harf olup olmadığına bakar



(defn lexer [filename]

  (def all-string "")

  (with-open [rdr (reader (str filename ".coffee"))]
    (doseq [line (line-seq rdr)]
      (def all-string (str all-string line " "))))          ; \n yerine " "  koyuldu


  (def all-string (str/replace all-string #" " "®" ) )      ; her " " , "®" ile değiştirildi

  ;(println all-string)

  (def size (count all-string))
  (def vect [])
  (def temp "")      ;
  (loop [i 0]

    (when (< i size )
      (if (= (nth all-string i) \® )
        (do (def temp "")  )
        (do (if (or (= (nth all-string i) \( ) (= (nth all-string i) \) ) (and (or (= (nth all-string i) \+ ) (= (nth all-string i) \* )
                                                                                   (= (nth all-string i) \/ ) (= (nth all-string i) \- )) (= (nth all-string (+ i 1)) \® )))

                     (do (def vect (conj vect (str temp (nth all-string i)))) (def temp "") )   ; gelen character (, ), +, *, / bunlardan biriyse temp+operator şeklinde (""+operator) vect e direk eklenir

                     (do (def temp (str temp (nth all-string i))) ; +, -, *, /, harfler veya sayılar için sonraki eleman aşağıdakilerden biri olana kadar temp'e eklenir
                         (if (and (not= size (+ i 1)) (or (= (nth all-string (+ i 1)) \® ) (= (nth all-string (+ i 1)) \( ) (= (nth all-string (+ i 1)) \) )))

                                      (do (def vect (conj vect temp)) (def temp "") ))
                                                                                                                              ))))
      (recur (+ i 1))
      )
    )


  ; buraya kadar tokenler oluşturuldu ve vect e eklendi
  ;(println vect)


  ;buradan aşağısında token lerin regular expression ile uygunluğu kontrol edilir.

  (def tempVect [] )                                        ;
  (def len (count vect) )


  (def y 0)

  (while (< y len )

    (if (or (= (nth vect y) "+") (= (nth vect y) "-") (= (nth vect y) "*") (= (nth vect y) "/") (= (nth vect y) "(") (= (nth vect y) ")") )

      (do (def tempVect (conj tempVect (nth vect y))))    ; token +, -, *, /, (, ) ise direk eklenir

      (do  (if (or (= (nth vect y) "and") (= (nth vect y) "or")     (= (nth vect y) "append") (= (nth vect y) "concat") (= (nth vect y) "for")
                    (= (nth vect y) "if")  (= (nth vect y) "true")   (= (nth vect y) "false")  (= (nth vect y) "not")    (= (nth vect y) "equal")
                    (= (nth vect y) "set") (= (nth vect y) "deffun") (= (nth vect y) "then")   (= (nth vect y) "else")   (= (nth vect y) "while"))

             (do (def tempVect(conj tempVect (nth vect y)))) ; and, or, append .... vs  ise direk eklenir.

             (do
                   (if (= (nth (nth vect y) 0) \-) (do (def x 1)) (do (def x 0))) ; tokenin ilk character i \- ise x 1  değilse 0 olur

                   (def digit false)                     ; digit  digitler için
                   (def letter false)                    ; letter harfler için  kontrolcü
                   (if (true? (digit? (nth (nth vect y) x)) )  (def digit true) (def letter true) ) ; character digitse digit, değilse letter true oldu


                   (def counts x)                         ; tüm characterler'in digit veya harf ten oluşup olmaığının kontrolü için
                   (def size (count (nth vect y)))        ; token  size'ı
                   (while (and (true? digit) (< x size) ) ; digit ten olan token için
                     (if (true? (digit? (nth (nth vect y) x)) )  (do (def counts (+ counts 1)) ) ) ;digit olduğu sürece counts artar
                     (def x (+ x 1))
                     )

                   (while  (and (true? letter) (< x size) ) ;letter dan oluşan token için
                     (if (true? (letter? (nth (nth vect y) x)) )  (do (def counts (+ counts 1)) ) )  ;letter olduğu sürece counts artar
                     (def x (+ x 1))
                     )

                   ; eğer token uzunluğu ile counts aynı değilse yada ilk char "-" olup 2. char harf (-abc) ise boş vektör dündürür.Değilse token regular expression a  uygundur.
                   (if (or (and (= (nth (nth vect y) 0) \-) (true? (letter? (nth (nth vect y) 1)) ))
                           (not= counts size))
                                 (do  (println (nth vect y) "Don't fit the regular expression!") (def tempVect []) (def y len)) (do (def tempVect(conj tempVect (nth vect y)))))))))
    (def y  (+ y 1))
    )


  (def lis ())
  (def size (count tempVect))

  (loop [x (- size 1)]

    (when (>= x 0 )
      (def lis (conj lis (nth tempVect x)))                 ; tempVect ten listeye alındı

      (recur (- x 1))
      )
    )

  ; (println lis)
  lis

  )


(println (lexer "CoffeeSample"))