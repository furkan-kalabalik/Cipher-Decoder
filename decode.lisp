; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"


;;******************READ FUNCTIONS******************************************
(defun read-as-list (filename)
(with-open-file (s filename)
    (let (words)
        (let (letter)
            (read-helper s letter words)
))))

(defun read-helper (s letter words);;This function read file char by char
    (let  ((c (read-char s nil)))
        (if (eq c nil) (progn 
            (push (reverse letter) words)
            (reverse words)
            )
            (progn 
                (when (char/= c #\Space)
                (if (char/= c #\Newline) (push c letter)) ;;if readed char is letter push to the word list
                )
                (when (or (char= c #\Space) (and (char= c #\Newline) (not (eql letter nil)))) ;;if readed char is blank or newline that doesn't implies new paragraph push word to main document
                (push (reverse letter) words) 
                (setf letter '()))
                (read-helper s letter words)
            )
        )
        
    )       
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***


(defun remove-nth (lst n);;Removes nth element of list
    (cond ((eql lst nil) (nil))
          ((= 0 n) (cdr lst))
          (t (cons (car lst) (remove-nth (cdr lst) (- n 1))))
    )
)

(defun change-nth (lst n change);;Changes nth element of list
    (cond ((eql lst nil) (nil))
          ((= n 0) (cons change (cdr lst)))
          (t (cons (car lst) (change-nth (cdr lst) (- n 1) change)))
    )
)

(defun to-str (word);;Converts char list to a string
    (if (not (eql word nil))
        (concatenate 'string (string (car word)) (to-str (cdr word)))
    )
)

;;*************************LETTER COUNTING FUNCTIONS***********************************
(defun count-letters-B-0 (filename);;Counts number of each 26 letter in document with help of helper
(with-open-file (s filename)
    (let ((alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
        (let ((letter '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
            (count-helper-B-0 s letter alphabet)
))))


(defun count-helper-B-0 (s letter alphabet);;increment index of particular letter if the readed char is a letter. If end of file return count numbers
    (let  ((c (read-char s nil)))
        (if (eq c nil) letter
            (if (char/= c #\Newline)
                (if (char/= c #\Space) (count-helper-B-0 s (change-nth letter (position c alphabet) (+ 1 (nth (position c alphabet) letter))) alphabet) (count-helper-B-0 s letter alphabet))             
            (count-helper-B-0 s letter alphabet)) 
        )
        
    )       
)


(defun count-letters-B-1 (filename);;Counts number of letters that comes after most frequent letter
(with-open-file (s filename)
    (let ((alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
        (let ((letter '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
            (count-helper-B-1 s letter alphabet nil)
))))


(defun count-helper-B-1 (s letter alphabet prev);;increment index of particular letter if the readed char is a letter and comes after most frequent letter. If end of file return count numbers
    (let  ((c (read-char s nil)))
        (if (eq c nil) letter
            (if (char/= c #\Newline)
                (if (and (not (eql (member prev '(#\e #\t #\a #\o #\i #\n)) nil)) (char/= c #\Space)) (count-helper-B-1 s (change-nth letter (position c alphabet) (+ 1 (nth (position c alphabet) letter))) alphabet c) (count-helper-B-1 s letter alphabet c))             
            (count-helper-B-1 s letter alphabet prev)) 
        )
        
    )       
)


(defun count-to-map (counts alphabet map);;With using letter counting numbers, creates a map from high to low
    (if (not (eq (length map) 6))(progn
        (push (nth (position (reduce 'max counts) counts) alphabet) map)
        (count-to-map (change-nth counts (position (reduce 'max counts) counts) 0) alphabet map))
        (reverse map)
    )
)

(defun get-count-mapping (counts alphabet);;Gets the mapping for most frequent letters in English alphabet with help of helper
    (let ((map))
        (count-to-map counts alphabet map)
    )
)


;;*******************************FACTORIAL CIPHER CALCULATING**************************************************
;;This section uses factoraid number system to calculate nth factorial of particular sequence. It calculates  *
;;factoraid number by dividing until quetioent become 0 and after finding factoraid num, creates cipher       *
;;alphabet according to that number.                                                                          *
;;*************************************************************************************************************
(defun complete-factoraid (factor alphabet);;Since our factoraid numbers must be have length 26, we must add up to factor until it reaches desired length
    (if (not (eql (length factor) (length alphabet)))
        (progn (push 0 factor)
                (complete-factoraid factor alphabet))
        factor
    )
)
(defun get-factoraid (factoraid n count alphabet);;Finds factoraid number by dividing 1,2,3... so it gives factoraid number to find nth permutation
    (if (not (eql n 0))
        (progn 
               (push (mod n count) factoraid)
               (get-factoraid factoraid (floor n count) (+ count 1) alphabet)
        ) (complete-factoraid factoraid alphabet)
    )
)

(defun factoraid (n alphabet) ;;Get's the factoraid number with desired length
    (let ((factor))
        (get-factoraid factor n 1 alphabet)
    )
)

(defun get-cipher (n);;Finds the nth cipher for Decoder-A
    (let ((cipher) (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
        (cipher-helper (factoraid n alphabet) cipher alphabet)
    )
)


(defun get-cipher-DecB-0 (n filename);;Finds the nth cipher for Decoder-B-0. This is different since we created some of mappings. It creates permutation for remaining letters
    (let ((cipher) (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
        (cipher-helper (factoraid n (set-difference alphabet (get-count-mapping (count-letters-B-0 filename) alphabet))) cipher (set-difference alphabet (get-count-mapping (count-letters-B-0 filename) alphabet)))
    )
)

(defun get-cipher-DecB-1 (n filename);;Finds the nth cipher for Decoder-B-1. This is different since we created some of mappings. It creates permutation for remaining letters
    (let ((cipher) (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
        (cipher-helper (factoraid n (set-difference alphabet (get-count-mapping (count-letters-B-1 filename) alphabet))) cipher (set-difference alphabet (get-count-mapping (count-letters-B-1 filename) alphabet)))
    )
)

(defun cipher-helper (factoraid cipher alphabet);;By using factoraid number starts from leftmost of number and alphabet remove by looking indexes and add them to cipher. So cipher can be created.
    (if (not (eql (length alphabet) 0))
        (progn 
            (push (nth (car factoraid) alphabet) cipher)
            (cipher-helper (cdr factoraid) cipher (remove-nth alphabet (car factoraid)))
        ) (reverse cipher)
    )
)

;;************************SPELL HELPER-0************************************************

(defun spell-helper-0 (word letter s);;Reads dictionary and creates word. After that compare created word with the given word. If they matches return T otherwise return NIL
    (let ((c (read-char s nil)))
        (cond ((eql c nil) (and t nil))
              ((char/= c #\Newline) (progn (push c letter) (spell-helper-0 word letter s)))
              ((char= c #\Newline) (if (compare-2-word word (reverse letter)) (and t t) (spell-helper-0 word '() s)))
              )
    )
)

(defun compare-2-word (word1 word2);;Compares two word. From first char to the end. If they matches return T, otherwise NIL
  (cond ((and (= 0 (length word1)) (= 0 (length word2))) (not nil))
        ((or (null word1) (null word2)) (not t)) 
        ((char/= (car word1) (car word2)) (not t))
        ((char= (car word1) (car word2)) (and t (compare-2-word (cdr word1) (cdr word2)))))
)

(defun spell-checker-0 (word filename);;Main spell-checker-0 function, looks for the word is in dictionary or not
	(with-open-file (s filename)
    (let ((is nil))
        (setf is (spell-helper-0 word '() s))
        (close s) 
    (if (not (eql is nil)) (and t t) (and nil nil))
    )
    )
)

;;************************************SPELL HELPER-1*****************************************

(defun create-hash-table (filename);;This function reads the entire dictionary and creates hash-map for every particular word.equalp used for string keys.
    (with-open-file (s filename)
    (let ((word-table (make-hash-table :test 'equalp)) (word))
        (read-hash-table s word word-table)
    )
))

(defun read-hash-table (s word word-table);;Reads dictionary and creates words. After that hashes that by converting into string and add that to hash-map as string key and list sequence of letters
    (let  ((c (read-char s nil)))
        (if (eq c nil) (progn 
            (setf (gethash (to-str (reverse word)) word-table) (reverse word))
            word-table
            )
            (progn 
                (when (char/= c #\Space)
                (if (char/= c #\Newline) (push c word))
                )
                (when (or (char= c #\Space) (char= c #\Newline))
                (setf (gethash (to-str (reverse word)) word-table) (reverse word))
                (setf word '()))
                (read-hash-table s word word-table)
            )
        )
        
    )   
)

(defun spell-checker-1 (word filename);;Checks whether given word is in hash-table i.e in dictionary or not.
 	(let ((word-table (create-hash-table filename)))
        (if (gethash (to-str word) word-table) t nil)
    )
)

;;*************************PARAGRAPH CHECKERS**************************************************************
;;Paragrap checkers takes an decoded document. After that they looks for every element in that document.  *
;;Elements are words. By using spell checkers they conclude that every word in decoded paragraph is in    *
;;dictionar and it is the correct patter or not.                                                          *
;;********************************************************************************************************* 

(defun check-paragraph-checker-0 (document dictionary)
    (cond ((eql document nil) (not nil))
          ((eql nil (spell-checker-0 (car document) dictionary)) (not t))
          (t (check-paragraph-checker-0 (cdr document) dictionary))
    )  
)

(defun check-paragraph-checker-1 (document dictionary)
    (cond ((eql document nil) (not nil))
          ((eql nil (spell-checker-1 (car document) dictionary)) (not t))
          (t (check-paragraph-checker-1 (cdr document) dictionary))
    )  
)

;;************************ENCODER DECODER FUNCTIONS*****************************************
;;This functions is used for creating mapped documents. They works inversly. Encode takes  *
;;cipher and base alphabet. Iterate over every letter and changes them according to given  *
;;mapping with corresponding letter. Creates the encoded document. Decoder works inverse   *
;;It creates mapping with cipher to base alphabet and constructs decoded document. After   *
;;decoding we can determine decoded doc is totally English or not.                         *
;;******************************************************************************************

(defun encode (cipher-alphabet document base-alphabet)
        (let ((newdoc nil))
            (let ((newword))
                (loop for word in document
                    do(loop for char in word
                        do(push (nth (position char base-alphabet) cipher-alphabet) newword)
                    )
                    do(push (reverse newword) newdoc)
                    (setf newword '())
                )
            )
            (reverse newdoc)
        )
    )

(defun decode (cipher-alphabet document base-alphabet)
        (let ((newdoc nil))
            (let ((newword))
                (loop for word in document
                    do(loop for char in word
                        do(push (nth (position char cipher-alphabet) base-alphabet) newword)
                    )
                    do(push (reverse newword) newdoc)
                    (setf newword '())
                )
            )
            (reverse newdoc)
        )
    )

;;Prints doc
(defun print-doc (doc) 
    (format t "~%~{~{~a~} ~}~%" doc)
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

;;Gen decoder A uses brute force. I tried 1000 times but it can be upgraded. Concept of that function for given encoded document, it creates
;;different ciphers with help of loop and every created cipher decoded with base alphabet. In this time this is normal 26 letter english alphabet.
;;After creating a decoded document with help of paragraph checkers, we can determine whether the this cipher and this decoding is true and we found
;;the right alphabet or not. If it founds, it prints the document on to the screen. 
;;Try numbers can be changed. In this case 1000 is try number.
(defun Gen-Decoder-A (encoded dictionary)
    (let ((is-solution 0))
        (loop for i from 1 to 1000
            do(if (not (eql (check-paragraph-checker-1 (decode (get-cipher i) encoded (get-cipher 0)) dictionary) nil)) 
                (setf is-solution i))
        )
        (if (= is-solution 0) (print "No cipher can decode this paragraph") (print-doc (decode (get-cipher is-solution) encoded (get-cipher 0)))) 
    )      
)

;;Gen decoder B-0 uses brute force but in this time we have some predictions about what mappings look like. Firstly our base alphabet little different from
;;normal english alphabet for counting letter. We know six most frequent letter. Because of that we add them to front of base alphabet and we create ciphers
;;for remaning letters. After that succesfull mapping, we can decode with help of count letters and with any paragraph checker we can determine the document
;;is English or not.
;;Try numbers can be changed. In this case 1000 is try number.
(defun Gen-Decoder-B-0 (encoded dictionary filename)
    (let ((is-solution 0) (cipher) (founded) (alphabet (append '(#\e #\t #\a #\o #\i #\n) (set-difference (get-cipher 0) '(#\e #\t #\a #\o #\i #\n)))))
        (loop for i from 1 to 1000
            do(progn
                (setf cipher (append (get-count-mapping (count-letters-B-0 filename) (get-cipher 0)) (get-cipher-DecB-0 i filename)))
                (if (not (eql (check-paragraph-checker-1 (decode cipher encoded alphabet) dictionary) nil)) (progn (setf is-solution i) (setf founded cipher)))
            )
        )
        (if (= is-solution 0) (print "No cipher can decode this paragraph") (print-doc (decode founded encoded alphabet)))
    )
)

;;Gen decoder B-1 uses brute force but in this time we have some predictions about what mappings look like. Firstly our base alphabet little different from
;;normal english alphabet for counting letter. We know six most frequent letter. Because of that we add them to front of base alphabet and we create ciphers
;;for remaning letters. After that cipher, we count letter that comes after most frequent English letters and we map them according to their occurance numbers
;;in succesful mapping we can see the decoded paragraph. 
;;Try numbers can be changed. In this case 1000 is try number.


(defun Gen-Decoder-B-1 (encoded dictionary filename)
  	(let ((is-solution 0) (cipher) (founded) (alphabet (append '(#\e #\t #\a #\o #\i #\n) (set-difference (get-cipher 0) '(#\e #\t #\a #\o #\i #\n)))))
        (loop for i from 1 to 1000
            do(progn
                (setf cipher (append (get-count-mapping (count-letters-B-1 filename) (get-cipher 0)) (get-cipher-DecB-1 i filename)))
                (if (not (eql (check-paragraph-checker-1 (decode cipher encoded alphabet) dictionary) nil)) (progn (setf is-solution i) (setf founded cipher)))
            )
        )
        (if (= is-solution 0) (print "No cipher can decode this paragraph") (print-doc (decode founded encoded alphabet)))
    )
)

;;Code-Breaker takes a decoder function and according to that function breakes the code and returns the cipher
(defun Code-Breaker (document decoder dictionary filename)
    (if (eql decoder #'Gen-Decoder-B-0) (funcall #'Gen-Decoder-B-0 document dictionary filename))
    (if (eql decoder #'Gen-Decoder-B-1) (funcall #'Gen-Decoder-B-1 document dictionary filename))
    (if (eql decoder #'Gen-Decoder-A) (funcall #'Gen-Decoder-A document dictionary))
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data (filename dictionary)
	(print "....................................................")
	(print "Testing....")
	(print "....................................................~%")
    ;;Since we have different alphabet needs, created different encodition. Cipher number can be changed. Given number creates nth-permutation of alphabet
    (let ((nth-factorial 984))
        (let ((encoded-A (encode (get-cipher nth-factorial) (read-as-list filename) (get-cipher 0))) (encoded-B-0 (encode (append (get-count-mapping (count-letters-B-0 filename) (get-cipher 0)) (get-cipher-DecB-0 nth-factorial filename)) (read-as-list filename)  (append '(#\e #\t #\a #\o #\i #\n) (set-difference (get-cipher 0) '(#\e #\t #\a #\o #\i #\n)))))
              (encoded-B-1 (encode (append (get-count-mapping (count-letters-B-1 filename) (get-cipher 0)) (get-cipher-DecB-1 nth-factorial filename)) (read-as-list filename)  (append '(#\e #\t #\a #\o #\i #\n) (set-difference (get-cipher 0) '(#\e #\t #\a #\o #\i #\n))))))
            (format t "~%")
            (print "Encoded-A is decoding...")
            (print-doc encoded-A)
            (format t "~%")
            (print "Encoded-A is result:")
            (Code-Breaker encoded-A #'Gen-Decoder-A dictionary filename)
            (format t "~%")
            (print "Encoded-B-0 is decoding...")
            (print-doc encoded-B-0)
            (format t "~%")
            (print "Encoded-B-0 is result:")
            (Code-Breaker encoded-B-0 #'Gen-Decoder-B-0 dictionary filename)
            (format t "~%")
            (print "Encoded-B-1 is decoding...")
            (print-doc encoded-B-1)
            (format t "~%")
            (print "Encoded-B-1 is result:")
            (Code-Breaker encoded-B-1 #'Gen-Decoder-B-1 dictionary filename)
            (format t "~%")
        )
    )
)


;; test code...
(test_on_test_data "myDocument.txt" "myDictionary.txt")