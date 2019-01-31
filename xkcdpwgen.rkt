#!/usr/bin/env racket 

#lang racket


; **skeleton taken from jackwarren.info tutorial**

; PROBLEM: create a program that lets users create passphrases based on english words
; the default setting is 4 english words concatenated together
; however, the user may specify the number of words
; the user may also specify how many words' first letters are capitalized
; the user may also choose to include numbers, and the number of them
; the user may also choose to include special characters, and the number of them

; param1 is a Number, the number of words, with a default value of 4
(define param1 (make-parameter 4))

; param2 is a Number, the number of caps, with a default value of 0
(define param2 (make-parameter 0))

; param3 is a Number, the number of inserted numbers, with a default value of 0
(define param3 (make-parameter 0))

; param4 is a Number, the number of inserted symbols, with a default value of 0
(define param4 (make-parameter 0))

(require racket/runtime-path)
(define-runtime-path MY-RAW-PATH "mediumwords.txt")
(define FILE (file->list MY-RAW-PATH))


(define parser
  (command-line
   #:usage-help 
   "This program is designed to generate random passphrases in accordance with the xkcd method."
   "Generate a secure, memorable password using the XKCD method
                
optional arguments:
    -h, --help            show this help message and exit
    -w WORDS, --words WORDS
                          include WORDS words in the password (default=4)
    -c CAPS, --caps CAPS  capitalize the first letter of CAPS random words
                          (default=0)
    -n NUMBERS, --numbers NUMBERS
                          insert NUMBERS random numbers in the password
                          (default=0)
    -s SYMBOLS, --symbols SYMBOLS
                          insert SYMBOLS random symbols in the password
                          (default=0)
"

   #:once-each
   [("-w" "--words") WORDS
    "include WORDS words in the password (default=4)"
    (param1 (string->number WORDS))]
   [("-c" "--caps") CAPS 
    "capitalize the first letter of CAPS random words (default=0)"
    (param2 (string->number CAPS))]
   [("-n" "--numbers") NUMBERS 
    "insert NUMBERS random numbers in the password (default=0)"
    (param3 (string->number NUMBERS))]
   [("-s" "--symbols") SYMBOLS 
    "insert SYMBOLS random symbols in the password (default=0)"
    (param4 (string->number SYMBOLS))]
  
   #:args () (void)))


; taken from racket documentation
(define (capitalize-first-letter str)
  (regexp-replace #rx"^." str string-upcase))


; extract : Number [Listof Symbol] -> String
; takes in n randomly generated Strings and adds them together to make one long String
; helper for words

(define (extract x f)
  (local (; ext-helper : accumulates the recursion of the index until it equals the random number
          ; extracts element correspnding to nth word in given list
          (define (ext-helper x f acc)
            (if (= acc x) (symbol->string (first f))
                (ext-helper x  (rest f) (+ acc 1)))))
    (ext-helper x FILE 0)))

; words : Number Number Number Number [Listof Symbol] -> [Listof String]
; takes in 4 randomly generated Strings and adds them together to make one long String

(define (words w c n s f)
  (local (; words: Number [Listof Symbol] Number Number [Listof String]
          ; Takes in the given # of words, file, random number, and list of accumulating strings
          (define (words-h w f x addr strung)
            (if (= w addr)
                (cons (extract x f) strung)
                (words-h w f (random 4000) (+ 1 addr) (cons (extract x f) strung)))))
    (words-h w FILE (random 4000) 1 '())))

; caps : Number Number Number Number -> [Listof String]
; takes in the number of capital letters desired and randomly capitalizes the first letter of a word

(define (caps w c n s)
  (shuffle (local (; takes in # capital letters desired, los, index for capitals, and accumulated los
          (define (caplos las i)
            (list-update las i capitalize-first-letter))

          (define (caps-hh c i accu)
            (if (= i c) accu (caps-hh c (+ 1 i) (caplos accu i)))))
    (caps-hh c 0 (words w c n s FILE)))))
    
; numbers : Number Number Number Number -> [Listof String]
; takes in a number of the desired Numbers to be appended to the end, beginning, or in between words

(define (numbers w c n s)
  (shuffle (local (; takes in a word and half of half the time, puts it to the beginning of the
                   ; word, other half the half appends it to the end, and other actual half appends
                   ; it to a random place in the middle of the word
                   ; numberize-help : String -> String
                   (define (numberize-help w)
                     (cond
                       [(and (> (random 4000) 2000) (even? (random 4000)))
                        (string-append (number->string (random 9)) w)]
                       [(and (> (random 4000) 2000) (odd? (random 4000)))
                        (string-append w (number->string (random 9)))]
                       [else (local (; locally define a random number to be used as an index
                                     ; to append a different random number inside the word
                                     (define rando-index (random (- (string-length w) 1))))

                               (string-append (substring w 0 rando-index)
                                              (number->string (random 9))
                                              (substring w rando-index (string-length w))))]))

                   ; numberize : [Listof String] Number -> [Listof String]
                   ; takes in the accumulated list of strings and the accumulating number
                   ; of how many numbers have been added to a string in the los
                   (define (numberize las i)
                     (list-update las i numberize-help)) 

                   ; Number Number [Listof String] -> [Listof String]
                   ; takes in the accumulated list of strings and the accumulating number
                   ; of how many numbers have been added to a string in the los
                   (define (num-help n i accu)
                     (if (= i n) accu (num-help n (+ i 1) (numberize accu i)))))
             (num-help n 0 (caps w c n s)))))
                   
; symbols : Number Number Number Number -> [Listof String]
; takes in a number of the desired Symbols to be appended to the end, beginning, or in between words
; helpers defined locally follow same theme as numbers function and associated helpers

(define (symbols w c n s)
  (shuffle (local (
                   (define (symbolize-help w)
                     (cond
                       [(and (> (random 4000) 3000) (even? (random 4000)))
                        (string-append (list-ref '("!" "@" "#" "$" "%" "^" "&" "*" "?" "." "+") (random 9)) w)]
                       [(and (> (random 4000) 3000) (odd? (random 4000)))
                        (string-append w (list-ref '("!" "@" "#" "$" "%" "^" "&" "*" "?" "." "+") (random 9)))]
                       [else (local (
                                     (define rando-index (random (- (string-length w) 1))))
                               (string-append (substring w 0 rando-index)
                                              (list-ref '("!" "@" "#" "$" "%" "^" "&" "*" "?" "." "+") (random 9))
                                              (substring w rando-index (string-length w))))]))
                     
                   (define (symbolize las i)
                     (list-update las i symbolize-help)) 
                   
                   (define (symb-help n i accu)
                     (if (= i n) accu (symb-help n (+ i 1) (symbolize accu i)))))
             (symb-help s 0 (numbers w c n s)))))

; main : param1 param2 param3 param4 -> String
; takes in the number of english words to be concatenated, the number of capital letters
; to be randomly assigned to the first letter of a word, the number of numbers to be randomly
; inserted into the passphrase, and the number of special characters to be randomly inserted
; and concatenates the final list of strings into a passphrase as the output for xkcdpwgen
 
(define (main w c n s)
  (foldr string-append "" (symbols w c n s)))


; to print the passphrase in the command line 
(printf "~a~a\n" "Your new passcode is " (main (param1) (param2) (param3) (param4)))