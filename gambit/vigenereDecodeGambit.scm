(import
    (scheme base)
    (scheme write)  
    (srfi 193)   
    (srfi 8)
    (srfi 133)
    (scheme vector)   
)

(define ciphertext (string->list (car (command-args) )))

(define key (car (cdr (command-args))))

(define table #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
                #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\_ #\-))

(define keylen (string-length key))

(define tablelen (vector-length table))

(define (getcharpos chr)
    (vector-index (lambda (x) (char=? chr x)) table))

(define (decodedchar cphrchar keychar)
    (let ((ctextpos (getcharpos cphrchar))(keypos (getcharpos keychar)))  
        (if (and (integer? ctextpos)(integer? keypos))
            (vector-ref table (if (> keypos ctextpos)  (+ tablelen (- ctextpos keypos) ) (- ctextpos keypos))) 
            cphrchar)))

(define getkeychar
   (let ((idx -1))
           (lambda ()
              (if (= idx (- keylen 1))
                (set! idx 0)
                (set! idx (+ idx 1)))
            (string-ref key idx))))
(define (main args)
(let loop ((cphrtxt ciphertext)(result '()))
    (if (not (equal? cphrtxt '()))        
            (loop (cdr cphrtxt) (cons (decodedchar (car cphrtxt) (getkeychar))  result))
            (display (list->string(reverse result))))))
