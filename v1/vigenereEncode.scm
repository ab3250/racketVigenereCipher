#!racket

(require racket/vector)

(define plaintext (string->list (vector-ref (current-command-line-arguments) 0)))

(define key (vector-ref (current-command-line-arguments) 1))

(define table #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
                #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\_ #\-))


(define keylen (string-length key))

(define tablelen (vector-length table))

(define (getcharpos chr)
    (vector-member chr table))

(define (newchar ptxtchar keychar)

    (let ((ptextpos (getcharpos ptxtchar))(keypos (getcharpos keychar)))
    (if  (and (integer? ptextpos)(integer? keypos))
    (vector-ref table (modulo (+ ptextpos keypos ) tablelen ))
    ptxtchar)))

(define getkeychar
   (let ((idx -1))
           (lambda ()
              (if (= idx (- keylen 1))
                (set! idx 0)
                (set! idx (+ idx 1)))
            (string-ref key idx))))

(let loop ((plntxt plaintext)(result '()))
    (if (not (equal? plntxt '()))        
            (loop (cdr plntxt) (cons (newchar (car plntxt) (getkeychar))  result))
            (display (list->string(reverse result)))))
