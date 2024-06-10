;; Oblig 3b av sameera

(load "prekode3a.scm")



;; Oppgave 1

;; a og b
(define (memoize f)
  (let ((table (make-table)))
    (lambda x
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (apply f x)))
              (insert! x result table)
              result))))))

(define mem
  (let ((proc-table (make-table)))
    (lambda (message proc)
      
      (define (unmemoize proc)
        (or (lookup proc proc-table) proc))
    
      (cond ((and (eq? message 'memoize)
                  (not (lookup proc proc-table)))
             (let ((memoized-proc (memoize proc)))
               (insert! memoized-proc proc proc-table)
               memoized-proc))
            ((eq? message 'unmemoize) (unmemoize proc))
            (else proc)))))


(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(set! fib (mem 'unmemoize fib))
"hei"
(fib 3)
(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)


;; 1c

;; forskjellen er er at memoiseringsprosedyren bare husker resultatet for det øyeblikkelige kallet,
;; som for eksempel (fib 3), men den husker ikke de mellomliggende resultatene som trengs for å beregne det endelige svaret.
;; Dette fører til at memoiseringsprosedyren ikke bygger opp en fullstendig og gjenbrukbar tabell med mellomliggende resultater.




;; Oppgave 2
;; 2a
(define (list-to-stream list)
  (cons-stream (car list) (list-to-stream (cdr list)))) ;; first element is car of list rest is cons-stream of list

(define (stream-to-list stream . nr-elems)
  (if (null? nr-elems)
      (if (null? stream) '()
          (cons (stream-car stream) (stream-to-list (stream-cdr stream))))
      (if (> (car nr-elems) 0) (cons (stream-car stream) (stream-to-list (stream-cdr stream) (- (car nr-elems) 1))) '())
  )
      )
  


(stream-cdr(stream-cdr(list-to-stream '(1 2 3 4 5))))
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

;; 2b
(define (stream-take n stream)
  (if (> n 0) (cons-stream (stream-car stream) (stream-take (- n 1) (stream-cdr stream))) '()))

(define foo (stream-take 10 nats))
foo
(show-stream foo 5)
(show-stream foo 20)
(show-stream (stream-take 15 nats) 10)

;; 2c
;; Et potensielt problem som kan oppstå med forslaget til "Petter Smart" er at uendelige strømmer kan skapen uendelige løkker.
;; Dersom memq kalles med en uendelig strøm og et element som ikke finnes i strømen vil den aldri finne neste element og aldri terminere.

;; 2d

;; To make the predicate we define a procedure that takes a element and keeps an internal list,
;; It loops through the list to check if the element has already bee given, if so return #f, otherwise return the element and add it to the list
(define (make-predicate)
  (let ((element-list '())) 
    (define (predicate item)
      (display element-list)
      (if (memq item element-list) 
          #f
          (begin
            (set! element-list (cons item element-list))
                  (display element-list)
            item))) 
    predicate))

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter (make-predicate) (stream-cdr stream))))))



(stream-cdr(stream-cdr(stream-cdr(remove-duplicates (list-to-stream '(1 2 3 2 5))))))


