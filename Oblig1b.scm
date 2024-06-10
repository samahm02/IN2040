;; Sameera oblig 1b

"Oppgave1"

;;f

(car(cdr '(0 42 #t bar)))

;;g
(car(cdr(car '((0 42) (#t bar)))))

;;h
(car(car(cdr '((0) (42 #t) (bar)))))


;;i

;; cons:
(cons (cons 0 (cons 42 '())) (cons( cons #t (cons 'bar '())) '()))

;; list:
(list (list 0 42) (list #t 'bar))

"Oppgave2"

;;a
(define (take n items)
  (cond ((<(length items) n) items)
        ((null? items) '())
        ((= n 1) (cons(car items) '()))
        (else
         (cons (car items) (take (- n 1) (cdr items))))))
;;b
(define (take n items)
  (define (tail-take m items)
    (cond ((null? items) '())
    ((<(length items) n) items)
    ((= (- n 1) m) (cons (car items) '()))
    (else
         (cons (car items) (tail-take (+ m 1) (cdr items))))))
  (tail-take 0 items))

;;c
(define (take-while pred items)
  (if (pred (car items))
      (cons (car items) (take-while pred (cdr items)))
      '()))


;;d
(define (map2 pred l1 l2)
  (cond ((null? l1) '()) 
        ((null? l2) '())
        (else
         (cons (pred (car l1) (car l1)) (map2 pred (cdr l1) (cdr l2))))))


;;e
(map2 (lambda(x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))