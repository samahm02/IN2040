;; Oblig 2b av sameera


;; oppgave 1

;; a
(define (make-counter)
  (let ((count 0))
    (lambda()
    (set! count(+ count 1))
    count)))


(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1) ;1
(c1) ; 2
(c1) ; 3
count ;42
(c2) ; 1


;; oppgave 2

;; a

(define (make-stack initial-elements)
  (define stack (if (list? initial-elements) initial-elements '()))

  (define (push! . elements)
    (set! stack (append (reverse elements) stack)))

  (define (pop!)
    (if (null? stack)
        '() ; Returner en tom liste hvis stakken er tom
          (set! stack (cdr stack))))

  (define (get-stack)
    stack)

  (lambda (message . args)
    (cond
      ((eq? message 'push!) (apply push! args))
      ((eq? message 'pop!) (pop!))
      ((eq? message 'stack) (get-stack))
      (else (error "Ukjent melding:" message)))))

; Test eksempler:
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

(s1 'pop!) ; Skal ikke gi feilmelding, returnerer '()
(s1 'stack) ; Skal returnere '(bar)

(s2 'pop!) ; Skal ikke gi feilmelding, returnerer '()
(s2 'push! 1 2 3 4)
(s2 'stack) ; Skal returnere '(4 3 2 1)

(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) ; Skal returnere '(baz zip zap bah bar)
(s1 'pop!)
(s1 'stack)


;b

(define (push! stack . elements)
  (apply (stack 'push!) elements))

(define (pop! stack)
  (stack 'pop!))

(define (stack stack)
  (stack 'stack))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)


;; oppgave 3
