;; Oblig 3b av sameera

(load "evaluator.scm")

(set! the-global-environment (setup-environment))
(read-eval-print-loop)

;;1
; (a)
;; (foo 2 square) -> Prosedyren evalueres til 0. Vi får null ettersom at (= cond 2) evalueres til #t og ((= cond 2) 0) gir 0.
;;Vi ser også at evaluatoren benytter to definisjoner av cond.
;; (cond ((= cond 2) 0) den første referansen av cond vil løses opp til funksjonsdefinisjonen i den globale omgivelsen;
;; dette skyldes at cond syntaktisk står i posisjon til en prosedyre.
;; Deb andre referansen til cond, (= cond 2), vil evalueres til en definisjonen internt i prosedyren og evalueres til 2.  

;; (foo 4 square) -> evalueres til 16. Her er ikke cond argumentet 2, dermed vil ikke 0 returneres.
;; Istedenfor vil else, evlauert som en special formen i relasjon til cond, sørge for at (else cond) blir evaluert.
;; Deretter vil else evalueres til den nærmeste definerte prosedyren som er den intert definerte prosedyren fra de aktuelle parameterene, square, og cond bli evaluert til 4.
;; square av 4 gir alltid 16. 

;;(cond ((= cond 2) 0) 
;;    (else (else 4))) -> evalueres til 2

;; (cond ((= cond 2) 0) -> første cond evalueres til den globalt definerte special formen. Den andre defineres til den global definerte verdien som evalueres til 3.
;; Dermed evalueres (= cond 2) til #f og else grenen evalueres.
;; else-grenen evalueres til (else 4) som vil evaluere else til (define (else x) (/ x 2)), tidligere definert i det globale skopet, som er en normal order procedure som gir oss 


;; oppgave 2 a

;; lagt til prosedyre for 1+ og 1- i primitive-procedures i evaluator.scm som vist under:

;;(define primitive-procedures
;;  (list (list 'car car)
;;        (list 'cdr cdr)
;;        (list 'cons cons)
;;        (list 'null? null?)
;;        (list 'not not)
;;        (list '+ +)
;;        (list '- -)
;;        (list '* *)
;;        (list '/ /)
;;        (list '= =)
;;        (list 'eq? eq?)
;;        (list 'equal? equal?)
;;        (list 'display 
;;              (lambda (x) (display x) 'ok))
;;        (list 'newline 
;;              (lambda () (newline) 'ok))
;;      her kan vi legge til flere primitiver.
        ;; Opphave 2 a, endret under:
;;        (list '1+
;;              (lambda (x) (+ x 1))) ;;prosedyre for pluss
;;        (list '1-
;;              (lambda (x) (- x 1))) ;;prosedyre for minus
        ;; endret over dette
        ;;))


;; oppgave 2b

(define (install-primitive! name proc)
  (define-variable! name (list 'primitive proc) the-global-environment))

(install-primitive! 'square (lambda (x) (* x x)))

;; testes ved å kalle på i (read-eval-print-loop)

;; oppgave 3a

;;lagt til følgdende i evaluator.scm:
;oppgave 3a under:
(define (eval-and exp env)
  (cond ((last-exp? exp)                
         (mc-eval(car exp) env))
        ((false?                            
          (mc-eval (car exp) env)) #f) 
        (else (eval-and (cdr exp) env)))) 


(define (eval-or exp env)
  (cond ((last-exp? exp)                    
         (mc-eval(car exp) env))
        ((true?                             
          (mc-eval (car exp) env)) #t) 
        (else (eval-or (cdr exp) env))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

;;bare endret der det er kommentar:

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and (cdr exp) env)) ;; oppgave 3a and
        ((or? exp) (eval-or (cdr exp) env)))) ;; oppgave 3a or

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; oppgave 3a and
        ((or? exp) #t)  ;; oppgave 3a or
        (else #f)))


;; Oppgave 3b:

;; opptatert tidligere eval-if 
(define (eval-if exp env)
  (cond ((else? exp)
         (mc-eval (if-alternative exp) env))
        ((and (true? (mc-eval (if-predicate exp) env))
              (then? exp))
         (mc-eval (if-consequent exp) env))
        ((or (elsif? (elsif-part exp))
             (else? (elsif-part exp)))
         (eval-if (elsif-part exp) env))
        (else (display 'FAILED))))

(define (if-consequent exp) (cadddr exp)) ;;OPPGAVE 3b endret på fra caddr til cadddr

(define (if-alternative exp) (cadr exp)) ;; OPPGAVE 3b endret på

(define (elsif-part exp) (cddddr exp)) ;; OPPGAVE 3b lagt til prosedyre

;; oppgave 3b:
;; nye proserdyrer for nøkkelordene
(define (then? exp)
  (eq? 'then (caddr exp)))

(define (elsif? exp)
  (eq? 'elsif (car exp)))

(define (else? exp)
  (eq? 'else (car exp)))

;; oppgave 3c
;; Lagt til: "((let? exp) (eval-let (cdr exp) env)))) ;; oppgave 3c" i (define (eval-special-form exp env)

;; Lagt til: "((let? exp) #t) ;; oppgave 3c" i (define (special-form? exp)

;; Lagt til metoden for å sjekke referanse via tag;
(define (let? exp) ;; oppgave 3c
  (tagged-list? exp 'let))

;; Lagt til evaluerings metode for let, benytter seg av make-lambda for å lage lambda utrykket:
(define (eval-let exp env) ;; oppgave 3c
  (let ((params (car exp))
        (body (cdr exp)))
    (mc-eval 
     (cons (make-lambda (map car params) body) 
           (map cadr params))
     env)))

;; oppgave 3d - ikke implementert 

