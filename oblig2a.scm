(load "huffman.scm")


;; Oppgave 1
;; a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))


(define my-pair (p-cons 1 2))
(p-car my-pair) ; This will display 1
(p-cdr my-pair) ; This will display 2
(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

;; b

(define foo 42)

((lambda (foo x)
   (if (= x foo) 'same 'different))
 5 42) ;; Evaluerer til different


((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel) ;; Evaluerer til (towel (42 towel))

;; c

(define (infix-eval lst)
  (apply (cadr lst) (list (car lst) (caddr lst))))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

;; d

(define bah '(84 / 2))
;;(infix-eval bah) ;;Gir en feilmelding ettersom / ikke evalueres til prosedyren bundet til /-symbolet

;; Oppgave 2

;; a

(define (decode2 bits tree)
  (define (decode-1 bits current-branch acc)
    (if (null? bits)
        (reverse acc)
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) acc))
              (decode-1 (cdr bits) next-branch acc)))))
  (decode-1 bits tree '()))


;; b

(decode2 sample-code sample-tree) ;; Output: (samurais fight ninjas by night)

;; c

(define (encode symbols tree)
  ; Hjelpeprosedyre for å finne en kode for et symbol i treet
  (define (find-code symbol current-branch code)
    (cond
      ((null? current-branch)
       '())
      ((leaf? current-branch)
       (if (eq? symbol (symbol-leaf current-branch))
           code
           '()))
      (else
       (let ((left-code (find-code symbol (left-branch current-branch) (append code '(0))))
             (right-code (find-code symbol (right-branch current-branch) (append code '(1)))))
         (if (not (null? left-code))
             left-code
             right-code)))))

  ; Hovedprosedyren for å kode meldingen
  (define (encode-message message)
    (if (null? message)
        '()
        (let ((symbol (car message))
              (rest (cdr message)))
          (let ((code (find-code symbol tree '())))
            (if (null? code)
                '()
                (append code (encode-message rest)))))))

  (encode-message symbols))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)



;; d

(define (grow-huffman-tree freqs)
  (define (grow-huffman-tree-inner leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (let* ((left (car leaf-set))
               (right (cadr leaf-set))
               (combined (adjoin-set (make-code-tree left right)
                                     (cddr leaf-set))))
          (grow-huffman-tree-inner combined))))
    
  (grow-huffman-tree-inner (make-leaf-set freqs)))

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)


;; e

(define freqs '((samurais 57) (ninjas 20) (fight 45) (night 12)
                              (hide 3) (in 2) (ambush 2) (defeat 1) (the 5)
                              (sword 4) (by 12) (assassin 1) (river 2)
                              (forest 1) (wait 1) (poison 1)))

(define huffman-tree (grow-huffman-tree freqs))

(encode '(ninjas fight
                 ninjas fight ninjas
                 ninjas fight samurais
                 samurais fight
                 samurais fight ninjas
                 ninjas fight by night) huffman-tree)
;;Den bruker 43 bits til å kode meldingen

;; 43 /17 er ca 2,5 bit per kodeord

;; log2(17) er ca 4, da vil ta 4 bits å kode meldingen i fastlengdekode


;; f
