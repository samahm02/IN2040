;Oblig 1a, Sameera;

"Oppgave 1"

;;A
(* (+ 4 2) 5)
;; Ingen feil og svaret blir 30.
;;Man legger sammem det i det innerste parantesen og ganger det med 6.

;;B
(* (+ 4 2) (5))
;; Her får vi syntax feil
;;siden den forventer et prosedyre men får bare paramenteren 5.

;;C
(* (4 + 2) 5)
;; Her blir det syntax feil
;; Prosedyren skal stå mellom parameterene og ikke mellom

;;D
(define bar (/ 44 2))
  bar
;; Ingen feil og svaret blir 22.
;; Her blir bar defninert som deler 44 på 2.
;; til slutt kaller vi prosedyren med navnet bar

;;E
(- bar 11)
;; Ingen feil og svaret blir 11.
;; Her blir prosedyren bar fra 1D brukt og tekker fra 11.

;;F
(/(* bar 3 4 1) bar)
;; Ingen feil og svaret blir 12.
;; Her bruker vi prosedyren bar og ganger det med 3, 4 og 1.
;; Da får vi 264 som vi deler på 22, som vlir 12.

"Oppgave 2"
;;A
;; Disse er special forms ved at de slipper å evaluere alle utrykkene
;; Dette ser vi under

(or (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
;; Dette første uttrykket blir til "paff!"
;; Dette skjer siden OR vil finne det første som er True
;; Da vil den returnere svaret og vi slipper å evaluere (1 -1) som har syntax feil


(and (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
;; Det andre uttrykket blir til #f
;; Dette skjer siden AND vil gi oss false om en av argumentene er false.
;; Det samme vil skje som i det første utrykket, at vi slipper å evaluere (1-1)

(if (positive? 42)
    "poff!"
    (i-am-undefined))
;; Det siste utrykket blir til "poff!"
;; Det som skjer er at vi skjekker om 42 er positivt og siden dette stemmer så får vi poff
;; Siden 42 var positivt så blir ikke udefinerte prosedyren evaluert


;;B
;; ser først om tallet er mindre enn null
;; hvis ikke så ser vi om det er større enn null, hvis ikke så må det være 0
(define (sign tall)
  (if (< tall 0) -1
      (if (> tall 0) 1 0)))


;; Gjør det samme her bare ved å bruke cond
;; ser om det er mindre en null, så større en null og til slutt må det være null
(define (sign tall)
  (cond ((< tall 0) -1)
  ((> tall 0) 1)
  (else 0)))


;;C
;; vi bruker OR siden den returneren det første som blir true
;; vi ser først om tallet er mindre enn null,
;; hvis det er mindre så blir den til -1, ellers så blir det til false
;; samme skjer med den andre AND, men er ser vi om at det er større enn null
;; Hvis ingen av disse blir til True så returneres 0
(define (sign tall)
  (or
   (and (< tall 0) -1)
   (and (> tall 0) 1)
   0))


"Oppgave 3"
;;A

(define (add1 tall)
  (+ tall 1))

(define (sub1 tall)
  (- tall 1))

;;B
(define (pluss tall1 tall2)
  (cond((zero? tall1) tall2)
       ((zero? tall2) tall1)
       (else (pluss (add1 tall1) (sub1 tall2) ))))

;;C
;; Prosedyren er rekursiv ved at den kaller på seg selv, men den har en iterativ prosess
;; både tall1 og tall2 endres i hvert trinn av rekursjonen, og det er ingen ventende funksjonskall som må behandles senere
;; vi kan visualisere prosedyren slik;
;;(pluss 2 2)

;;(pluss 3 1)

;;(pluss 4 0)

;; en ligende variant med rekursiv prosess er:
(define (pluss_rekursiv tall1 tall2)
  (cond((zero? tall1) tall2)
       ((zero? tall2) tall1)
       (else (add1 (pluss tall1 (sub1 tall2)) ))))

;; Her får vi ventende kall som dette:
;;(add1 (pluss_rekursiv 2 1))

;;(add1 (add1 (pluss_rekursiv 2 0)))

;;(add1 (add1 2))

;;(add1 3)



;;D

;; Vi kan ta vekk paramenterene b og n siden vi allerde har de tilgjengelig
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
          e
          (power-iter (+ 1 e))))
  (power-iter 1))


;;E
(define (fib n)
  (define (fib-iter a b N)
    (if (= N 0)
        b
        (fib-iter (+ a b) a (- N 1))))
  (fib-iter 1 0 n))
;; Vi kan ikke ta bort paramenteren N siden den ender verdi og sendes videre rekursivt