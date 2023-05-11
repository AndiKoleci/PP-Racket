#lang racket

(provide (all-defined-out))
;Koleci Alexandru, 323CD

;; Dacă ne interesează doar al n-lea TPP din arbore, este
;; convenabil să determinăm secvența de transformări care
;; conduce la acest TPP, așa cum am procedat până acum.
;;
;; În schimb, dacă ne interesează primele n TPP (sau în
;; general o secvență mai lungă de TPP) ar fi de preferat
;; crearea unui flux infinit care să le conțină pe toate
;; în ordine.
;;
;; Observăm că această ordine corespunde unei parcurgeri
;; BFS a arborelui infinit. Acesta este un BFS mai simplu
;; decât BFS-ul uzual
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; întrucât succesorii unui TPP sunt automat triplete noi,
;; deci nu este necesar să verificăm dacă un nod a mai
;; fost sau nu vizitat.
;; 
;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (apply + (map * X Y))
  )

(define (multiply M V)
  (foldl (λ (x acc) (append acc (list (dot-product x V)))) null M)
  )


; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.

(define (first ppt)
  (multiply T1 ppt))

(define (second ppt)
  (multiply T2 ppt))

(define (third ppt)
  (multiply T3 ppt))

(define ppt-stream-in-tree-order
  (let loop ((Q '((3 4 5))))
    (stream-cons (car Q) (loop (append (cdr Q) (list (first (car Q)) (second (car Q)) (third (car Q))))))
    )
  )


;; Un alt mod de a genera TPP se folosește de perechi (g, h)
;; care indeplinesc condițiile:
;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;; Nu întâmplător am ales aceste notații, teoria este aceeași
;; cu cea din spatele cvartetelor (g, e, f, h), pe care le
;; putem exprima și ca (g, (h-g)/2, (h+g)/2, h).
;;
;; Pentru a obține un TPP dintr-o pereche (g, h) se aplică
;; aceleași formule (dar le vom exprima în funcție de g și h):
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;; Acest mod de generare ne furnizează TPP în altă ordine
;; decât cea dată de parcurgerea în lățime a arborelui TPP.
;;
;; Noua ordine se obține parcurgând pe coloane diagrama:
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;; (lipsește perechea (3,9), 3 și 9 nefiind prime între ele)
;;
;; Folosind această indexare, primele 6 TPP sunt:
;;    (3,4,5)                           - din perechea (1,3)
;;    (5,12,13), (15,8,17)              - din (1,5), (3,5)
;;    (7,24,25), (21,20,29), (35,12,37) - din (1,7), (3,7), (5,7)
;;
;; Ne propunem să definim fluxul infinit de TPP în ordinea de
;; mai sus. Acesta se bazează pe fluxul corespunzător de 
;; perechi (g, h), pe care îl generăm astfel:
;;  - pornim cu 2 fluxuri infinite:
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (întrucât g < h)
;;  - fluxul ordonat pe coloane va conține:
;;    * perechea compusă din cele mai mici numere din G și H
;;      (ex: (1,3))
;;    * apoi interclasarea (conform ordinii "pe coloane") între:
;;      - perechile compuse dintre minimul din G și restul din H
;;        (ex: (1,5), (1,7), (1,9) ...)
;;      - fluxul ordonat generat de restul lui G și restul lui H
;;        (ex: (3,5), (3,7), (5,7) ...)
;; Aceasta este abordarea generală, în urma căreia generăm toate
;; perechile, inclusiv pe cele de numere care nu sunt prime  
;; între ele. Perechile neconforme trebuie înlăturate ulterior
;; (utilizând funcția de bibliotecă gcd).


; TODO
; Definiți o funcție care primește 2 fluxuri numerice infinite
; G și H, și generează fluxul de perechi de câte un element 
; din G și unul din H ordonate conform metodei de mai sus.
; Condițiile ca g și h să fie impare, prime între ele, respectiv
; menținerea restricției g < h (cât timp urmați algoritmul) nu
; trebuie impuse în implementarea funcției pairs.
; Ele vor fi asigurate de definirea fluxurilor de mai jos prin:
;  - apelarea lui pairs exclusiv pe fluxurile
;    G = 1, 3, 5, 7 ... și H = 3, 5, 7, 9 ...
;  - eliminarea perechilor de numere neprime între ele (care 
;    există în rezultatul funcției pairs, dar nu vor mai exista
;    în fluxul gh-pairs-stream)
(define (pairs G H)
  'your-code-here)


; TODO
; Definiți fluxul de perechi (g, h) pe care se bazează noua
; indexare a TPP.
; Nu folosiți recursivitate explicită.
(define gh-pairs-stream
  'your-code-here)


; TODO
; Definiți fluxul de TPP corespunzător fluxului anterior de
; perechi (g, h).
(define ppt-stream-in-pair-order
  'your-code-here)


