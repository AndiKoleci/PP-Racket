#lang racket

(require "ppt.rkt")

; ignorați următoarele linii de cod...
(define show-defaults 999) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define nopoints #f) (define name-ex '(testul testele trecut capitolul))
(define default-results `(#f 0 () your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul gvn msg expcd))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn "" (or r "nu îndeplinește condiția"))))))
(define (check-part part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check-part '- 1 given main-test expected conds))
(define the cons) (define is (cons equal? "diferă de cel așteptat")) (define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-unique (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))

(sunt 4 exerciții)

(define naturals
  (let nat ([seed 0])
    (stream-cons seed (nat (add1 seed)))))

(define squares
  (stream-map sqr naturals))

(define powers-of-2
  (stream-cons
   1
   (stream-map (lambda (x) (* 2 x)) powers-of-2)))

(exercițiul 1 : 40 puncte)
(when (not (equal? ppt-stream-in-tree-order 'your-code-here))
  (check-part 'a (/ 1 4) (stream->list (stream-take ppt-stream-in-tree-order 20)) is
              '((3 4 5) (15 8 17) (21 20 29) (5 12 13) (35 12 37) (65 72 97) (33 56 65) (77 36 85)
                (119 120 169) (39 80 89) (45 28 53) (55 48 73) (7 24 25) (63 16 65) (133 156 205)
                (85 132 157) (273 136 305) (403 396 565) (115 252 277) (209 120 241)))
  (check-part 'b (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-tree-order 100) 10)) is
              '((627 364 725) (817 744 1105) (145 408 433) (391 120 409) (765 868 1157)
                (429 700 821) (1161 560 1289) (1755 1748 2477) (539 1140 1261) (777 464 905)))
  (check-part 'c (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-tree-order 211) 8)) is
              '((3055 1008 3217) (5781 6460 8669) (3045 5092 5933) (7521 3560 8321)
                (11523 11564 16325) (3683 7644 8485) (4641 2840 5441) (5763 5084 7685)))
  (check-part 'd (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-tree-order 2022) 4)) is
              '((69361 114840 134161) (177885 84868 197093) (271015 271128 383353) (85207 178224 197545)))
  )

(exercițiul 2 : 40 puncte)
(when (stream? (pairs naturals naturals))
  (check-part 'a (/ 1 4) (stream->list (stream-take (pairs naturals (stream-rest naturals)) 10)) is
              '((0 . 1) (0 . 2) (1 . 2) (0 . 3) (1 . 3) (2 . 3) (0 . 4) (1 . 4) (2 . 4) (3 . 4)))
  (check-part 'b (/ 1 4) (stream->list (stream-take (pairs squares (stream-rest squares)) 12)) is
              '((0 . 1) (0 . 4) (1 . 4) (0 . 9) (1 . 9) (4 . 9) (0 . 16) (1 . 16) (4 . 16) (9 . 16) (0 . 25) (1 . 25)))
  (check-part 'c (/ 1 4) (stream->list (stream-take (stream-tail (pairs powers-of-2 (stream-rest powers-of-2)) 5) 8)) is
              '((4 . 8) (1 . 16) (2 . 16) (4 . 16) (8 . 16) (1 . 32) (2 . 32) (4 . 32)))
  (check-part 'd (/ 1 4)
              (stream->list
               (stream-take
                (stream-tail
                 (pairs (stream-map (compose ceiling sqrt) powers-of-2)
                        (stream-rest (stream-map (compose ceiling sqrt) powers-of-2))) 10) 5)) is
              '((1 . 6.0) (2.0 . 6.0) (2 . 6.0) (3.0 . 6.0) (4 . 6.0)))
  )

(exercițiul 3 : 20 puncte)
(when (not (equal? gh-pairs-stream 'your-code-here))
  (check-part 'a (/ 1 4) (stream->list (stream-take gh-pairs-stream 20)) is
              '((1 . 3) (1 . 5) (3 . 5) (1 . 7) (3 . 7) (5 . 7) (1 . 9) (5 . 9) (7 . 9) (1 . 11) (3 . 11)
                (5 . 11) (7 . 11) (9 . 11) (1 . 13) (3 . 13) (5 . 13) (7 . 13) (9 . 13) (11 . 13)))
  (check-part 'b (/ 1 4) (stream->list (stream-take (stream-tail gh-pairs-stream 100) 10)) is
              '((19 . 31) (21 . 31) (23 . 31) (25 . 31) (27 . 31) (29 . 31) (1 . 33) (5 . 33) (7 . 33) (13 . 33)))
  (check-part 'c (/ 1 4) (stream->list (stream-take (stream-tail gh-pairs-stream 211) 8)) is
              '((1 . 47) (3 . 47) (5 . 47) (7 . 47) (9 . 47) (11 . 47) (13 . 47) (15 . 47)))
  (check-part 'd (/ 1 4) (stream->list (stream-take (stream-tail gh-pairs-stream 2022) 4)) is
              '((79 . 141) (83 . 141) (85 . 141) (89 . 141)))
  )

(exercițiul 4 : 20 puncte)
(when (not (equal? ppt-stream-in-pair-order 'your-code-here))
  (check-part 'a (/ 1 4) (stream->list (stream-take ppt-stream-in-pair-order 20)) is
              '((3 4 5) (5 12 13) (15 8 17) (7 24 25) (21 20 29) (35 12 37) (9 40 41) (45 28 53)
                (63 16 65) (11 60 61) (33 56 65) (55 48 73) (77 36 85) (99 20 101) (13 84 85)
                (39 80 89) (65 72 97) (91 60 109) (117 44 125) (143 24 145)))
  (check-part 'b (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-pair-order 100) 10)) is
              '((589 300 661) (651 260 701) (713 216 745) (775 168 793) (837 116 845)
                (899 60 901) (33 544 545) (165 532 557) (231 520 569) (429 460 629)))
  (check-part 'c (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-pair-order 211) 8)) is
              '((47 1104 1105) (141 1100 1109) (235 1092 1117) (329 1080 1129) (423 1064 1145)
                (517 1044 1165) (611 1020 1189) (705 992 1217)))
  (check-part 'd (/ 1 4) (stream->list (stream-take (stream-tail ppt-stream-in-pair-order 2022) 4)) is
              '((11139 6820 13061) (11703 6496 13385) (11985 6328 13553) (12549 5980 13901)))
  )

(sumar)
