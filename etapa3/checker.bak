#lang racket

(require "cryptosystem.rkt")

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

(sunt 8 exerciții)

(exercițiul 1 : 20 puncte)
(check-part 'a (/ 1 20) (key 1) is '(5 12 13 24 4 5 3 4 5))
(check-part 'b (/ 1 20) (key 2) is '(9 13 14 20 24 25 15 8 17))
(check-part 'c (/ 1 20) (key 4) is '(16 3 7 19 6 10 5 12 13))
(check-part 'd (/ 1 20) (key 7) is '(18 19 8 14 15 4 6 2 11))
(check-part 'e (/ 1 20) (key 9) is '(10 3 1 13 6 4 11 12 7))
(check-part 'f (/ 1 20) (key 17) is '(17 12 1 15 10 26 3 1 8))
(check-part 'g (/ 1 20) (key 20) is '(1 3 19 4 6 22 20 12 25))
(check-part 'h (/ 1 20) (key 96) is '(1 6 19 25 3 16 23 15 22))
(check-part 'i (/ 1 20) (key 106) is '(5 6 13 9 10 17 24 25 11))
(check-part 'j (/ 1 20) (key 131) is '(21 23 5 24 26 8 10 12 1))
(check-part 'k (/ 1 20) (key 487) is '(15 7 2 2 21 16 12 20 2))
(check-part 'l (/ 1 20) (key 577) is '(10 3 1 13 6 4 11 12 7))
(check-part 'm (/ 1 20) (key 742) is '(9 11 11 18 20 20 1 9 1))
(check-part 'n (/ 1 20) (key 864) is '(14 6 4 9 1 26 24 16 20))
(check-part 'o (/ 1 20) (key 987) is '(18 2 2 9 20 20 19 18 19))
(check-part 'p (/ 1 20) (key 1143) is '(15 14 23 3 2 11 22 6 13))
(check-part 'q (/ 1 20) (key 2509) is '(23 6 22 9 19 8 24 7 2))
(check-part 'r (/ 1 20) (key 3614) is '(15 10 26 17 12 1 3 26 8))
(check-part 's (/ 1 20) (key 4016) is '(5 6 13 9 10 17 24 25 11))
(check-part 't (/ 1 20) (key 5191) is '(13 6 4 10 3 1 11 15 7))

(exercițiul 2 : 15 puncte)
(check-part 'a (/ 1 5) (message->codes "do or do not") is '(4 15 0 15 18 0 4 15 0 14 15 20))
(check-part 'b (/ 1 5) (message->codes "there is no try") is '(20 8 5 18 5 0 9 19 0 14 15 0 20 18 25))
(check-part 'c (/ 1 5) (message->codes "functionale") is '(6 21 14 3 20 9 15 14 1 12 5))
(check-part 'd (/ 1 5) (message->codes "ce tare ma distrez la facultate") is
            '(3 5 0 20 1 18 5 0 13 1 0 4 9 19 20 18 5 26 0 12 1 0 6 1 3 21 12 20 1 20 5))
(check-part 'e (/ 1 5) (message->codes "faith and trust and pixie dust") is '(6 1 9 20 8 0 1 14 4 0 20 18 21 19 20 0 1 14 4 0 16 9 24 9 5 0 4 21 19 20))

(exercițiul 3 : 15 puncte)
(check-part 'a (/ 1 5) (codes->message '(4 15 0 15 18 0 4 15 0 14 15 20)) is "do or do not")
(check-part 'b (/ 1 5) (codes->message '(20 8 5 18 5 0 9 19 0 14 15 0 20 18 25)) is "there is no try")
(check-part 'c (/ 1 5) (codes->message '(6 21 14 3 20 9 15 14 1 12 5)) is "functionale")
(check-part 'd (/ 1 5) (codes->message '(3 5 0 20 1 18 5 0 13 1 0 4 9 19 20 18 5 26 0 12 1 0 6 1 3 21 12 20 1 20 5)) is
            "ce tare ma distrez la facultate")
(check-part 'e (/ 1 5) (codes->message '(6 1 9 20 8 0 1 14 4 0 20 18 21 19 20 0 1 14 4 0 16 9 24 9 5 0 4 21 19 20)) is "faith and trust and pixie dust")

(exercițiul 4 : 20 puncte)
(check-part 'a (/ 1 10) (extend-key '(24 16 20 11 3 7 21 20 2) 21) is '(24 16 20 11 3 7 21 20 2 24 16 20 11 3 7 21 20 2 24 16 20))
(check-part 'b (/ 1 10) (extend-key '(24 16 20 11 3 7 21 20 2) 7) is '(24 16 20 11 3 7 21))
(check-part 'c (/ 1 10) (extend-key '(5 12 21 16 25 16 23 17 12) 3) is '(5 12 21))
(check-part 'd (/ 1 10) (extend-key '(8 12 2 4 7 9 18 1) 8) is '(8 12 2 4 7 9 18 1))
(check-part 'e (/ 1 10) (extend-key '(8) 10) is '(8 8 8 8 8 8 8 8 8 8))
(check-part 'f (/ 1 10) (extend-key '(3 8 9) 10) is '(3 8 9 3 8 9 3 8 9 3))
(check-part 'g (/ 1 10) (extend-key '(18 19 1 18 17 7 10 0 4 17) 17) is '(18 19 1 18 17 7 10 0 4 17 18 19 1 18 17 7 10))
(check-part 'h (/ 1 10) (extend-key '(13 6 4 10 3 1 11 15 7) 18) is '(13 6 4 10 3 1 11 15 7 13 6 4 10 3 1 11 15 7))
(check-part 'i (/ 1 10) (extend-key '(11 17) 11) is '(11 17 11 17 11 17 11 17 11 17 11))
(check-part 'j (/ 1 10) (extend-key '(13 6 4 10 3 1 11 15 7) 1) is '(13))

(exercițiul 5 : 15 puncte)
(when (not (equal? encrypt-codes 'your-code-here))
  (check-part 'a (/ 1 5) (encrypt-codes '(4 15 0 15 18 0 4 15 0 14 15 20) '(17 15 1 9 7 20 6 4 5)) is
              '(21 3 1 24 25 20 10 19 5 4 3 21))
  (check-part 'b (/ 1 5) (encrypt-codes '(20 8 5 18 5 0 9 19 0 14 15 0 20 18 25) '(22 21 13 16 15 7 17 3 1)) is
              '(15 2 18 7 20 7 26 22 1 9 9 13 9 6 5))
  (check-part 'c (/ 1 5) (encrypt-codes '(6 21 14 3 20 9 15 14 1 12 5) '(9 10 17 5 6 13 24 2 11)) is
              '(15 4 4 8 26 22 12 16 12 21 15))
  (check-part 'd (/ 1 5) (encrypt-codes '(3 5 0 20 1 18 5 0 13 1 0 4 9 19 20 18 5 26 0 12 1 0 6 1 3 21 12 20 1 20 5)
                                        '(0 8 8 0 8 8 4 0 4))
              is '(3 13 8 20 9 26 9 0 17 1 8 12 9 0 1 22 5 3 0 20 9 0 14 9 7 21 16 20 9 1 5))
  (check-part 'e (/ 1 5) (encrypt-codes '(6 1 9 20 8 0 1 14 4 0 20 18 21 19 20 0 1 14 4 0 16 9 24 9 5 0 4 21 19 20)
                                        '(13 6 4 10 3 1 11 15 7))
              is '(19 7 13 3 11 1 12 2 11 13 26 22 4 22 21 11 16 21 17 6 20 19 0 10 16 15 11 7 25 24))
  )

(exercițiul 6 : 15 puncte)
(when (not (equal? decrypt-codes 'your-code-here))
  (check-part 'a (/ 1 5) (decrypt-codes '(14 6 10 16 0 1 12 6 19 24 6 3) '(10 18 10 1 9 1 8 18 19)) is
              '(4 15 0 15 18 0 4 15 0 14 15 20))
  (check-part 'b (/ 1 5) (decrypt-codes '(14 15 7 14 14 4 18 18 26 8 22 2 16 0 2) '(21 7 2 23 9 4 9 26 26)) is
              '(20 8 5 18 5 0 9 19 0 14 15 0 20 18 25))
  (check-part 'c (/ 1 5) (decrypt-codes '(10 18 9 22 5 19 26 20 8 16 2) '(4 24 22 19 12 10 11 6 7)) is
              '(6 21 14 3 20 9 15 14 1 12 5))
  (check-part 'd (/ 1 5) (decrypt-codes '(5 23 25 5 2 26 23 22 18 3 18 2 21 20 1 9 0 4 2 3 26 12 7 9 21 16 17 22 19 18 17)
                                        '(2 18 25 12 1 8 18 22 5))
              is '(3 5 0 20 1 18 5 0 13 1 0 4 9 19 20 18 5 26 0 12 1 0 6 1 3 21 12 20 1 20 5))
  (check-part 'e (/ 1 5) (decrypt-codes '(2 7 4 2 0 8 25 21 6 23 26 13 3 11 1 24 8 16 0 6 11 18 16 17 2 7 6 17 25 15)
                                        '(23 6 22 9 19 8 24 7 2))
              is '(6 1 9 20 8 0 1 14 4 0 20 18 21 19 20 0 1 14 4 0 16 9 24 9 5 0 4 21 19 20))
  )

(exercițiul 7 : 10 puncte)
(when (not (equal? encrypt-message 'your-code-here))
  (check-part 'a (/ 1 5) (encrypt-message "do or do not there is not try" '(12 16 20 23 0 4 0 8 8)) is "pdtkrddwhzdmwtlezmlylwnsthacn")
  (check-part 'b (/ 1 5) (encrypt-message "having your book turned into a movie is like seeing your oxen turned into bouillon cubes"
                                          '(24 16 20 11 3 7 21 20 2))
              is "eqotqnurqrgtmrvetvrggpggcgvlpukpvpbgxylkopeybpuytqnurqrgtz lhtvrggpggcgvlpvzxpfeqkpweelm")
  (check-part 'c (/ 1 5) (encrypt-message "all the world is made of faith and trust and pixie dust"
                                          '(9 25 2 26 15 19 6 5 14))
              is "jjnzh kejxpncoaye jbgzcyfkorrjzpfjeg susottinygzhtsjzfb")
  (check-part 'd (/ 1 5) (encrypt-message "it sounds plausible enough tonight but wait until tomorrow wait for the common sense of the morning"
                                          '(1 21 19 22 15 13 11 3 7))
              is "jnsncgygzajdwietesfuxicgrkguifdvudcivnsrpvdcaonagofzpvslgroill a gmofshgdiehc kvlomxvcskwofuejf tqn")
  (check-part 'e (/ 1 5) (encrypt-message "some birds are not meant to be caged that s all their feathers are too bright their songs too sweet and wild so you let them go or when you open the cage to feed them they somehow fly out past you and the part of you that knows it was wrong to imprison them in the first place rejoices but still the place where you live is that much more drab and empty for their departure"
                                          '(6 17 26 12 23 5 1 24 19))
              is "yelqwgjowyq caeollfcdmjyaqgfsdlzfhbwfjgmpetxtrbzedjjoslv edjspsghdlptpxuxzftpeuexohzdkshpszenloafblfrmpwajiwfinlutvxdkjzedjnxzuqncwaibffonfwtqbffjgqwhbdxfjnlbjfaszydywyibqfinyamptslbxlkzuxhgislutvxttuzedjamtxjz bezlmfjgmpelkgbizupexykfmq jlaqgfzlanntlffjgqiejkszydlbnsplffkmzjaoxpehoaxazmzqreeqmxlnvzahfdbsbydcaezlmfbhgaejpszy ewrv  fcncaeeothq z efjhzozrkwaq kzql jqyjzkqq")
  )

(exercițiul 8 : 10 puncte)
(when (not (equal? decrypt-message 'your-code-here))
  (check-part 'a (/ 1 5) (decrypt-message "jqk zqklytqdlaylocfkclve xrx " '(6 2 11 12 8 17 7 24 25)) is "do or do not there is not try")
  (check-part 'b (/ 1 5) (decrypt-message "mmhfrlcbtzcmzstndyzc bhelrytlnxqtymjeuexpnniexqrfrlcbtzcmlajqdyzc bhelrytlolynoptslprfjv"
                                          '(5 12 13 24 4 5 3 4 5))
              is "having your book turned into a movie is like seeing your oxen turned into bouillon cubes")
  (check-part 'c (/ 1 5) (decrypt-message "vpqhk wtpivqlrajtfvhjhfyrzucxmhsfvtmlyxartextjmbqwsvnln"
                                          '(21 4 5 8 18 19 18 20 20))
              is "all the world is made of faith and trust and pixie dust")
  (check-part 'd (/ 1 5) (decrypt-message "nzmaykkbcevyjdif wjfrwykdfkyu rqyqymzzmekzqyeszvujjlkzwxaejmygdela jjeckhuzvydxqpsyriywxrsjfzxadflr"
                                          '(5 6 13 9 10 17 24 25 11))
              is "it sounds plausible enough tonight but wait until tomorrow wait for the common sense of the morning")
  (check-part 'e (/ 1 5) (decrypt-message "ufkqaj miurzcfhejybdcmoarotbtcldiy ibkfmuhjvfncyeim mehwzeiminecicluwfvgt etuhkcjkiydpvynevfmltdw ybslpad gibjmlzwlvqgkyeimdvlqrmcadz sbpmfawg sbkfqaksbjbkmlgmwzevzcyaaz cbjmyfpfrehcwlpbkvucjrlzwlvfpvyeimrkftky ghpjzbkfmuhbityjyuuhnwxbnp oorotb kasqjjsbkfqnh ievzclgqinybgjmdmrmjlfgof rxzvrqejtcvyjwyamiu eyzccfhpjzbcggfh nevzzeaulymbdmcfhvmfdrzzehwhuvpyrpzromg plemgwwvlpq"
                                          '(2 18 25 12 1 8 18 22 5))
              is "some birds are not meant to be caged that s all their feathers are too bright their songs too sweet and wild so you let them go or when you open the cage to feed them they somehow fly out past you and the part of you that knows it was wrong to imprison them in the first place rejoices but still the place where you live is that much more drab and empty for their departure")
  )

(sumar)
