#|Question 1|#
(define(make6 x y)
  (if (or (< (left (makePos x)) 0)(<(right (makePos y)) 0)) -2
  (+(right (makePos y))(left (makePos x)))))

(define(count n)
  (if (< n 10) 1
      (+ 1 (count(/ n 10)))))

(define(right y)
  (cond ((< y 100) -1)
        (else (mod y 1000))))

(define(left x)
  (if(< x 100) -1
     (* (/ (- x (mod x (pow 10 (-(count x) 3))))
           (pow 10 (-(count x) 3))) 1000)))

(define(makePos L)
  (if(< L 0) (* L -1) L))

(define (pow L M)
  (if (= M 0) 1
      (* L (pow L (- M 1)))))

(define (mod L M)
  (if (< (- L M) 0) L
      (mod (- L M) M)))

#|Question 2|#
(define (concatL L M)
  (cond
   ((null? L) '())
   ((null? M) '())
   (else (append
          (list
           (string-append
            (concatS (car L)) (concatS (car M))
           )
          )
          (cond
            ((null? (cdr L)) '())
            ((null? (cdr M)) '())
           (else(concatL (cdr L) (cdr M)))
          )
         )
   )
  )
)

(define (concatS L)
  (if (null? L) ""
      (string-append
       (symbol->string
        (car L))
       (concatS
        (cdr L))
       )
  )
)
(concatS '(a b c))
#|Basic Test|#
(concatL '((a b) (c d) (d e)) '((f f f) (d e s) (v v v v)))
#|Tests when L<M or M<L|#
(concatL '((a b) (c d)) '((e f)))
(concatL '((a b)) '((c d) (e f)))
#|Tests when L or M is null|#
(concatL '((a b)) '())
(concatL '() '((a b)))

#|Question 3|#
(define (buildList N E1 M E2)
  (cond
    ((> N 0) (append (list E1) (buildList (- N 1) E1 M E2)))
    ((and (= N 0) (= M 1)) (list E2))
    (else (append (list E2) (buildList N E1 (- M 1) E2))))
)
#|Tests|#
(buildList 5 '() 3 'B)
(buildList 3 'A 2 'C)
(buildList 3 '(A B) 2 'C)

#|Question 4|#

(define (DFA-Acceptor list S F P)
  (if(null? list) (compfunc S F)
     (cond
       ((compfunc S F) #t)
       ((eq? S P) #f)
       (else (DFA-Acceptor (cdr list) (S (car list)) F P))
     )
  )
)

(define (compfunc F list)
  (if (null? list) #f
  (if (eq? F (car (map eval list))) #t (compfunc F (cdr list)))))

(define (Q0 value) (if (= value 1) Q0 Q1))
(define (Q1 value) (if (= value 1) Q2 P))
(define (Q2 value) Q1)
(define (P value) P)

#|Tests|#
(DFA-Acceptor '() Q0 '(Q0) P)
(DFA-Acceptor '() Q0 '(Q2) P)
(DFA-Acceptor '(1 1) Q0 '(Q2 Q0) P)
(DFA-Acceptor '(1 1) Q0 '(Q2) P)

#|Question 5|#
(define (selectN N)
  (lambda (L) (remove-last L N)))

(define (remove-last L N) (if (= N 1) (reverse (cdr (reverse L)))
                              (remove-last (reverse (cdr (reverse L))) (- N 1))
                          )
)

#|Test|#
(define select3 (selectN 3))
(select3 '(A B C D E F))