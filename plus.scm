


(define zero?
  (lambda (n) 
    (cond ((eq? 0 n) #t)
          (else #f))))



(define add1
  (lambda (n) (+ 1 n)))


(define sub1
  (lambda (n) (- n 1)))



(define plus
  (lambda (a b) 
    (cond ((zero? b) a)
          (else (add1 (plus a (sub1 b)))))))



(define sub
  (lambda (a b ) 
    (cond ((eq? b 0) a)
          (else (sub1 (sub a (sub1 b)))))))


(define insertR*
  (lambda (new old l) 
    (cond ((null? l) (quote ()))
          (else (cond ((atom? (car l)) 
                  (cond ((eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l)))))
                        (else (cons (car l) (insertR* new old (cdr l))))))  
                (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))))


(define insertR*
  (lambda (new old l) 
    (cond ((null? l) (quote ()))
          ((atom? (car l)) 
            ((cond 
              ((eq? (car l) new) 
                (cons old (cons new (insertR* new old (cdr l)))))
              (else 
                (cons (car l) (insertR* new old (cdr l)))))))
          (else
            (cons (insertR* new old (car l))
              (insertR* new old (cdr l)))))))



