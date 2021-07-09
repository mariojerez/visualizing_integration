(require (lib "fungraph.ss" "concabs"))



(define power
  (lambda (base exponent)
    (cond
      ((= exponent 0) 1)
      (else (* base (power base (- exponent 1)))))))


(define add-all
  (lambda (nums)
    (cond
      ((null? nums) 0)
      (else (+ (car nums) (add-all (cdr nums)))))))


(define add-to-end
  (lambda (n nums)
    (cond
      ((null? nums) (cons n '()))
      (else (cons (car nums) (add-to-end n (cdr nums)))))))

(define f1
  (lambda (x)
    (+ (- (/ (power x 4) 24) (/ (power x 2) 2)) 4.0)))

(define f2
  (lambda (x)
    (+ x 4)))

(define df1
  (lambda (x)
    (- (/ (power x 3) 6) x)))

(define slopes-f1
  (lambda (n x1 x2)
    (slopes-f1-help (/ (- x2 x1) n) n x1 x2)))

(define slopes-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n -1) '())
      (else (add-to-end (df x2)
                        (slopes-f1-help dx (- n 1) x1 (- x2 dx)))))))


(define area-reclft-f1
  (lambda (n x1 x2)
    (add-all (area-reclft-f1-help (/ (- x2 x1) n) n x1 x2))))


(define area-reclft-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f1 (- x2 dx)))
                        (area-reclft-f1-help dx (- n 1) x1 (- x2 dx)))))))

(define area-reclft-f2
  (lambda (n x1 x2)
    (add-all (area-reclft-f2-help (/ (- x2 x1) n) n x1 x2))))

(define area-reclft-f2-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f2 (- x2 dx)))
                        (area-reclft-f2-help dx (- n 1) x1 (- x2 dx)))))))


(define area-recrt-f1
  (lambda (n x1 x2)
    (add-all (area-recrt-f1-help (/ (- x2 x1) n) n x1 x2))))

(define area-recrt-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f1 x2))
                        (area-recrt-f1-help dx (- n 1) x1 (- x2 dx)))))))

(define area-recrt-f2
  (lambda (n x1 x2)
    (add-all (area-recrt-f2-help (/ (- x2 x1) n) n x1 x2))))

(define area-recrt-f2-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f2 x2))
                        (area-recrt-f2-help dx (- n 1) x1 (- x2 dx)))))))

(define area-recmid-f1
   (lambda (n x1 x2)
    (add-all (area-recmid-f1-help (/ (- x2 x1) n) n x1 x2))))

(define area-recmid-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f1 (- x2 (* (/ 1 2) dx))))
                        (area-recmid-f1-help dx (- n 1) x1 (- x2 dx)))))))

(define area-recmid-f2
   (lambda (n x1 x2)
    (add-all (area-recmid-f2-help (/ (- x2 x1) n) n x1 x2))))

(define area-recmid-f2-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (f2 (- x2 (* (/ 1 2) dx))))
                        (area-recmid-f2-help dx (- n 1) x1 (- x2 dx)))))))

(define area-trap-f1
  (lambda (n x1 x2)
    (add-all (area-trap-f1-help (/ (- x2 x1) n) n x1 x2))))

(define area-trap-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (/ (+ (f1 x2) (f1 (- x2 dx))) 2))
                        (area-trap-f1-help dx (- n 1) x1 (- x2 dx)))))))

(define area-trap-f2
  (lambda (n x1 x2)
    (add-all (area-trap-f2-help (/ (- x2 x1) n) n x1 x2))))

(define area-trap-f2-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* dx (/ (+ (f2 x2) (f2 (- x2 dx))) 2))
                        (area-trap-f2-help dx (- n 1) x1 (- x2 dx)))))))

(define area-sim-f1
  (lambda (n x1 x2)
    (add-all (area-sim-f1-help (/ (- x2 x1) n) n x1 x2))))


(define area-sim-f1-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* (/ dx 6) (+ (f1 (- x2 dx)) (+ (* 4 (f1 (- x2 (/ dx 2)))) (f1 x2))))
                        (area-sim-f1-help dx (- n 1) x1 (- x2 dx)))))))

(define area-sim-f2
  (lambda (n x1 x2)
    (add-all (area-sim-f2-help (/ (- x2 x1) n) n x1 x2))))

(define area-sim-f2-help
  (lambda (dx n x1 x2)
    (cond
      ((= n 0) '())
      (else (add-to-end (* (/ dx 6) (+ (f2 (- x2 dx)) (+ (* 4 (f2 (- x2 (/ dx 2)))) (f2 x2))))
                        (area-sim-f2-help dx (- n 1) x1 (- x2 dx)))))))





