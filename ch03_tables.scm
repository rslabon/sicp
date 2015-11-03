(define (make-table)
  (list `*table*))

(define (get-record key records)
  (if (null? records)
      #f
      (if (eq? (caar records) key)
          (car records)
          (get-record key (cdr records)))))

(define (get-value key table)
  (let ((record (get-record key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert-value! key value table)
  (let ((record (get-record key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))
  

(define t (make-table))
(get-value `a t)
(insert-value! `a 666 t)
(get-value `a t)