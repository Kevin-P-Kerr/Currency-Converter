#lang racket
(require db)

(struct master (db))
(struct money (master curr quant))
(struct curr (master unit rat))

(define (initalize-master! home)
  (define db (sqlite3-connect #:database 'memory #:mode 'create))
  (define the-master (master db))
  (unless (table-exists? db "currencies")
     (query-exec db
      (string-append 
       "CREATE TABLE currencies"
       "(unit TEXT, rat INTEGER)"))
    (master-currency-insert! the-master "dollars" 1)
    (master-currency-insert! the-master "pounds" 1.5425)
    (master-currency-insert! the-master "yen" 0.0130)
    (master-currency-insert! the-master "euros" 1.2723)
    (query-exec db
      (string-append "CREATE TABLE conversions" 
                     "(oldunit TEXT, oldquantity TEXT, newunit TEXT, newquantity TEXT)"))
      (master-currency-convert! the-master "euros" "1" "dollars" "1.2723")
    the-master))

(define (master-currency-convert! master old-unit old-quantity new-unit new-quantity)
  (query-exec (master-db master)
              "INSERT INTO conversions (oldunit, oldquantity, newunit, newquantity) VALUES (?, ?, ?, ?)"
              old-unit old-quantity new-unit new-quantity))

(define (master-currency-insert! master unit rat)
  (query-exec (master-db master)
              "INSERT INTO currencies (unit, rat) VALUES (?, ?)"
              unit rat))

(define (make-money master name amount)
  (money master (curr master name
                       (query-value (master-db master)
                                    "SELECT rat FROM currencies WHERE unit = ?"
                                    name))
                 amount))

(define (currency-convert master amount name_1 name_2)
  (let [(a (make-money master name_1 amount))
        (the-rat (/ (query-value (master-db master)
                                 "SELECT rat FROM currencies WHERE unit = ?" name_1)
                    (query-value (master-db master)
                                 "SELECT rat FROM currencies WHERE unit = ?" name_2)))]
    (cons a (make-money master name_2 (* amount the-rat)))))

(define (currency-convert-store master amount name_1 name_2)
  (let ([old (car (currency-convert master amount name_1 name_2))]
        [new (cdr (currency-convert master amount name_1 name_2))])
    (master-currency-convert! master 
                              (curr-unit (money-curr old))
                              (money-quant old)
                              (curr-unit (money-curr new))
                              (money-quant new))))

(define (select-old-unit master)
  (query-value (master-db master)
               "SELECT oldunit FROM conversions"))

(define (select-new-unit master)
  (query-value (master-db master)
               "SELECT newunit FROM conversions"))

(define (select-old-quantity master)
  (query-value (master-db master)
               "SELECT oldquantity FROM conversions"))

(define (select-new-quantity master)
  (query-value (master-db master)
               "SELECT newquantity FROM conversions"))

(define (clear-conversions master)
  (query-exec (master-db master)
               "DELETE FROM conversions"))




(define y (initalize-master! (build-path (current-directory))))
(define x (make-money y "pounds" 200))
(* 2 (money-quant x))

(money-curr x)
(curr-unit (money-curr x))
(curr-rat (money-curr x))
(define iou (currency-convert y 200 "pounds" "yen"))
(define io (car iou))
(define u (cdr iou))
(list (money-quant io) (money-quant u))

(provide money-quant money-curr money-quant curr-unit curr-rat initalize-master! master-currency-convert! select-new-unit select-old-quantity select-old-unit currency-convert make-money master-currency-insert! clear-conversions select-new-quantity y x)

(select-old-unit y)
(select-new-unit y)
(select-old-quantity y)
(select-new-quantity y)
