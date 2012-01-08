#lang racket
(require db)

; A master is a database connection
(struct master (db))

; money consists of a master, curr (currency), and a quantity thereof.
(struct money (master curr quant))

; curr consits of a master,a unit (the name of the currency),
;and the rat (the dollar-amount a unit of the currency can buy)
(struct curr (master unit rat))

;initalize-master! takes a path, and creates a sqlite3 database there if one does not exist, and returns a master
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

; master-currency-convert! consumes a master and the results of a currency conversion, 
;and pushes them into the database associated with the master.
(define (master-currency-convert! master old-unit old-quantity new-unit new-quantity)
  (query-exec (master-db master)
              "INSERT INTO conversions (oldunit, oldquantity, newunit, newquantity) VALUES (?, ?, ?, ?)"
              old-unit old-quantity new-unit new-quantity))

; master-currency-insert! consumes a master, and a new unit and rat 
; to be added to the database-table of currencies associatd with the master.
(define (master-currency-insert! master unit rat)
  (query-exec (master-db master)
              "INSERT INTO currencies (unit, rat) VALUES (?, ?)"
              unit rat))

; make-money consumes a master, a currency name and amount
; and creates a (struct money) out of them.
(define (make-money master name amount)
  (money master (curr master name
                       (query-value (master-db master)
                                    "SELECT rat FROM currencies WHERE unit = ?"
                                    name))
                 amount))

; currency-convert consumes a master, a number, a currency to converted from
; and a currency to be converted to, and returns a (cons (struct money) (struct money))
; of the original and converted money, respectively. 
(define (currency-convert master amount name_1 name_2)
  (let [(a (make-money master name_1 amount))
        (the-rat (/ (query-value (master-db master)
                                 "SELECT rat FROM currencies WHERE unit = ?" name_1)
                    (query-value (master-db master)
                                 "SELECT rat FROM currencies WHERE unit = ?" name_2)))]
    (cons a (make-money master name_2 (* amount the-rat)))))

; currency-convert-store converts one currency to another,
; and stores the results
(define (currency-convert-store master amount name_1 name_2)
  (let ([old (car (currency-convert master amount name_1 name_2))]
        [new (cdr (currency-convert master amount name_1 name_2))])
    (master-currency-convert! master 
                              (curr-unit (money-curr old))
                              (money-quant old)
                              (curr-unit (money-curr new))
                              (money-quant new))))

; select-old-unit consumes a master and selects from it
; the value of oldunit
(define (select-old-unit master)
  (query-value (master-db master)
               "SELECT oldunit FROM conversions"))

; select-new-unit consums a master and selects from it
; the value of newunit
(define (select-new-unit master)
  (query-value (master-db master)
               "SELECT newunit FROM conversions"))

; select-old-quantity consumes a master and selects from it
; the value of oldquantity
(define (select-old-quantity master)
  (query-value (master-db master)
               "SELECT oldquantity FROM conversions"))

; select-new-quantity consumes a master and select from it
; the value of newquantity
(define (select-new-quantity master)
  (query-value (master-db master)
               "SELECT newquantity FROM conversions"))

;clear-conversions consumes a master, and clears the values
; of the conversions table, in preparation to render a new conversion
(define (clear-conversions master)
  (query-exec (master-db master)
               "DELETE FROM conversions"))

(provide money-quant money-curr  money-master
         curr-unit curr-rat curr-master 
         initalize-master! master-currency-convert! master-currency-insert!
         select-new-unit select-old-quantity select-old-unit 
         currency-convert currency-convert-store make-money clear-conversions select-new-quantity)
