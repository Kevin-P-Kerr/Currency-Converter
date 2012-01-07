#lang web-server/insta
(require web-server/formlets "currency-model.rkt")

(define (start request)
  (render-main-page (initalize-master! (build-path (current-directory))) request))

(define (render-main-page master request)
  (local [(define (response-generator make-url)
            (response/xexpr 
             `(html (head (title "Welcome To My Demo!"))
                    (body (h1 "A Currency Converter")
                          (p "This application is a simple currency converter.")  
                          (p "It supports Dollars, Yen, Pounds, and Euros.  Feel free to add your own currency by clicking the link below")
                          (p "To convert from one currency to another"
                          (br "Enter the amount of the original currency in the first field, 
                              the name of the original currency in the second, and the name of the new currency in the third."))
                          (li "There are" (li ,(select-new-quantity master) " "  ,(select-new-unit master)) (li "in") (li ,(select-old-quantity master) " " ,(select-old-unit master)))
                     (form ([action 
                             ,(make-url convert-currency-handler)])
                             ,@(formlet-display convert-currency-formlet)
                             (input ([type "submit"])))
                     (a ([href ,(make-url add-new-currency-handler)])
                        "Add A New Currency")))))
          
          (define (convert-currency-handler request)
            (define-values (amount namea nameb)
              (formlet-process convert-currency-formlet request))
            (clear-conversions master)
            (define numamount (string->number amount))
            (let ([old (car (currency-convert master numamount namea nameb))]
                  [new (cdr (currency-convert master numamount namea nameb))])
              (master-currency-convert! master (curr-unit (money-curr old)) (money-quant old) (curr-unit (money-curr new))(money-quant new))
              (render-main-page master request)))
          
         
            
          
          (define (add-new-currency-handler request)
            (render-add-new-currency-page master request))]
    
    (send/suspend/dispatch response-generator)))

(define (render-add-new-currency-page master request)
  (local [(define (response-generator make-url)
            (response/xexpr 
             `(html 
               (head (title "Add A Currency"))
               (body (h1 "Add A Currency")
                     (h2 "To Add A Currency Enter The Currency Name And The Number Of Dollars The Base Unit Can Buy")
                     (form ([action 
                             ,(make-url add-currency-handler)])
                             ,@(formlet-display add-currency-formlet)
                             (input ([type "submit"])))))))
          
          (define (add-currency-handler request)
            (define-values (unit rat)
              (formlet-process add-currency-formlet request))
            (master-currency-insert! master unit rat)
            (render-main-page master (redirect/get)))]
          
          (send/suspend/dispatch response-generator)))

(define add-currency-formlet
  (formlet
   (#%# ,{input-string . => . unit}
        ,{input-string . => . rat})
   (values unit rat)))

(define convert-currency-formlet
  (formlet
   (#%# ,{input-string . => . amount}
        ,{input-string . => . namea}
        ,{input-string . => . nameb})
   (values amount namea nameb)))



                                        
  
