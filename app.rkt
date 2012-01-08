#lang web-server/insta
(require web-server/formlets "model.rkt")

; start-->request takes a request and initalizes the database and renders
; the main page
(define (start request)
  (render-main-page (initalize-master! (build-path (current-directory))) request))

; render-main-page consumes a master and a request  
; returns an x-expression to be rendered as html
(define (render-main-page master request)
  (local [(define (response-generator make-url)
            (response/xexpr 
             `(html (head (title "Welcome To My Demo!"))
                    (body (h1 "A Currency Converter")
                          (p "This application is a simple currency converter.")  
                          (p "It supports Dollars, Yen, Pounds, and Euros.  Feel free to add your own currency by clicking the link below.")
                          (p "To convert from one currency to another"
                          (br "Enter the amount of the original currency in the first field, 
                              the name of the original currency in the second, and the name of the new currency in the third.")
                          (br "Note that you must use lower-case letter when indicating the currencies you wish to convert."))
                          (li "There are" (li ,(select-new-quantity master) " "  ,(select-new-unit master)) (li "in") 
                              (li ,(select-old-quantity master) " " ,(select-old-unit master)))
                     (form ([action 
                             ,(make-url convert-currency-handler)])
                             ,@(formlet-display convert-currency-formlet)
                             (input ([type "submit"])))
                     (a ([href ,(make-url add-new-currency-handler)])
                        "Add A New Currency")))))
          
          ; convert-currency-handler consumes a request and extracts from its bindings 
          ; the arguments for currency-convert-store, then renders main page with the
          ; newly converted currency
          (define (convert-currency-handler request)
            (define-values (amount namea nameb)
              (formlet-process convert-currency-formlet request))
            (clear-conversions master)
            (define numamount (string->number amount))
            (currency-convert-store master numamount namea nameb)
              (render-main-page master request))
          
          ; add-new-currency-handler consumes a request and returns render-add-new-currency-page
          (define (add-new-currency-handler request)
            (render-add-new-currency-page master request))]
    
    ;send/suspend/dispatch consumes a response-generator and provides it make-url
    (send/suspend/dispatch response-generator)))

; render-add-new-currency page consumes a master and request, and returns html to be rendered.
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
          
          ; add-currency-handler consumes a request and extracts from its bindings the arguments 
          ; for master-currency-insert! and then renders the main page
          (define (add-currency-handler request)
            (define-values (unit rat)
              (formlet-process add-currency-formlet request))
            (master-currency-insert! master unit rat)
            (render-main-page master (redirect/get)))]
          
          (send/suspend/dispatch response-generator)))

;the formlets return an x-expression forest to be rendred into html
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



                                        
  
