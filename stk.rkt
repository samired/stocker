#lang racket

(require (planet neil/csv:2:0) net/url plot math/matrix srfi/19)

(plot-new-window? #t)

(define args (current-command-line-arguments))
(define c (vector-ref args 0))


; getting hestorical data for stock, the main function that all others are built upon it
(define (stock stk)
  (csv->list
   (get-pure-port
    (string->url
     (string-append
      "http://finance.google.com/finance/historical?q="
      stk
      "&output=csv")))))


(define (time-price stock)
  (reverse (map
            (compose
             datetime->real
             (lambda (x) (string->date x "~d-~b-~y"))
             car)
            (cdr stock))))


(define (open-price stock)
  (reverse (map (compose string->number third) (cdr stock))))





(define  (historical x)
  (parameterize ([plot-x-ticks (date-ticks #:number 6 #:formats '("~b-~y"))])
    (plot
     (lines
      (map vector
           (time-price (stock x))
           (open-price (stock x))) #:label x  #:color 2))))

(define (daily x)
  (csv->list
   (get-pure-port
    (string->url
     (string-append
      "http://download.finance.yahoo.com/d/quotes.csv?s="
      x
      "&f=oba")))))
(historical c)
(daily c)



