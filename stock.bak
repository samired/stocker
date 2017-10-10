#lang racket

(require (planet neil/csv:2:0) net/url plot math/matrix srfi/19)

(plot-new-window? #f)


(define mtrx
  (list*->matrix
   (csv->list
    (get-pure-port
     (string->url
      "http://finance.google.com/finance/historical?q=WFT&output=csv")))))

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


(define (compare-stocks a b c) 
  (parameterize
      ([plot-x-ticks (date-ticks #:number 6 #:formats '("~b-~y"))])
    (plot
     (list
      (lines (map vector
                  (time-price (stock a))
                  (open-price (stock a)))
             #:label a #:color 1)
      
      (lines (map vector
                  (time-price (stock b))
                  (open-price (stock b)))
             #:label b #:color 2)
      
      (lines (map vector
                  (time-price (stock c))
                  (open-price (stock c)))
             #:label c #:color 3)))))


(define (daily x)
  (csv->list
   (get-pure-port
    (string->url
     (string-append
      "http://download.finance.yahoo.com/d/quotes.csv?s="
      x
      "&f=oba")))))


(define  (yearly x)
  (parameterize ([plot-x-ticks (date-ticks #:number 6 #:formats '("~b-~y"))])
    (plot
     (lines
      (map vector
           (time-price (stock x))
           (open-price (stock x))) #:label x  #:color 2))))

(define (diff x y)
  (parameterize ([plot-x-ticks (date-ticks #:number 6 #:formats '("~b-~y"))])
    (plot
     (list
      (tick-grid)
      (lines-interval
       (map vector
            (time-price (stock x))
            (open-price (stock x)))
       (map vector
            (time-price (stock y))
            (open-price (stock y)))
       
       #:color 3 #:line1-color 1 #:line2-color 2 #:label "Difference")))))

(diff "fb" "spy")

(diff "ne" "wft")
(yearly "amd")
(daily "amd")

(compare-stocks "slb" "bhi" "hal")

