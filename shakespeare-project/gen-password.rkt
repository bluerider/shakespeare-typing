#lang racket

;; provide the following modules
(provide default-dictionary)
(provide gen-password)
(provide gen-password-list)
(provide gen-password-list-all)

;; set the default dictionary
(define default-dictionary
  (let ([fun (lambda (start num)
                (for/list ([i (in-range num)])
                   (integer->char (+ i start))))])
       (append (fun 65 26)
               (fun 97 26)
               (fun 48 10)
               (list #\space))))
               
;; generate random password of a certain size
(define gen-password
  (lambda (#:dictionary [dictionary default-dictionary]
           #:scramble [scramble #t]
           size)
     (letrec ([seed-list (for/list ([i (in-range size)])
                             (random (length dictionary)))]
              [string-list (map (lambda (seed)
                                  (list-ref dictionary seed))
                                seed-list)])
        (map (lambda (string)
                (list->string 
                   (if (false? scramble)
                       string
                       (shuffle string))))
             string-list))))
                           
;; generate random password list
(define gen-password-list
  (lambda (#:dictionary [dictionary default-dictionary]
           #:scramble [scramble #t]
           #:variable-length [variable-length #f]
           size)
     (for/list ([i (in-range (expt (length dictionary) size))])
         (gen-password #:dictionary dictionary #:scramble scramble
                       (if (false? variable-length)
                           size
                           (random (length dictionary)))))))
                           
;; generate all possible passwords of a certain size
(define gen-password-list-all
  (lambda (#:dictionary [dictionary default-dictionary]
           size)
       (if (<= size 1)
           (map (lambda (char)
                   (list->string (list char)))
                dictionary)
           (append-map (lambda (string-list)
                          (map (lambda (char)
                                 (string-append (list->string (list char))
                                                string-list))
                               dictionary))
                       (gen-password-list-all #:dictionary dictionary
                                              (sub1 size))))))
                                              

