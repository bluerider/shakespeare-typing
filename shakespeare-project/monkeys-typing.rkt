#lang racket

;; load the password generator
(require "gen-password.rkt")

;; provide the following modules
(provide shakespeare-dictionary)
(provide shakespeare-dictionary-abridged)
(provide string-gen)

;; load shakespeare text as dictionary
(define shakespeare-dictionary
   (map (lambda (string)
           (string->list string))
        (append-map (lambda (line)
                       (string-split line " "))
                    (file->lines "shakespeare.txt"))))
(define shakespeare-dictionary-abridged
   (map (lambda (string)
           (string->list string))
        (append-map (lambda (line)
                       (string-split line " "))
                    (file->lines "shakespeare-abridged.txt"))))
   
;; (gen-password-list #:dictionary shakespeare-dictionary
;;                    #:variable-length #t
;;                    #:scramble #f
;;                    1)

;; have random string spoken by festival
(define (string-gen size)
   (let ([random-string (foldl (lambda (string condensor)
                                   (string-append string " " condensor))
                               ""
                               (gen-password #:dictionary shakespeare-dictionary-abridged
                                             #:scramble #f
                                             size))])
         (displayln random-string)
         (system (string-append "festival --tts <<" "EOF" "\n"
                                                    random-string "\n"
                                                    "EOF"))
   ))
   
;; process cmdline argument
(module* main #f
   (apply
      (lambda (size)
          (string-gen (string->number size)))
      (vector->list (current-command-line-arguments))))