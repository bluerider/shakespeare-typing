#lang racket

(require "monkeys-typing.rkt")

(define (dedup-file file-list)
  (define (iterator abridged dictionary)
     (let ([string (car dictionary)])
          (if (null? (cdr dictionary))
               abridged
               (iterator (cons string abridged)
                         (foldl (lambda (pos-string condensor)
                                   (if (string-ci=? string pos-string)
                                       condensor
                                       (cons pos-string condensor)))
                                '()
                                 dictionary)))))
  (iterator '() file-list))

;; generate the abridged dictionary
(define out-port (open-output-file "shakespeare-abridged.txt"))
  (for-each (lambda (string)
               (if (void? string)
                   (displayln "void-found" out-port)
                   (displayln string out-port)))
            (dedup-file (map list->string shakespeare-dictionary)))