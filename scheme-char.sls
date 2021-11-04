(define-library (scheme char)
  (export
    char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
    char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
    char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<?
    string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase
    string-upcase)
  (import (chezscheme))

(begin
  (define (digit-value ch)
    (unless (char? ch)
      (errorf 'digit-value "~a is not a character" ch))
    (cond
      [(not (char? ch))
       (errorf 'digit-value "~a is not a character" ch)]
      [(not (char-numeric? ch)) #f]
      [else
       (do ([ch ch (integer->char (- (char->integer ch) 1))]
            [value -1 (+ value 1)])
         ((not (char-numeric? ch)) value))])
))
