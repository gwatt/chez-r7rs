;
; All code shamelessly lifted from ChezScheme s/syntax.ss
;

(let ()

(define make-wrap cons)
(define wrap-marks car)
(define wrap-subst cdr)

(define make-binding cons)

(define-record-type syntax-object
  (fields expression wrap)
  (nongenerative #{syntax-object bdehkef6almh6ypb-a})
  (sealed #t))

(define-record-type fixed-ribcage
  (fields symnames marks label/pls)
  (nongenerative #{fixed-ribcage cqxefau3fa3vz4m0-0})
  (sealed #t))

(define-syntax top-wrap
  (identifier-syntax '((top))))

(define (make-resolved-id sym marks label/pl)
  (when (gensym? sym) (gensym->unique-string sym))
  (make-syntax-object sym
    (make-wrap marks
      (list (make-fixed-ribcage (vector sym) (vector marks) (vector label/pl))))))

(define (global-extend type sym val)
  (#%$sc-put-cte
    (make-resolved-id sym (wrap-marks top-wrap) sym)
    (make-binding type val)
    '*system*))

(define (generate-id sym)
  (gensym (symbol->string sym)))

(define (create-library-uid name)
  (syntax-case name ()
    [(dir-id ... file-id)
     (and (andmap identifier? #'(dir-id ...)) (identifier? #'file-id))
     (let ([uid (generate-id (datum file-id))])
       (values #'(dir-id ... file-id) (datum->syntax #'* uid)))]))

(define (symbolic-id=? id sym)
  (eq? (id-sym-name id) sym))

(define (id-sym-name e)
  (unannotate
    (if (syntax-object? e)
        (syntax-object-expression e)
        e)))

(define (unannotate e)
  (if (annotation? e)
      (annotation-expression e)
      e))

(global-extend 'macro 'define-library
  (lambda (orig)
    (syntax-case orig ()
      [(_ name decls ...)
       (letrec
         ([extract-forms
           (lambda (input exports imports code)
             (syntax-case input ()
               [() (list exports imports code)]
               [((export? ex ...) . rest) (symbolic-id=? #'export? 'export)
                (extract-forms #'rest
                  #`(#,@exports ex ...)
                  imports code)]
               [((import? im ...) . rest) (symbolic-id=? #'import? 'import)
                (extract-forms #'rest
                  exports
                  #`(#,@imports im ...)
                  code)]
               [((begin? b ...) . rest) (symbolic-id=? #'begin? 'begin)
                (extract-forms #'rest
                  exports
                  imports
                  #`(#,@code b ...))]
               [((include? inc) . rest) (symbolic-id=? #'include? 'include)
                (extract-forms #'rest
                  exports
                  imports
                  #`(#,@code (include inc)))]))])
         (let-values ([(library-path uid) (create-library-uid #'name)])
           (with-syntax
             ([((ex ...) (im ...) (code ...)) (extract-forms #'(decls ...) '() '() '())])
             #`(library name
                 (export ex ...)
                 (import im ...)
                 code ...))))])))

;
; Copied from (#%$system-property-list 'library)
;

(#%$sputprop 'define-library '*top* 'define-library)
(#%$sputprop 'define-library '*scheme* 'define-library)
(#%$sputprop 'define-library '*pretty-format*
  '(_ (name ...) #f e ...))
(#%$sputprop 'define-library '*flags* 4)
 
)
