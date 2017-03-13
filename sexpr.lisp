#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defun symbol->identifier (symbol)
  (with-output-to-string (out)
    (with-input-from-string (in (string symbol))
      (loop for char = (read-char in NIL)
            while char
            do (case char
                 (#\- (write-char (char-upcase (read-char in)) out))
                 (T (write-char (char-downcase char) out)))))))

(defun sexpr->glsl-ast (form)
  (etypecase form
    ((or null integer float keyword string)
     form)
    (symbol
     (symbol->identifier form))
    (cons
     (funcall (or (sexpr-transform (first form))
                  (error "No sexpr-transformer found for the following form:~%  ~a"
                         form))
              form))))

(defmacro with-glsl-syntax (&body forms)
  (serialize
   `(shader
     ,@(mapcar #'sexpr->glsl-ast forms))))

(defvar *sexpr-transforms* (make-hash-table :test 'eql))

(defun sexpr-transform (op)
  (gethash op *sexpr-transforms*))

(defun (setf sexpr-transform) (function op)
  (setf (gethash op *sexpr-transforms*) function))

(defun remove-sexpr-transform (op)
  (remhash op *sexpr-transforms*))

(defmacro define-sexpr-transform (op args &body body)
  (let ((o (gensym "FORM")))
    `(progn (setf (sexpr-transform ',op)
                  (lambda (,o)
                    (flet ((r (form)
                             (sexpr->glsl-ast form)))
                      (declare (ignorable #'r))
                      (destructuring-bind ,args (rest ,o)
                        ,@body))))
            ',op)))

(define-sexpr-transform + (&rest values)
  (cond ((cdr values)
         `(addition ,(r (first values))
                    ,(r `(+ ,@(rest values)))))
        (values
         (r (first values)))
        (T
         0)))

(define-sexpr-transform - (first &rest values)
  (cond (values
         `(subtraction ,(r first)
                       ,(r `(+ ,@values))))
        (T
         `(negation ,(r first)))))

(define-sexpr-transform * (&rest values)
  (cond ((cdr values)
         `(multiplication ,(r (first values))
                          ,(r `(* ,@(rest values)))))
        (values
         (r (first values)))
        (T
         1)))

(define-sexpr-transform / (first &rest values)
  (cond (values
         `(division ,(r first)
                       ,(r `(* ,@values))))
        (T
         `(division 1.0 ,(r first)))))

(defmacro define-sexpr-comparator (name ast)
  `(define-sexpr-transform ,name (first &rest values)
     (cond ((cdr values)
            `(logical-and (,',ast ,(r first) ,(r (first values)))
                          ,(r `(,',name ,(first values) ,(cdr values)))))
           (values
            `(,',ast ,(r first) ,(r (first values))))
           (T
            :true))))

(define-sexpr-comparator = equal)
(define-sexpr-comparator /= not-equal)
(define-sexpr-comparator > greater-than)
(define-sexpr-comparator < less-than)
(define-sexpr-comparator >= greater-equal-than)
(define-sexpr-comparator <= less-equal-than)

(defmacro define-expr-binary (name ast default)
  `(define-sexpr-transform ,name (&rest values)
     (cond (values
            `(,',ast ,(r (first values))
                     ,(r `(,',name ,@(rest values)))))
           (T
            ,default))))

(define-expr-binary and logical-and :true)
(define-expr-binary logand bitwise-and (1- (ash 1 32)))
(define-expr-binary or logical-or :false)
(define-expr-binary logior inclusive-or 0)
;; FIXME for uneven args case
(define-expr-binary xor logical-xor :false)
(define-expr-binary logxor exclusive-or 0)

(define-sexpr-transform lognot (value)
  `(bit-inversion ,(r value)))

(define-sexpr-transform ash (value amount)
  ;; This is leaky if VALUE is modifying.
  `(conditional
    (less-than 0 ,(r value))
    (left-shift ,(r value) ,(r amount))
    (right-shift ,(r value) ,(r amount))))

(define-sexpr-transform 1+ (value)
  `(addition 1 ,(r value)))

(define-sexpr-transform 1- (value)
  `(subtraction ,(r value) 1))

(define-sexpr-transform incf (place &optional (amount 1))
  `(assignment ,(r place) :+= ,(r amount)))

(define-sexpr-transform decf (place &optional (amount 1))
  `(assignment ,(r place) :-= ,(r amount)))

(define-sexpr-transform dotimes ((var count) &rest body)
  `(for-statement
    (variable-declaration
     (type-qualifier) (type-specifier :int) ,(r var) 0)
    (less-than ,(r var) ,(r count))
    (prefix-increment ,(r var))
    (compound-statement ,@(mapcar #'r body))))

(define-sexpr-transform case (var &body cases)
  `(switch-statement ,var
                     (compound-statement
                      ,@(loop for (c . body) in cases
                              appending (loop for case in (enlist c)
                                              collect `(case-label ,(if (eql T case)
                                                                        :default
                                                                        (r case))))
                              appending (mapcar #'r body)
                              appending `((break))))))

(define-sexpr-transform return (&optional value)
  `(return ,(if value
                (r value)
                no-value)))

(define-sexpr-transform discard ()
  `(discard))

(define-sexpr-transform continue ()
  `(continue))

(define-sexpr-transform break ()
  `(break))

(defun separate-qualifier-specifier (types)
  (let ((qualifiers ())
        (specifiers ()))
    (dolist (type (enlist types))
      (if (or (find type '(:invariant :smooth :flat :noperspective
                           :precise :const :inout :in :out :centroid
                           :patch :sample :uniform :buffer :shared
                           :coherent :volatile :restrict :readonly
                           :writeonly :highp :mediump :lowp))
              (and (consp type) (find (first type) '(layout-qualifier
                                                     subroutine-qualifier))))
          (push type qualifiers)
          (push type specifiers)))
    (values (nreverse qualifiers)
            (nreverse specifiers))))

(define-sexpr-transform defvar (type ident &optional value)
  (multiple-value-bind (qualifiers specifiers)
      (separate-qualifier-specifier type)
    `(variable-declaration
      ,(if qualifiers `(type-qualifier ,@qualifiers) no-value)
      (type-specifier ,@specifiers)
      ,(r ident)
      ,no-value
      ,value)))

(define-sexpr-transform let (bindings &body body)
  `(compound-statement
    ,@(loop for binding in bindings
            collect (r `(defvar ,@binding)))
    ,@(mapcar #'r body)))

(define-sexpr-transform defun (identifier arglist type &body body)
  (let ((prototype (multiple-value-bind (qualifiers specifiers)
                       (separate-qualifier-specifier type)
                     `(function-prototype
                       (type-qualifier ,@qualifiers)
                       (type-specifier ,@specifiers)
                       ,(r identifier)
                       ,@(loop for (type ident &optional array) in arglist
                               collect `((type-specifier ,type) ,(r ident)
                                         ,@(when array (list array))))))))
    (if body
        `(function-definition
          ,prototype
          (compound-statement
           ,@(mapcar #'r body)))
        `(function-declaration
          ,prototype))))

(define-sexpr-transform setf (&rest pairs)
  (if (cddr pairs)
      `(multiple-statements
        ,@(loop for (place value) on pairs by #'cddr
                collect (r `(setf ,place ,value))))
      `(assignment ,(r (first pairs))
                   :=
                   ,(r (second pairs)))))
