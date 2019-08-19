#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defvar *serialize-stream*)

(defun serialize (part &optional to)
  (etypecase to
    (null
     (with-output-to-string (*serialize-stream*)
       (serialize-part part)))
    ((eql T)
     (let ((*serialize-stream* *standard-output*))
       (serialize-part part)
       to))
    (stream
     (let ((*serialize-stream* to))
       (serialize-part part)
       to))
    (pathname
     (with-open-file (*serialize-stream* to :direction :output)
       (serialize-part part)
       to))))

(defun sformat (string &rest args)
  (format *serialize-stream* "~?" (compile-format-string string) args))

(define-compiler-macro sformat (string &rest args)
  `(format *serialize-stream* ,(compile-format-string string) ,@args))

(defun %format-object (s a cp at)
  (declare (ignore cp at))
  (let ((*serialize-stream* s))
    (serialize-part a)))

(defvar *indent* 0)

(defmacro with-indentation ((&optional (step 2)) &body body)
  `(let ((*indent* (+ ,step *indent*)))
     ,@body))

(defun indent (&optional (offset 0))
  (fresh-line *serialize-stream*)
  (format *serialize-stream* "~v{ ~}" (+ *indent* offset) '(0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-format-string (string)
    (with-output-to-string (out)
      (loop for i from 0 below (length string)
            do (cond ((and (char= #\~ (char string i))
                           (char= #\o (char string (1+ i))))
                      (write-string "~/ORG.SHIRAKUMO.TRIAL.GLSL::%FORMAT-OBJECT/" out)
                      (incf i))
                     (T
                      (write-char (char string i) out)))))))

(defvar *serializers* (make-hash-table :test 'eql))

(defun serializer (type)
  (gethash type *serializers*))

(defun (setf serializer) (function type)
  (setf (gethash type *serializers*) function))

(defun remove-serializer (type)
  (remhash type *serializers*))

(defmacro define-serializer (type (object) &body body)
  `(progn (setf (serializer ',type)
                (lambda (,object)
                  ,@body))
          ',type))

(defmacro define-serialization (type args &body body)
  (let ((object (gensym "OBJECT")))
    `(define-serializer ,type (,object)
       (destructuring-bind ,args (rest ,object)
         ,@body))))

(defun serialize-part (part)
  (etypecase part
    (integer
     (sformat "~d" part))
    (float
     (sformat "~f~@[lf~]" part (typep part 'double-float)))
    ((eql :\;))
    (keyword
     (sformat "~a" (find part *glsl-keywords* :test #'string-equal)))
    (string
     (sformat "~a" part))
    (null)
    ((eql #.no-value))
    (cons
     (funcall (or (serializer (first part))
                  (error "Cannot serialize AST-object of type ~s."
                         (first part)))
              part))))

(define-serialization unsigned-int (int)
  (serialize-part int)
  (sformat "u"))

(define-serialization preprocessor-directive (directive)
  (sformat "~&~a~%" directive))

(define-serialization modified-reference (expression &rest modifiers)
  (sformat "~o~{~o~}" expression modifiers))

(define-serialization field-modifier (identifier)
  (sformat ".~o" identifier))

(define-serialization array-modifier (expression)
  (sformat "[~o]" expression))

(define-serialization increment-modifier ()
  (sformat "++"))

(define-serialization decrement-modifier ()
  (sformat "--"))

(define-serialization call-modifier (&rest values)
  (sformat "(~{~o~^, ~})" values))

(define-serialization same-+ (expression)
  (sformat "+~o" expression))

(define-serialization negation (expression)
  (sformat "-~o" expression))

(define-serialization inversion (expression)
  (sformat "!~o" expression))

(define-serialization bit-inversion (expression)
  (sformat "~~~o" expression))

(define-serialization prefix-increment (expression)
  (sformat "++~o" expression))

(define-serialization prefix-decrement (expression)
  (sformat "--~o" expression))

(define-serialization multiplication (left right)
  (sformat "(~o * ~o)" left right))

(define-serialization division (left right)
  (sformat "(~o / ~o)" left right))

(define-serialization modulus (left right)
  (sformat "(~o % ~o)" left right))

(define-serialization addition (left right)
  (sformat "(~o + ~o)" left right))

(define-serialization subtraction (left right)
  (sformat "(~o - ~o)" left right))

(define-serialization left-shift (left right)
  (sformat "(~o << ~o)" left right))

(define-serialization right-shift (left right)
  (sformat "(~o >> ~o)" left right))

(define-serialization less-than (left right)
  (sformat "(~o < ~o)" left right))

(define-serialization greater-than (left right)
  (sformat "(~o > ~o)" left right))

(define-serialization less-equal-than (left right)
  (sformat "(~o <= ~o)" left right))

(define-serialization greater-equal-than (left right)
  (sformat "(~o >= ~o)" left right))

(define-serialization equal (left right)
  (sformat "(~o == ~o)" left right))

(define-serialization not-equal (left right)
  (sformat "(~o != ~o)" left right))

(define-serialization bitwise-and (left right)
  (sformat "(~o & ~o)" left right))

(define-serialization exclusive-or (left right)
  (sformat "(~o ^ ~o)" left right))

(define-serialization inclusive-or (left right)
  (sformat "(~o | ~o)" left right))

(define-serialization logical-and (left right)
  (sformat "(~o && ~o)" left right))

(define-serialization logical-xor (left right)
  (sformat "(~o ^^ ~o)" left right))

(define-serialization logical-or (left right)
  (sformat "(~o || ~o)" left right))

(define-serialization conditional (condition expression else)
  (sformat "~o? ~o :~o" condition expression else))

(define-serialization assignment (place op value)
  (sformat "~o ~a ~o" place op value))

(define-serialization multiple-expressions (&rest expressions)
  (sformat "~{~o~^, ~}" expressions))

(define-serialization function-declaration (prototype)
  (sformat "~o" prototype))

(define-serialization function-prototype (qualifier specifier identifier &rest parameters)
  (sformat "~o~o ~o(~{~{~o~^ ~}~^, ~})"
           qualifier specifier identifier parameters))

(define-serialization precision-declarator (precision type)
  (sformat "precision ~o ~o" precision type))

(define-serialization variable-declaration (qualifier specifier identifier array &optional initializer)
  (sformat "~o~o ~o~o~@[ = ~o~]" qualifier specifier identifier array initializer))

(define-serialization layout-qualifier (&rest ids)
  (sformat "layout(~{~o~^, ~})" ids))

(define-serialization layout-qualifier-id (identifier &optional value)
  (sformat "~o~@[ = ~o~]" identifier value))

(define-serialization type-qualifier (&rest qualifiers)
  (sformat "~{~o ~}" qualifiers))

(define-serialization subroutine-qualifier (&optional type-name)
  (sformat "subroutine~@[(~o)~]" type-name))

(define-serialization type-specifier (type &optional array)
  (sformat "~o~@[~o~]" type array))

(define-serialization array-specifier (&rest specifiers)
  (sformat "~:[[]~;~:*~{[~o]~}~]" specifiers))

(define-serialization type-name (identifier)
  (sformat "~o" identifier))

(define-serialization struct-specifier (identifier)
  (sformat "struct ~o" identifier))

(define-serialization struct-declaration (identifier instance &rest declarators)
  (with-indentation ()
    (sformat "struct ~o{~{~o~}" identifier declarators))
  (indent) (sformat "} ~o" instance))

(define-serialization struct-declarator (qualifier specifier identifier &optional array)
  (indent) (sformat "~o~o ~o~o;" qualifier specifier identifier array))

(define-serialization interface-declaration (qualifier identifier instance &rest declarators)
  (cond (identifier
         (with-indentation ()
           (sformat "~o~o{~{~o~}" qualifier identifier declarators))
         (indent) (sformat "} ~o" instance))
        (T
         (sformat "~o" qualifier))))

(define-serialization instance-name (identifier &optional array)
  (sformat "~o~o" identifier array))

(define-serialization array-initializer (type &optional initializer &rest initializers)
  (sformat "~o[](~@[~o~{, ~o~}~])" type initializer initializers))

(define-serialization multiple-statements (&optional statement &rest statements)
  (when statement
    (sformat "~o" statement)
    (loop for statement in statements
          do (sformat ";") (indent) (sformat "~o" statement))))

(define-serialization compound-statement (&rest statements)
  (sformat "{")
  (with-indentation ()
    (dolist (statement statements)
      (cond ((preprocessor-p statement NIL)
             (sformat "~o" statement))
            ((eql statement :\;))
            ((and (listp statement) (eql 'case-label (car statement)))
             (indent -2) (sformat "~o" statement))
            (T
             (indent) (sformat "~o;" statement)))))
  (indent) (sformat "}"))

(define-serialization selection-statement (expression statement &optional else)
  (sformat "if(~o)~o~@[else~o~]" expression statement else))

(define-serialization condition-declarator (qualifier specifier identifier initializer)
  (sformat "~o~o ~o = ~o" qualifier specifier identifier initializer))

(define-serialization switch-statement (expression statement)
  (sformat "switch(~o)~o" expression statement))

(define-serialization case-label (case)
  (if (eql :default case)
      (sformat "default: ")
      (sformat "case ~o: " case)))

(define-serialization while-statement (condition statement)
  (sformat "while(~o)~o" condition statement))

(define-serialization do-statement (statement expression)
  (sformat "do~o" statement)
  (indent) (sformat "while(~o)" expression))

(define-serialization for-statement (declaration condition expression statement)
  (sformat "for(~o; ~o; ~o)~o" declaration condition expression statement))

(define-serialization continue ()
  (sformat "continue"))

(define-serialization break ()
  (sformat "break"))

(define-serialization return (&optional value)
  (sformat "return~@[ ~o~]" value))

(define-serialization discard ()
  (sformat "discard"))

(define-serialization function-definition (prototype statement)
  (sformat "~%~o~o" prototype statement))

(define-serialization shader (&rest items)
  (dolist (item items)
    (cond ((preprocessor-p item NIL)
           (serialize-part item))
          (T
           (indent)
           (serialize-part item)
           (unless (eql 'function-definition (first item))
             (sformat ";"))))))
