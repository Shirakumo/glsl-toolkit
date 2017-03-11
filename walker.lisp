#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defstruct (environment (:conc-name NIL)
                        (:constructor %make-environment)
                        (:copier NIL))
  (root NIL)
  (bindings (make-hash-table :test 'equal)))

(defun binding (name environment)
  (gethash name (bindings environment)))

(defun (setf binding) (value name environment)
  (setf (gethash name (bindings environment)) value))

(defun make-environment (&optional parent)
  (let ((environment (%make-environment)))
    (cond (parent
           (setf (root environment) (root parent))
           (loop for k being the hash-keys of (bindings parent)
                 for v being the hash-values of (bindings parent)
                 do (setf (binding k environment) v)))
          (T
           (setf (root environment) environment)
           ;; FIXME: inject standard function and variable defs
           ))
    environment))

(defun constant-p (value environment)
  (declare (ignore environment))
  (or (integerp value)
      (floatp value)
      (and (consp value) (eql 'unsigned-int (first value)))
      (eql value :true)
      (eql value :false)))

(defun variable-p (value environment)
  (let ((binding (binding value environment)))
    (and binding
         (eql :variable (first binding)))))

(defun function-p (value environment)
  (let ((binding (binding value environment)))
    (and binding
         (eql :function (first binding)))))

(defun expression-p (value environment)
  )

(defun statement-p (value environment)
  )

(defun global-identifier-p (value environment)
  (not (null (binding value (root environment)))))

(defun local-identifier-p (value environment)
  (not (eql (binding value environment)
            (binding value (root environment)))))

(defun control-flow-p (value environment)
  (declare (ignore environment))
  (and (consp value)
       (find (first value) '(selection-statement
                             case-label
                             switch-statement
                             while-statement
                             do-statement
                             for-statement
                             continue
                             break
                             return
                             discard))))

(defun walk (ast function &optional (environment (make-environment)))
  (walk-inner ast ast function environment))

(defun walk-inner (ast context function environment)
  (flet ((walk (node &optional (environment environment))
           (walk-inner node ast function environment)))
    (etypecase ast
      ((or integer float keyword string null (eql #.no-value))
       (funcall function ast context environment))
      (cons
       (with-restructuring-case (funcall function ast context environment)
         (unsigned-int (int)
          int
          NIL)
         (preprocessor-directive (directive)
          directive
          NIL)
         (modified-reference (expression &rest modifiers)
          (walk expression)
          (mapcar* #'walk modifiers))
         (field-modifier (identifier)
          (walk identifier)
          NIL)
         (array-modifier (expression)
          (walk expression)
          NIL)
         (increment-modifier ()
          NIL)
         (decrement-modifier ()
          NIL)
         (call-modifier (&rest values)
          (mapcar* #'walk values))
         ((same-+ negation inversion bit-inversion
           prefix-increment prefix-decrement) (expression)
          (walk expression)
          NIL)
         ((multiplication division modulus addition
           subtraction left-shift right-shift less-than
           greater-than less-equal-than greater-equal-than
           equal not-equal bitwise-and exclusive-or
           inclusive-or logical-and logical-xor logical-or) (left right)
          (walk left)
          (walk right)
          NIL)
         (conditional (condition expression else)
          (walk condition)
          (walk expression)
          (walk else)
          NIL)
         (assignment (place op value)
          (walk place)
          op
          (walk value)
          NIL)
         (multiple-expressions (&rest expressions)
          (mapcar* #'walk expressions))
         (function-declaration (prototype)
          (walk prototype)
          NIL)
         (function-prototype (qualifier specifier identifier &rest parameters)
          qualifier
          specifier
          (progn (setf (binding identifier environment)
                       (list :function qualifier specifier parameters))
                 (walk identifier))
          parameters)
         (precision-declarator (precision type)
          precision
          type
          NIL)
         (variable-declaration (qualifier specifier &rest initializers)
          qualifier specifier
          (loop for (identifier array init) in initializers
                do (setf (binding identifier environment)
                         (list :variable qualifier specifier array))
                for item = (walk identifier)
                when item collect (list item array init)))
         (layout-qualifier (&rest ids)
          (mapcar #'walk ids))
         (layout-qualifier-id (identifier &optional value)
          (walk identifier)
          (when value (list (walk value))))
         (type-qualifier (&rest qualifiers)
          qualifiers)
         (subroutine-qualifier (&optional type-name)
          type-name
          NIL)
         (type-specifier (type &optional array)
          type
          (when array (list array)))
         (array-specifier (&rest specifiers)
          specifiers)
         (type-name (identifier)
          (walk identifier)
          NIL)
         (struct-specifier (identifier)
          (walk identifier)
          NIL)
         (struct-declaration (identifier &rest declarators)
          (walk identifier)
          (mapcar* #'walk declarators))
         (struct-declarator (qualifier specifier &rest fields)
          qualifier
          specifier
          (mapcar* #'walk fields))
         (array-initializer (&rest initializers)
          (mapcar* #'walk initializers))
         (multiple-statements (&rest statements)
          (mapcar* #'walk statements))
         (compound-statement (&rest statements)
          (let ((environment (make-environment environment)))
            (loop for statement in statements
                  for item = (walk statement environment)
                  when item collect item)))
         (selection-statement (expression statement &optional else)
          (walk expression)
          (walk statement)
          (when else (list (walk else))))
         (condition-declarator (qualifier specifier identifier initializer)
          qualifier
          specifier
          (progn (setf (binding identifier environment)
                       (list :variable qualifier specifier NIL))
                 (walk identifier))
          (walk initializer)
          NIL)
         (switch-statement (expression statement)
          (walk expression)
          (walk statement)
          NIL)
         (case-label (expression)
          (if (eql expression :default)
              :default
              (walk expression))
          NIL)
         (while-statement (condition statement)
          (walk condition)
          (walk statement)
          NIL)
         (do-statement (statement expression)
           (walk statement)
           (walk expression)
           NIL)
         (for-statement (declaration condition expression statement)
          (walk declaration)
          (walk condition)
          (walk expression)
          (walk statement)
          NIL)
         ((continue break discard) ())
         (return (&optional value)
          (when value (list (walk value))))
         (function-definition (prototype statement)
          (walk prototype)
          (walk statement)
          NIL)
         (shader (&rest items)
          (mapcar* #'walk items)))))))
