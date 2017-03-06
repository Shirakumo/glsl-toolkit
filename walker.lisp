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
                 do (setf (gethash k (bindings environment)) v)))
          (T
           (setf (root environment) environment)))
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

(defun global-p (value environment)
  (not (null (binding value (root environment)))))

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
  (flet ((walk (ast &optional (environment environment))
           (walk ast function environment)))
    (etypecase ast
      ((or integer float keyword string null (eql #.no-value))
       (setf ast (funcall function ast environment)))
      (cons
       (setf ast (funcall function ast environment))
       (with-object-case ast
         (unsigned-int (int)
          (declare (ignore int)))
         (preprocessor-directive (directive)
          (declare (ignore directive)))
         (modified-reference (expression &rest modifiers)
          (walk expression)
          (mapcar #'walk modifiers))
         (field-modifier (identifier)
          (walk identifier))
         (array-modifier (expression)
          (walk expression))
         (increment-modifier ())
         (decrement-modifier ())
         (call-modifier (&rest values)
          (mapcar #'walk values))
         ((same-+ negation inversion bit-inversion
           prefix-increment prefix-decrement) (expression)
          (walk expression))
         ((multiplication division modulus addition
           subtraction left-shift right-shift less-than
           greater-than less-equal-than greater-equal-than
           equal not-equal bitwise-and exclusive-or
           inclusive-or logical-and logical-xor logical-or) (left right)
          (walk left)
          (walk right))
         (conditional (condition expression else)
          (walk condition)
          (walk expression)
          (walk else))
         (assignment (place op value)
          (declare (ignore op))
          (walk place)
          (walk value))
         (multiple-expressions (&rest expressions)
          (mapcar #'walk expressions))
         (function-declaration (prototype)
          (walk prototype))
         (function-prototype (qualifier specifier identifier &rest parameters)
          (setf (binding identifier environment)
                (list :function (rest qualifier) (rest specifier) parameters))
          (walk identifier))
         (precision-declarator (precision type)
          (declare (ignore precision type)))
         (variable-declaration (qualifier specifier &rest initializers)
          (loop for (identifier array init) in initializers
                do (setf (binding identifier environment)
                         (list :variable (rest qualifier) (rest specifier) array))
                   (walk identifier)))
         (layout-qualifier (&rest ids)
          (mapcar #'walk ids))
         (layout-qualifier-id (identifier &optional value)
          (walk identifier)
          (when value (walk value)))
         (type-qualifier (&rest qualifiers)
          (declare (ignore qualifiers)))
         (subroutine-qualifier (&optional type-name)
          (declare (ignore type-name)))
         (type-specifier (type &optional array)
          (declare (ignore type array)))
         (array-specifier (&rest specifiers)
          (declare (ignore specifiers)))
         (type-name (identifier)
          (walk identifier))
         (struct-specifier (identifier)
          (walk identifier))
         (struct-declaration (identifier &rest declarators)
          (walk identifier)
          (mapcar #'walk declarators))
         (struct-declarator (qualifier specifier &rest fields)
          (declare (ignore qualifier specifier))
          (mapcar #'walk fields))
         (array-initializer (&rest initializers)
          (mapcar #'walk initializers))
         (compound-statement (&rest statements)
          (let ((environment (make-environment environment)))
            (dolist (statement statements)
              (walk statement environment))))
         (selection-statement (expression statement &optional else)
          (walk expression)
          (walk statement)
          (when else (walk else)))
         (condition-declarator (qualifier specifier identifier initializer)
          (setf (binding identifier environment)
                (list :variable (rest qualifier) (rest specifier) NIL))
          (walk identifier)
          (walk initializer))
         (switch-statement (expression statement)
          (walk expression)
          (walk statement))
         (case-label (expression)
          (unless (eql expression :default)
            (walk expression)))
         (while-statement (condition statement)
          (walk condition)
          (walk statement))
         (do-statement (statement expression)
           (walk statement)
           (walk expression))
         (for-statement (declaration condition expression statement)
          (walk declaration)
          (walk condition)
          (walk expression)
          (walk statement))
         ((continue break discard) ())
         (return (&optional value)
          (when value (walk value)))
         (function-definition (prototype statement)
          (walk prototype)
          (walk statement))
         (shader (&rest items)
          (mapcar #'walk items))))))
  ast)
