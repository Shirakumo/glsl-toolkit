(in-package #:org.shirakumo.trial.glsl)

(defclass environment ()
  ((root :initform NIL :reader root)
   (bindings :initform (make-hash-table :test 'equal) :reader bindings)))

;; FIXME: all this stuff could as well be generic
(defun binding (name environment)
  (gethash name (bindings environment)))

(defun (setf binding) (value name environment)
  (setf (gethash name (bindings environment)) value))

(defun make-environment (&optional parent)
  (let ((environment (make-instance 'environment)))
    (cond (parent
           (setf (slot-value environment 'root) (root parent))
           (loop for k being the hash-keys of (bindings parent)
                 for v being the hash-values of (bindings parent)
                 do (setf (binding k environment) v)))
          (T
           (setf (slot-value environment 'root) environment)
           ;; FIXME: inject standard function and variable defs
           ))
    environment))

(defun root-environment-p (environment)
  (eql environment (root environment)))

(defun preprocessor-p (value environment)
  (declare (ignore environment))
  (and (consp value)
       (eql (first value) 'preprocessor-directive)))

(defun constant-p (value environment)
  (declare (ignore environment))
  (or (integerp value)
      (floatp value)
      (and (consp value) (eql 'unsigned-int (first value)))
      (eql value :true)
      (eql value :false)))

(defun declaration-p (value environment)
  (declare (ignore environment))
  (and (consp value)
       (find (first value) '(function-declaration
                             function-definition
                             variable-declaration
                             precision-declaration
                             struct-declaration
                             interface-declaration))))

(defun expression-p (value environment)
  (or (constant-p value environment)
      (identifier-p value environment)
      (and (consp value)
           (find (first value) '(assignment
                                 conditional
                                 logical-or
                                 logical-xor
                                 logical-and
                                 inclusive-or
                                 exclusive-or
                                 bitwise-and
                                 not-equal
                                 equal
                                 greater-equal-than
                                 less-equal-than
                                 greater-than
                                 less-than
                                 right-shift
                                 left-shift
                                 subtraction
                                 addition
                                 modulus
                                 division
                                 multiplication
                                 prefix-decrement
                                 prefix-increment
                                 bit-inversion
                                 inversion
                                 negation
                                 same-+
                                 modified-reference)))))

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

(defun keyword-p (value environment)
  (declare (ignore environment))
  (find value *glsl-keyword-symbols*))

(defun statement-p (value environment)
  (or (declaration-p value environment)
      (expression-p value environment)
      (control-flow-p value environment)
      (eql value :\;)))

(defun identifier-p (value environment)
  (declare (ignore environment))
  (or (keywordp value) (stringp value)))

(defun global-identifier-p (value environment)
  (not (null (binding value (root environment)))))

(defun local-identifier-p (value environment)
  (not (eql (binding value environment)
            (binding value (root environment)))))

(defun variable-identifier-p (value environment)
  (let ((binding (binding value environment)))
    (and binding
         (eql :variable (first binding)))))

(defun function-identifier-p (value environment)
  (let ((binding (binding value environment)))
    (and binding
         (eql :function (first binding)))))

(defun overload-p (prototype environment)
  (destructuring-bind (qualifier specifier identifier &rest parameters) prototype
    (declare (ignore qualifier))
    (when (eql :function (car (binding identifier environment)))
      (loop for binding on (binding identifier environment) by #'cddddr
            always (destructuring-bind (ex-qualifier ex-specifier ex-parameters) (rest binding)
                     (declare (ignore ex-qualifier))
                     ;; It's an overload if:
                     ;; 1. We have a different return type, or
                     ;; 2. We have a different number of arguments, or
                     ;; 3. The arguments have different type specifiers
                     (or (not (equal ex-specifier specifier))
                         (not (= (length parameters) (length ex-parameters)))
                         (loop for param in parameters
                               for ex-param in ex-parameters
                               thereis (or (not (equal (second param) (second ex-param)))
                                           (not (equal (fourth param) (fourth ex-param)))))))))))

(defun walk (ast function &optional (environment (make-environment)))
  (walk-part ast ast function environment))

(defun walk-part (ast context function environment)
  (etypecase ast
    ((or integer float keyword string null (eql #.no-value))
     (funcall function ast context environment))
    (cons
     (setf ast (funcall function ast context environment))
     (when ast
       (funcall (or (walker (first ast))
                    (error "Cannot walk AST-object of type ~s."
                           (first ast)))
                ast function environment)))))

(defvar *walkers* (make-hash-table :test 'eql))

(defun walker (type)
  (gethash type *walkers*))

(defun (setf walker) (function type)
  (setf (gethash type *walkers*) function))

(defun remove-walker (type)
  (remhash type *walkers*))

(defmacro define-walker (type (ast func env) &body body)
  `(progn (setf (walker ',type)
                (lambda (,ast ,func ,env)
                  ,@body))
          ',type))

(defmacro define-walking-body (type args &body body)
  (destructuring-bind (type &key (ast (gensym "AST"))
                                 (func (gensym "FUNC"))
                                 (env (gensym "ENV"))) (enlist type)
    `(define-walker ,type (,ast ,func ,env)
       (flet ((walk (node &optional (,env ,env))
                (walk-part node ,ast ,func ,env)))
         (destructuring-bind ,args (rest ,ast)
           (list* ',type
                  ,@body))))))

(defmacro define-empty-op-walker (type)
  `(define-walking-body ,type ()
     NIL))

(defmacro define-unary-op-walker (type)
  `(define-walking-body ,type (inner)
     (walk inner)
     NIL))

(defmacro define-binary-op-walker (type)
  `(define-walking-body ,type (left right)
     (walk left)
     (walk right)
     NIL))

(define-walking-body unsigned-int (int)
  int
  NIL)

(define-walking-body preprocessor-directive (directive)
  directive
  NIL)

(define-walking-body modified-reference (expression &rest modifiers)
  (walk expression)
  (mapcar* #'walk modifiers))

(define-walking-body field-modifier (identifier)
  (walk identifier)
  NIL)

(define-walking-body array-modifier (expression)
  (walk expression)
  NIL)

(define-walking-body increment-modifier ()
  NIL)

(define-walking-body decrement-modifier ()
  NIL)

(define-walking-body call-modifier (&rest values)
  (mapcar* #'walk values))

(define-unary-op-walker same-+)
(define-unary-op-walker negation)
(define-unary-op-walker inversion)
(define-unary-op-walker bit-inversion)
(define-unary-op-walker prefix-increment)
(define-unary-op-walker prefix-decrement)

(define-binary-op-walker multiplication)
(define-binary-op-walker division)
(define-binary-op-walker modulus)
(define-binary-op-walker addition)
(define-binary-op-walker subtraction)
(define-binary-op-walker left-shift)
(define-binary-op-walker right-shift)
(define-binary-op-walker less-than)
(define-binary-op-walker greater-than)
(define-binary-op-walker less-equal-than)
(define-binary-op-walker greater-equal-than)
(define-binary-op-walker equal)
(define-binary-op-walker not-equal)
(define-binary-op-walker bitwise-and)
(define-binary-op-walker exclusive-or)
(define-binary-op-walker inclusive-or)
(define-binary-op-walker logical-and)
(define-binary-op-walker logical-xor)
(define-binary-op-walker logical-or)

(define-walking-body conditional (condition expression else)
  (walk condition)
  (walk expression)
  (walk else)
  NIL)

(define-walking-body assignment (place op value)
  (walk place)
  op
  (walk value)
  NIL)

(define-walking-body multiple-expressions (&rest expressions)
  (mapcar* #'walk expressions))

(define-walking-body function-declaration (prototype)
  (walk prototype)
  NIL)

(define-walking-body (function-prototype :ast ast :env env) (qualifier specifier identifier &rest parameters)
  qualifier
  specifier
  (progn (let ((binding (list :function qualifier specifier parameters))
               (existing (binding identifier env)))
           (cond ((null existing)
                  (setf (binding identifier env) binding))
                 ((overload-p ast env)
                  (setf (binding identifier env) (append binding existing)))))
         (walk identifier))
  parameters)

(define-walking-body precision-declarator (precision type)
  precision
  type
  NIL)

(define-walking-body (variable-declaration :env env) (qualifier specifier identifier array &optional init)
  qualifier specifier
  (progn (setf (binding identifier env)
               (list :variable qualifier specifier array))
         (walk identifier))
  array (when init (list (walk init))))

(define-walking-body layout-qualifier (&rest ids)
  (mapcar #'walk ids))

(define-walking-body layout-qualifier-id (identifier &optional value)
  (walk identifier)
  (when value (list (walk value))))

(define-walking-body type-qualifier (&rest qualifiers)
  qualifiers)

(define-walking-body subroutine-qualifier (&optional type-name)
  type-name
  NIL)

(define-walking-body type-specifier (type &optional array)
  type
  (enlist array))

(define-walking-body array-specifier (&rest specifiers)
  specifiers)

(define-walking-body type-name (identifier)
  (walk identifier)
  NIL)

(define-walking-body struct-specifier (identifier)
  (walk identifier)
  NIL)

(define-walking-body struct-declaration (identifier instance &rest declarators)
  (walk identifier)
  (walk instance)
  (mapcar* #'walk declarators))

(define-walking-body struct-declarator (qualifier specifier identifier &optional array)
  qualifier
  specifier
  (walk identifier)
  (if array (list array)))

(define-walking-body interface-declaration (qualifier identifier instance &rest declarators)
  qualifier
  (walk identifier)
  (walk instance)
  (mapcar* #'walk declarators))

(define-walking-body instance-name (identifier &optional array)
  (walk identifier)
  (list
   (enlist array)))

(define-walking-body array-initializer (&rest initializers)
  (mapcar* #'walk initializers))

(define-walking-body multiple-statements (&rest statements)
  (mapcar* #'walk statements))

(define-walking-body (compound-statement :env env) (&rest statements)
  (let ((env (make-environment env)))
    (loop for statement in statements
          for item = (walk statement env)
          when item collect item)))

(define-walking-body selection-statement (expression statement &optional else)
  (walk expression)
  (walk statement)
  (when else (list (walk else))))

(define-walking-body (condition-declarator :env env) (qualifier specifier identifier initializer)
  qualifier
  specifier
  (progn (setf (binding identifier env)
               (list :variable qualifier specifier NIL))
         (walk identifier))
  (walk initializer)
  NIL)

(define-walking-body switch-statement (expression statement)
  (walk expression)
  (walk statement)
  NIL)

(define-walking-body case-label (expression)
  (if (eql expression :default)
      :default
      (walk expression))
  NIL)

(define-walking-body while-statement (condition statement)
  (walk condition)
  (walk statement)
  NIL)

(define-walking-body do-statement (statement expression)
  (walk statement)
  (walk expression)
  NIL)

(define-walking-body for-statement (declaration condition expression statement)
  (walk declaration)
  (walk condition)
  (walk expression)
  (walk statement)
  NIL)

(define-empty-op-walker continue)
(define-empty-op-walker break)
(define-empty-op-walker discard)

(define-walking-body return (&optional value)
  (when value (list (walk value))))

(define-walking-body function-definition (prototype statement)
  (walk prototype)
  (walk statement)
  NIL)

(define-walking-body shader (&rest items)
  (mapcar* #'walk items))
