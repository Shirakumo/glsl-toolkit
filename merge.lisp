#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defvar *unique-counter* 0)

(defun uniquify (table &optional name)
  (cl-ppcre:register-groups-bind (realname) ("^_GLSLTK_(.*)_\\d+$" name)
    (setf name realname))
  (loop for ident = (format NIL "_GLSLTK_~@[~a_~]~d" name (incf *unique-counter*))
        unless (gethash ident table)
        do (return ident)))

(defun matching-qualifiers-p (a b)
  (let ((irrelevant '(:highp :mediump :lowp :invariant :precise :smooth :flat :noperspective)))
    (null (set-difference
           (set-difference a irrelevant)
           (set-difference b irrelevant)
           :test #'equal))))

(defun matching-specifiers-p (a b)
  (equal a b))

(defun matching-declarators-p (a b)
  (and (matching-qualifiers-p (first a) (first b))
       (matching-specifiers-p (second a) (second b))
       (equal (fourth a) (fourth a))))

(defun find-layout-qualifier (qualifiers)
  (find 'layout-qualifier qualifiers :key (lambda (a) (if (listp a) (first a) a))))

(defun find-direction-qualifier (qualifiers)
  (unless (eql no-value qualifiers)
    (find-any '(:in :out :inout :uniform) qualifiers)))

(defun find-matching-layout-declaration (qualifiers declarations)
  (find (find-layout-qualifier qualifiers)
        (loop with direction = (find-direction-qualifier qualifiers)
              for declaration in declarations
              when (find direction (first declaration))
              collect declaration)
        :test #'equal :key (lambda (a) (find-layout-qualifier (first a)))))

;; See https://www.khronos.org/opengl/wiki/Shader_Compilation#Interface_matching
;; it has some notes on how variables are matched up between shader stages.
;; We imitate that behaviour, to a degree. We don't match up the same types,
;; as that would probably lead to confusing merges in most cases.
(defun handle-declaration (ast context environment global-env)
  (declare (ignore context))
  (unless (root-environment-p environment)
    (return-from handle-declaration ast))
  (flet ((store-identifier (from &optional (to from))
           (setf (gethash from global-env)
                 (if (loop for v being the hash-values of global-env
                           thereis (equal v from))
                     (uniquify global-env to)
                     to))))
    (case (first ast)
      ((function-definition function-declaration)
       (let ((ident (fourth (second ast))))
         (cond ((string= ident "main")
                (push (setf (gethash ident global-env) (uniquify global-env ident))
                      (gethash 'main global-env)))
               (T
                (store-identifier ident))))
       ast)
      (struct-declaration
       (store-identifier `(:struct ,(second ast)))
       ast)
      (precision-declaration
       ast)
      ;; FIXME!!!!
      (interface-declaration
       ast)
      (variable-declaration
       (cond ((find-direction-qualifier (second ast))
              (destructuring-bind (qualifiers specifiers identifier array &optional init) (rest ast)
                (cond ((find-layout-qualifier qualifiers)
                       (let ((matching (find-matching-layout-declaration
                                        qualifiers
                                        (gethash 'declarations global-env))))
                         (cond ((not matching)
                                (push (rest ast) (gethash 'declarations global-env))
                                ast)
                               ((matching-declarators-p matching (rest ast))
                                (unless (equal init (fifth matching))
                                  (warn "Mismatched initializers between duplicate variable declarations:~%  ~a~%  ~a"
                                        (serialize `(variable-declaration ,@matching) NIL)
                                        (serialize ast NIL)))
                                (setf (gethash identifier global-env) (third matching))
                                (setf (binding identifier environment) (list :variable qualifiers specifiers array))
                                ;; We already have this declaration.
                                NIL)
                               (T
                                (error "Found two mismatched declarations with the same layout qualifier:~%  ~a~%  ~a"
                                       (serialize `(variable-declaration ,@matching) NIL)
                                       (serialize ast NIL))))))
                      ((gethash identifier global-env)
                       (let ((matching (find identifier
                                             (gethash 'declarations global-env)
                                             :test #'equal :key #'third)))
                         (cond ((matching-declarators-p matching (rest ast))
                                (unless (equal init (fifth matching))
                                  (warn "Mismatched initializers between duplicate variable declarations:~%  ~a~%  ~a"
                                        (serialize `(variable-declaration ,@matching) NIL)
                                        (serialize ast NIL)))
                                (setf (gethash identifier global-env) (third matching))
                                (setf (binding identifier environment) (list :variable qualifiers specifiers array))
                                ;; We /probably/ already have this declaration.
                                NIL)
                               (T
                                (warn "Found two mismatched declarations with the same identifier:~%  ~a~%  ~a"
                                      (serialize `(variable-declaration ,@matching) NIL)
                                      (serialize ast NIL))
                                (store-identifier identifier)
                                ast))))
                      (T
                       (push (rest ast) (gethash 'declarations global-env))
                       (store-identifier identifier)
                       ast))))
             (T
              (store-identifier (fourth ast))
              ast))))))

(defun handle-identifier (ast context environment global-env)
  (or (when (global-identifier-p ast environment)
        (case (first context)
          (struct-specifier (gethash `(:struct ,ast) global-env))
          (struct-declarator ast)
          (field-modifier ast)
          (T (gethash ast global-env))))
      ast))

(defun split-shader-into-groups (shader)
  (let ((groups (list 'precision-declaration ()
                      'variable-declaration ()
                      'struct-declaration ()
                      'function-declaration ()
                      'function-definition ()
                      'interface-declaration ())))
    (flet ((walker (ast context environment)
             (declare (ignore context))
             (when (declaration-p ast environment)
               (push ast (getf groups (first ast))))
             ast))
      (walk shader #'walker))
    groups))

;; FIXME: track use relations to reorder definitions properly
(defun merge-shaders (shaders &key (min-version "120") profile)
  (let ((*unique-counter* 0)
        (global-env (make-hash-table :test 'equal))
        (version min-version))
    (flet ((walker (ast context environment)
             (cond ((declaration-p ast environment)
                    (handle-declaration ast context environment global-env))
                   ((stringp ast)
                    (handle-identifier ast context environment global-env))
                   ((preprocessor-p ast environment)
                    (if (starts-with "#version" (second ast))
                        (cl-ppcre:register-groups-bind (v NIL p) ("#version (\\d{3})( (.*))?" (second ast))
                          (when (or (null version) (< (parse-integer version) (parse-integer v)))
                            (setf version v))
                          (when p
                            (when (and profile (string/= profile p))
                              (warn "Incompatible OpenGL profiles requested: ~a and ~a."
                                    profile p))
                            (setf profile p))
                          NIL)
                        ast))
                   (T
                    ast))))
      (let ((results (loop for shader in shaders
                           appending (rest (walk shader #'walker)))))
        (append '(shader)
                (when version
                  `((preprocessor-directive
                     ,(format NIL "#version ~a~@[ ~a~]" version profile))))
                results
                `((function-definition
                   (function-prototype
                    ,no-value :void "main")
                   (compound-statement
                    ,@(loop for main in (nreverse (gethash 'main global-env))
                            collect `(modified-reference ,main (call-modifier)))))))))))

(defun merge-shader-sources (sources &key to (min-version "120") profile)
  (serialize (merge-shaders
              (loop for source in sources
                    collect (typecase source
                              (cons source)
                              (T (parse source))))
              :min-version min-version :profile profile)
             to))
