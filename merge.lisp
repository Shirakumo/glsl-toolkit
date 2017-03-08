#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defvar *unique-counter* 0)

(defun uniquify (&optional name)
  (format NIL "__~@[~a_~]~d" name *unique-counter*))

(defun matching-qualifiers-p (a b)
  (let ((irrelevant '(:highp :mediump :lowp :invariant :precise :smooth :flat :noperspective)))
    (null (set-difference
           (set-difference a irrelevant)
           (set-difference b irrelevant)
           :test #'equal))))

(defun matching-specifiers-p (a b)
  (null (set-difference a b :test #'equal)))

(defun find-matching-declaration (declaration declarations)
  )

(defun handle-global-identifier (identifier context environment global-env)
  (flet ((store-identifier (from &optional (to from))
           (setf (gethash from global-env)
                 (if (gethash from global-env)
                     (uniquify to)
                     to))))
    (case (first context)
      ;; Variable handling is a bit complicated because we want to
      ;; ensure that duplicate definitions of variables that are
      ;; referring to "the same thing" are merged together rather
      ;; than delegated to unique fields.
      ;;
      ;; We must merge fields marked as `in`, `inout`, `out`, and
      ;; `uniform`. This must be in accordance to the GLSL spec,
      ;; which can be read here:
      ;; https://www.khronos.org/opengl/wiki/Shader_Compilation#Interface_matching
      (variable-declaration
       (destructuring-bind (qualifiers specifiers &rest initializers) (rest context)
         (destructuring-bind (identifier array init) (find identifier initializers
                                                           :key #'first :test #'equal)
           (if (gethash identifier global-env)
               (cond ((find :uniform qualifiers)     
                      (error "Previous variable declaration of name ~s clobbers uniform declaration."
                             identifier))
                     ((find-any '(:in :out :inout) qualifiers)
                      (find-matching-declaration
                       (list qualifiers specifiers identifier array init)
                       (gethash 'declarations global-env)))
                     (T
                      (setf (gethash identifier global-env) (uniquify identifier))))
               (setf (gethash identifier global-env) identifier)))))
      ;; We assume all struct and function declarations are different
      ;; and not shared across blocks, and thus just overwrite the
      ;; declaration with a new, unique name if it already existed.
      ;;
      ;; This means that we cannot catch potential internal
      ;; consistency faults, such as two function declarations within
      ;; the same file or an undeclared variable reference in a later
      ;; block referring to an earlier block's function declaration.
      ;;
      ;; For now, we consider this merge strategy to only guarantee
      ;; valid results for already valid shader parts.
      (function-prototype
       (store-identifier identifier))
      (struct-declaration
       (store-identifier `(:struct ,identifier)))
      (struct-specifier
       (gethash `(:struct ,identifier) global-env))
      (T
       (or (gethash identifier global-env)
           identifier)))))

(defun merge-shaders (&rest shaders)
  (let ((*unique-counter* 0))
    (with-output-to-string (out)
      (let ((global-env (make-hash-table :test 'equal)))
        (setf (gethash "main" global-env) "main")
        (flet ((walker (ast context environment)
                 (if (global-identifier-p ast environment)
                     (handle-global-identifier ast context environment global-env)
                     ast)))
          (loop for shader in shaders
                for *unique-counter* from 0
                do (serialize (walk (parse shader) #'walker) out)))
        (serialize `(shader
                     (function-definition
                      (function-prototype
                       ,no-value :void "main")
                      (compound-statement
                       ,@(loop for shader in shaders
                               for *unique-counter* from 0
                               collect `(modified-reference ,(uniquify "main") (call-modifier))))))
                   out)))))
