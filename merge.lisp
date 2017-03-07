#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defvar *unique-counter* 0)

(defun uniquify (&optional name)
  (format NIL "__~@[~a_~]~d" name (incf *unique-counter*)))

(defun handle-global-identifier (identifier context environment global-env)
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
         (when (and (find :uniform qualifiers)
                    (gethash identifier global-env))
           (error "Previous variable declaration of name ~s clobbers uniform declaration."
                  identifier))
         )))
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
     (setf (gethash identifier global-env)
           (if (gethash identifier global-env)
               (uniquify identifier)
               identifier)))
    (struct-declaration
     (setf (gethash `(:struct ,identifier) global-env)
           (if (gethash `(:struct ,identifier) global-env)
               (uniquify identifier)
               identifier)))
    (struct-specifier
     (gethash `(:struct ,identifier) global-env))
    (T
     (or (gethash identifier global-env)
         identifier))))

(defun merge-shaders (&rest shaders)
  (let ((*unique-counter* 0))
    (with-output-to-string (out)
      (let ((global-env (make-hash-table :test 'equal)))
        (setf (gethash "main" global-env) "main")
        (flet ((walker (ast context environment)
                 (if (global-identifier-p ast environment)
                     (handle-global-identifier ast context environment global-env)
                     ast)))
          (dolist (shader shaders)
            (serialize (walk (parse shader) #'walker) out)))))))
