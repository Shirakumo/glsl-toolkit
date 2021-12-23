#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defun transform-to-gles (source version)
  (declare (ignore version))
  (labels ((replace-parts (statement &rest parts)
             (loop for part in statement
                   for rep = (getf parts part part)
                   collect rep))
           (transform (statement)
             (etypecase statement
               (cons
                (case (first statement)
                  (variable-declaration
                   (mapcar #'transform statement))
                  (type-qualifier
                   (append (replace-parts statement :in :varying :out :varying)
                           (unless (find-any '(:mediump :highp :lowp :uniform) statement)
                             '(:mediump))))
                  (T
                   statement)))
               ((or symbol string)
                statement))))
    (transform source)))

(defun transform-to-core (source version)
  (error "IMPLEMENT"))

(defun transform (source profile version)
  (let ((shader (etypecase source
                  (string (parse source))
                  (cons source)))
        (transform (ecase profile
                     (:es (lambda (statement) (transform-to-gles statement version)))
                     (:core (lambda (statement) (transform-to-core statement version)))
                     ((NIL) #'identity))))
    `(shader
      ,@(loop for statement in (rest shader)
              collect (funcall transform statement)))))
