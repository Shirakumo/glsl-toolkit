#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defmacro with-object-case (object &body cases)
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (ecase (first ,o)
         ,@(loop for (type args . body) in cases
                 collect `(,type (destructuring-bind ,args (rest ,o)
                                   ,@body)))))))

(trivial-indent:define-indentation with-object-case (4 &rest (&whole 2 (&whole 1 &rest 1) 4 &rest 1)))
