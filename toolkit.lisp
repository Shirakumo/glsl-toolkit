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

(defmacro with-restructuring-case (object &body cases)
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (with-object-case ,o
         ,@(loop for (type args . body) in cases
                 collect `(,type ,args
                                 (list* (car ,o)
                                        ,@body)))))))

(trivial-indent:define-indentation with-restructuring-case (4 &rest (&whole 2 (&whole 1 &rest 1) 4 &rest 1)))

(defun mapcar* (function list)
  (loop for item in list
        for result = (funcall function item)
        when result collect result))

(defun find-any (choices sequence)
  (find choices sequence :test (lambda (a b) (find b a))))
