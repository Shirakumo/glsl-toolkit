#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defvar *string-stream* "")
(defvar *string-index* 0)

(defun consume ()
  (prog1 (char *string-stream* *string-index*)
    (incf *string-index*)))

(defun end-of-stream-p ()
  (<= (length *string-stream*) *string-index*))

(defun advance (&optional (offset 1))
  (incf *string-index* offset))

(defun backtrack (&optional (offset 1))
  (decf *string-index* offset))

(defun peek (&optional (offset 0))
  (char *string-stream* (+ *string-index* offset)))

(defmacro with-string-input (string &body body)
  `(let ((*string-stream* ,string)
         (*string-index* 0))
     ,@body))

(defvar *rules* (make-hash-table :test 'eql))

(defun rule (name)
  (or (gethash name *rules*)
      (error "No rule named ~s is known." name)))

(defun (setf rule) (parser name)
  (setf (gethash name *rules*) parser))

(defun remove-rule (name)
  (remhash name *rules*))

(defun consume-string (string)
  (let ((start *string-index*))
    (loop for comp across string
          do (when (or (end-of-stream-p)
                       (char/= comp (consume)))
               (setf *string-index* start)
               (return NIL))
          finally (return string))))

(defun consume-any (choices)
  (unless (end-of-stream-p)
    (when (find (peek) choices)
      (consume))))

(defun consume-notany (choices)
  (unless (end-of-stream-p)
    (unless (find (peek) choices)
      (consume))))

(defun compile-rule (rule)
  (etypecase rule
    (null)
    (symbol
     (unless (ignore-errors (rule rule))
       (alexandria:simple-style-warning
        "No rule named ~s is known." rule))
     `(funcall (rule ',rule)))
    (character
     `(when (char= ,rule (peek))
        (consume)))
    (string
     `(consume-string ,rule))
    (cons
     (case (first rule)
       (and `(let ((index *string-index*))
               (or (and ,@(mapcar #'compile-rule (rest rule)))
                   (null (setf *string-index* index)))))
       (or `(or ,@(mapcar #'compile-rule (rest rule))))
       (notany `(consume-notany ',(second rule)))
       (any `(consume-any ',(second rule)))
       (when `(when ,(compile-rule (second rule))
                ,@(mapcar #'compile-rule (cddr rule))))
       (v `(v ,(compile-rule (second rule))))
       (* `(loop until (end-of-stream-p)
                 while ,(compile-rule (second rule))
                 finally (return T)))
       (? `(or ,(compile-rule (second rule)) ,(or (third rule) T)))
       (T rule)))))

(defmacro define-rule (name &body rules)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (rule ',name)
           (lambda ()
             (let ((v))
               (flet ((v (value)
                        (when value (setf v value))))
                 (or v
                     ,(compile-rule `(or ,@rules)))))))
     ',name))

(defmacro define-struct (name rule &body transform)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (rule ',name)
           (lambda ()
             (let ((v))
               (flet ((v (value)
                        (when value (push value v) value)))
                 (when ,(compile-rule rule)
                   (setf v (nreverse v))
                   ,(if transform
                        `(progn ,@transform)
                        `(list* ',name v)))))))
     ',name))

(defun parse (input &optional toplevel-rule)
  (with-string-input input
    (funcall (rule toplevel-rule))))
