#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defvar *token-array* "")
(defvar *token-index* 0)

(macrolet ((define-novalue ()
             (let ((name 'no-value))
               `(define-symbol-macro ,name ',name))))
  (define-novalue))

(deftype index ()
  `(integer 0 ,array-dimension-limit))

(defun end-of-tokens-p ()
  (<= (length *token-array*) *token-index*))

(define-compiler-macro end-of-tokens-p ()
  `(<= (length (the vector *token-array*))
       (the index *token-index*)))

(defun advance (&optional (offset 1))
  (incf *token-index* offset))

(define-compiler-macro advance (&optional (offset 1))
  `(setf *token-index* (+ (the index *token-index*) ,offset)))

(defun backtrack (&optional (offset 1))
  (decf *token-index* offset))

(define-compiler-macro backtrack (&optional (offset 1))
  `(setf *token-index* (- (the index *token-index*) ,offset)))

(defun peek (&optional (offset 0))
  (aref *token-array* (+ *token-index* offset)))

(define-compiler-macro peek (&optional (offset 0))
  `(aref (the vector *token-array*) (+ (the index *token-index*) ,offset)))

(defun consume ()
  (prog1 (peek)
    (advance)))

(define-compiler-macro consume ()
  `(prog1 (peek)
     (advance)))

(defmacro with-token-input (vector &body body)
  `(let ((*token-array* ,vector)
         (*token-index* 0))
     ,@body))

(defun rule (name)
  (or (find-symbol (string name) '#:org.shirakumo.trial.glsl.parser.rules)
      (error "No rule named ~s is known." name)))

(defun (setf rule) (parser name)
  (let ((symbol (intern (string name) '#:org.shirakumo.trial.glsl.parser.rules)))
    (export symbol (symbol-package symbol))
    (setf (fdefinition symbol) parser)))

(defun remove-rule (name)
  (let ((symbol (intern (string name) '#:org.shirakumo.trial.glsl.parser.rules)))
    (unexport symbol (symbol-package symbol))
    (fmakunbound symbol)
    (unintern symbol (symbol-package symbol))))

(defun consume-whitespace ()
  (loop until (end-of-tokens-p)
        for char = (peek)
        do (if (or (char= char #\Space)
                   (char= char #\Newline))
               (advance)
               (return))))

(defun consume-string (string)
  (let ((start *token-index*))
    (loop for comp across string
          do (when (or (end-of-tokens-p)
                       (char/= comp (consume)))
               (setf *token-index* start)
               (return NIL))
          finally (return string))))

(defun consume-any (choices)
  (unless (end-of-tokens-p)
    (when (find (peek) choices)
      (consume))))

(defun consume-notany (choices)
  (unless (end-of-tokens-p)
    (unless (find (peek) choices)
      (consume))))

(defun compile-rule (rule)
  (etypecase rule
    (null)
    (keyword
     `(when (and (not (end-of-tokens-p)) (eq ,rule (peek)))
        (consume)))
    (symbol
     `(,(intern (string rule) '#:org.shirakumo.trial.glsl.parser.rules)))
    (character
     `(when (and (not (end-of-tokens-p)) (eql ,rule (peek)))
        (consume)))
    (string
     `(consume-string ,rule))
    (cons
     (case (first rule)
       (and `(let ((index *token-index*)
                   (preval v))
               (or (and ,@(mapcar #'compile-rule (rest rule)))
                   (prog1 NIL (setf *token-index* index
                                    v preval)))))
       (or `(or ,@(mapcar #'compile-rule (rest rule))))
       (notany `(consume-notany ',(second rule)))
       (any `(consume-any ',(second rule)))
       (when `(when ,(compile-rule (second rule))
                ,@(mapcar #'compile-rule (cddr rule))))
       (v `(v ,(compile-rule (second rule))))
       (* `(loop until (end-of-tokens-p)
                 while ,(compile-rule (second rule))
                 finally (return T)))
       (+ (compile-rule `(and ,(second rule) (* ,(second rule)))))
       (? `(or ,(compile-rule (second rule)) ,(or (third rule) no-value)))
       (! `(let ((index *token-index*)
                 (preval v))
             (prog1 ,(compile-rule (second rule))
               (setf *token-index* index
                     v preval))))
       (T rule)))))

(defmacro define-rule (name &body body)
  (let ((name (intern (string name) '#:org.shirakumo.trial.glsl.parser.rules)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ()
         (let ((v))
           ,@body))
       (export ',name '#:org.shirakumo.trial.glsl.parser.rules)
       ',name)))

(defmacro define-reference (name &body rules)
  (let ((val (gensym "VALUE")))
    `(define-rule ,name
       (flet ((v (value)
                (when value (setf v value))))
         (declare (ignorable #'v) (inline v))
         (let ((,val ,(compile-rule `(or ,@rules))))
           (or v ,val))))))

(defmacro define-object (name rule &body transform)
  `(define-rule ,name
     (flet ((v (value)
              (when value (push value v) value)))
       (declare (ignorable #'v) (inline v))
       (when ,(compile-rule rule)
         (setf v (nreverse v))
         ,(if transform
              `(progn ,@transform)
              `(list* ',name v))))))

(defun newline-p (input)
  (or (eql input #\Linefeed)
      (eql input #\Return)))

(defun normalize-shader-source (input)
  (etypecase input
    (pathname (with-open-file (stream input :direction :input)
                (normalize-shader-source stream)))
    (string (with-input-from-string (stream input)
              (normalize-shader-source stream)))
    (stream
     (with-output-to-string (output)
       (loop for char = (read-char input NIL)
             while char
             do (case char
                  ;; Handle backslash escape
                  (#\\
                   (cond ((newline-p (peek-char NIL input NIL))
                          (read-char input)
                          (when (newline-p (peek-char NIL input NIL))
                            (read-char input)))
                         (T
                          (error "Illegal backslash without newline."))))
                  ;; Handle newline behaviour and such
                  ((#\Return #\Linefeed)
                   (when (newline-p (peek-char NIL input NIL))
                     (read-char input))
                   (write-char #\Newline output))
                  ;; Handle comments
                  (#\/
                   (case (peek-char NIL input)
                     (#\/ (loop for prev = #\  then char
                                for char = (read-char input NIL)
                                until (or (not char)
                                          (and (not (char= #\\ prev))
                                               (newline-p char))))
                      (write-char #\Newline output))
                     (#\* (loop for prev = #\  then char
                                for char = (read-char input)
                                until (and (char= #\* prev)
                                           (char= #\/ char))))
                     (T (write-char char output))))
                  ;; Handle consecutive whitespace
                  ((#\Tab #\Space)
                   (loop for char = (read-char input NIL)
                         while (or (eql char #\Tab)
                                   (eql char #\Space))
                         finally (when char (unread-char char input)))
                   (write-char #\Space output))
                  ;; Handle other chars
                  (T (write-char char output))))))))

(defun lex (input &optional (toplevel-rule 'tokenize))
  (with-token-input (normalize-shader-source input)
    (funcall (rule toplevel-rule))))

(defun parse (input &optional (toplevel-rule 'shader))
  (etypecase input
    ((or string stream pathname)
     (parse (lex input) toplevel-rule))
    (list
     (parse (coerce input 'vector) toplevel-rule))
    (vector
     (with-token-input input
       (funcall (rule toplevel-rule))))))

(defvar *traced* (make-hash-table :test 'eql))
(defvar *trace-level* 0)

(defun call-traced-function (name)
  (format T "~&~v{ ~}~2:* :~a > ~a : ~a~%"
          *trace-level* *token-index* name)
  (let* ((value (let ((*trace-level* (1+ *trace-level*)))
                  (funcall (gethash name *traced*)))))
    (format T "~&~v{ ~}~2:* :~a < ~a : ~a ~a~%"
            *trace-level* *token-index* name value)
    value))

(defun trace-parse-func (name)
  (unless (gethash name *traced*)
    (setf (gethash name *traced*) (fdefinition name))
    (setf (fdefinition name)
          (lambda () (call-traced-function name)))))

(defun untrace-parse-func (name)
  (when (gethash name *traced*)
    (setf (fdefinition name) (gethash name *traced*))
    (remhash name *traced*)))

(defun trace-parse ()
  (do-symbols (symbol '#:org.shirakumo.trial.glsl.parser.rules)
    (when (fboundp symbol) (trace-parse-func symbol))))

(defun untrace-parse ()
  (do-symbols (symbol '#:org.shirakumo.trial.glsl.parser.rules)
    (when (fboundp symbol) (untrace-parse-func symbol))))
