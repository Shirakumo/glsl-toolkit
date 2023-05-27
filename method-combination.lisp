#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defun definition-identifier (def)
  (ecase (first def)
    (function-prototype
     (fourth def))
    ((function-declaration function-definition)
     (definition-identifier (second def)))))

(defun definition-argvars (def)
  (ecase (first def)
    (function-prototype
     (loop for arg in (cddddr def)
           collect (car (last arg))))
    ((function-declaration function-definition)
     (definition-argvars (second def)))))

(defun (setf definition-identifier) (value def)
  (ecase (first def)
    (function-prototype
     (setf (fourth def) value))
    (function-declaration
     (setf (definition-identifier (second def)) value))
    (function-definition
     (setf (definition-identifier (second def)) value))))

(defun handle-function-definition (ast env)
  (let ((identifier (definition-identifier ast)))
    (cond ((find #\@ identifier)
           (let* ((name (subseq identifier 0 (position #\@ identifier)))
                  (comb (subseq identifier (1+ (length name))))
                  (comb (cond ((string= comb "after") :after)
                              ((string= comb "before") :before)
                              ((string= comb "around") :around)
                              ((string= comb "primary") :primary)
                              (T (error "Unsupported method combination type: ~a" comb)))))
             (setf (fourth (second ast)) name)
             (push (cons comb ast) (gethash name env))))
          (T
           (push (cons :primary ast) (gethash identifier env))))))

(defun resolve-method-definitions (identifier definitions)
  (let ((parts (list :before () :after () :primary () :around ()))
        (return-variable "_GLSLTK_return"))
    (loop for (comb . ast) in definitions
          do (push ast (getf parts comb)))
    (unless (getf parts :primary)
      (error "No primary method for ~a" identifier))
    (let* ((proto (copy-tree (second (first (getf parts :primary)))))
           (return-type (third proto)))
      ;; Turn into unique function names
      (flet ((uniquify (comb defs)
               (loop for i from 1
                     for def in defs
                     do (setf (definition-identifier def) (format NIL "_~a__~(~a~)_~d" identifier comb i)))))
        (loop for (k v) on parts by #'cddr
              do (uniquify k v)
                 (setf (getf parts k) (nreverse v))))
      ;; Resolve next methods
      (setf (definition-identifier proto) (format NIL "_~a__primary" identifier))
      (labels ((emit-call (identifier args)
                 `(modified-reference ,identifier (call-modifier ,@args)))
               (resolve-next-method (def next)
                 (let ((args (definition-argvars def)))
                   (walk def (lambda (ast ctx env)
                               (declare (ignore ctx env))
                               (cond ((and (consp ast)
                                           (eql 'modified-reference (first ast))
                                           (string= "call_next_method" (second ast)))
                                      (if (null (rest (third ast)))
                                          (emit-call next args)
                                          `(modified-reference ,next ,@(cddr ast))))
                                     ((equal ast "next_method_p")
                                      (if next 1 0))
                                     (T
                                      ast)))))))
        (loop for cons on (getf parts :around)
              for next-def = (second cons)
              for next-fun = (definition-identifier (or next-def proto))
              do (setf (car cons) (resolve-next-method (car cons) next-fun)))
        (loop for cons on (getf parts :primary)
              for next-def = (second cons)
              for next-fun = (if next-def (definition-identifier next-def) NIL)
              do (setf (car cons) (resolve-next-method (car cons) next-fun)))
        ;; Construct entry function
        (when (or (getf parts :before) (getf parts :after))
          (setf (getf parts :after) (nreverse (getf parts :after)))
          (let ((args (definition-argvars proto)))
            (push `(function-definition
                    ,proto
                    (compound-statement
                     ,@(loop for def in (getf parts :before)
                             collect (emit-call (definition-identifier def) args))
                     ,(if (equal '(glsl-toolkit:type-specifier :void) return-type)
                          (emit-call (definition-identifier (first (getf parts :primary))) args)
                          `(variable-declaration no-value (type-specifier ,return-type) ,return-variable no-value
                                                 ,(emit-call (definition-identifier (first (getf parts :primary))) args)))
                     ,@(loop for def in (getf parts :after)
                             collect (emit-call (definition-identifier def) args))
                     ,@(unless (equal '(glsl-toolkit:type-specifier :void) return-type)
                         `((return ,return-variable)))))
                  (getf parts :primary))))
        (cond ((getf parts :around)
               (setf (definition-identifier (first (getf parts :around))) identifier))
              (T
               (setf (definition-identifier (first (getf parts :primary))) identifier)))
        ;; Append the definitions together
        (append (getf parts :before)
                (reverse (rest (getf parts :primary)))
                (getf parts :after)
                (list (first (getf parts :primary)))
                (nreverse (getf parts :around)))))))

(defun combine-methods (shaders)
  (let ((shaders (mapcar #'copy-tree (mapcar #'ensure-shader (enlist shaders))))
        (env (make-hash-table :test 'equal))
        (other-forms ()))
    ;; TODO: handle overloading
    (dolist (shader shaders)
      (loop for ast in (rest shader)
            do (case (first ast)
                 (function-declaration)
                 (function-definition
                  (handle-function-definition ast env))
                 (T
                  (push ast other-forms)))))
    (let ((prototypes ()))
      (loop for definitions being the hash-values of env
            for proto = (loop for (comb . def) in definitions
                              do (when (eql :primary comb)
                                   (return (second def)))) 
            do (pushnew (copy-tree proto) prototypes :key #'fourth))
      `(shader
        ,@(nreverse other-forms)
        ;; Emit declarations first to handle the reordering of function definitions
        ,@(loop for proto in (reverse prototypes)
                collect `(function-declaration ,proto))
        ,@(loop for definitions being the hash-values of env using (hash-key identifier)
                append (resolve-method-definitions identifier definitions))))))
