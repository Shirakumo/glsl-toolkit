#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

;;; FIXME: this sucks. Ideally we'd be able to define a preprocessor class
;;;        and use class-based dispatch. But that would require having an AST
;;;        composed out of instances, rather than lists.

(defun preprocess (source &key include-resolution)
  (let ((shader (etypecase source
                  ((or string pathname) (parse source))
                  (cons source)))
        (accumulator ())
        (parts ())
        (section :global))
    (flet ((finish-section ()
             (setf (getf parts section) (append '(glsl-toolkit:shader)
                                                (getf parts :global)
                                                (nreverse accumulator)))))
      (dolist (form (rest shader))
        (cond ((and (eql 'preprocessor-directive (first form))
                    (starts-with "#section " (second form)))
               (finish-section)
               (setf section (let ((name (subseq (second form) (length "#section "))))
                               (cond ((string-equal name "VERTEX_SHADER") :vertex-shader)
                                     ((string-equal name "FRAGMENT_SHADER") :fragment-shader)
                                     ((string-equal name "COMPUTE_SHADER") :compute-shader)
                                     ((string-equal name "TESS_CONTROL_SHADER") :tess-control-shader)
                                     ((string-equal name "TESS_EVALUATION_SHADER") :tess-evaluation-shader)
                                     ((string-equal name "GEOMETRY_SHADER") :geometry-shadder)
                                     (T (error "Unknown shader section: ~s" name)))))
               (setf accumulator ()))
              ((and (eql 'preprocessor-directive (first form))
                    (starts-with "#include " (second form)))
               (let ((include (funcall include-resolution (subseq (second form) (length "#include ")))))
                 (dolist (form (if (eql 'shader (first include))
                                   (rest include)
                                   include))
                   (push form accumulator))))
              (T
               (push form accumulator))))
      (finish-section))
    (cond ((cddr parts)
           (remf parts :global)
           parts)
          (T
           (getf parts :global)))
    parts))

(defun transform-to-gles (version ast ctx env)
  (destructuring-bind (major minor) version
    (declare (ignore minor))
    (labels ((replace-parts (statement &rest parts)
               (loop for part in statement
                     for rep = (getf parts part part)
                     collect rep))
             (type-qualifier (qualifier)
               (if (listp qualifier)
                   (case (first qualifier)
                     (type-qualifier
                      (append (if (< major 3)
                                  (if (find 'layout-qualifier qualifier :key #'unlist)
                                      (replace-parts qualifier (second qualifier) :attribute)
                                      (replace-parts qualifier :in :varying :out :varying))
                                  qualifier)
                              (unless (find-any '(:mediump :highp :lowp) qualifier)
                                '(:mediump)))))
                   '(type-qualifier :mediump))))
      (typecase ast
        (cons
         (case (first ast)
           (variable-declaration
            (destructuring-bind (qualifier specifier identifier array &optional initializer)
                (rest ast)
              (list (first ast)
                    (type-qualifier qualifier)
                    specifier
                    identifier
                    array
                    (when (eq qualifier no-value)
                      initializer))))
           (function-prototype
            (destructuring-bind (qualifier specifier identifier &rest parameters)
                (rest ast)
              (list* (first ast)
                     (type-qualifier qualifier)
                     specifier
                     identifier
                     (loop for parameter in parameters
                           collect (if (find 'type-qualifier parameter :key #'unlist)
                                       parameter
                                       `((type-qualifier :mediump) ,@parameter))))))
           (T
            ast)))
        (T
         ast)))))

(defun transform-to-core (version ast ctx env)
  (error "IMPLEMENT"))

(defun transform (source profile version)
  (let* ((shader (etypecase source
                   ((or string pathname) (parse source))
                   (cons source)))
         (shader (walk shader (ecase profile
                                (:es (lambda (ast ctx env) (transform-to-gles version ast ctx env)))
                                (:core (lambda (ast ctx env) (transform-to-core version ast ctx env)))
                                ((NIL) #'identity)))))
    (etypecase source
      (string (serialize shader))
      (cons shader))))
