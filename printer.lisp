#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

(defvar *serialize-stream*)

(defun serialize (part &optional to)
  (etypecase to
    (null
     (with-output-to-string (*serialize-stream*)
       (serialize-part part)))
    ((eql T)
     (let ((*serialize-stream* *standard-output*))
       (serialize-part part)
       to))
    (stream
     (let ((*serialize-stream* to))
       (serialize-part part)
       to))
    (pathname
     (with-open-file (*serialize-stream* to :direction :output)
       (serialize-part part)
       to))))

(defun sformat (string &rest args)
  (format *serialize-stream* "~?" (compile-format-string string) args))

(define-compiler-macro sformat (string &rest args)
  `(format *serialize-stream* ,(compile-format-string string) ,@args))

(defun %format-object (s a cp at)
  (declare (ignore cp at))
  (let ((*serialize-stream* s))
    (serialize-part a)))

(defvar *indent* 0)

(defmacro with-indentation ((&optional (step 2)) &body body)
  `(let ((*indent* (+ ,step *indent*)))
     ,@body))

(defun indent ()
  (fresh-line *serialize-stream*)
  (format *serialize-stream* "~v{ ~}" *indent* 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-format-string (string)
    (with-output-to-string (out)
      (loop for i from 0 below (length string)
            do (cond ((and (char= #\~ (char string i))
                           (char= #\o (char string (1+ i))))
                      (write-string "~/ORG.SHIRAKUMO.TRIAL.GLSL.PARSER::%FORMAT-OBJECT/" out)
                      (incf i))
                     (T
                      (write-char (char string i) out)))))))

(defmacro with-object-case (object &body cases)
  (let ((o (gensym "O")))
    `(let ((,o ,object))
       (ecase (first ,o)
         ,@(loop for (type args . body) in cases
                 collect `(,type (destructuring-bind ,args (rest ,o)
                                   ,@body)))))))

(trivial-indent:define-indentation with-object-case (4 &rest (&whole 2 2 4 &rest 1)))

(defun serialize-part (part)
  (etypecase part
    (integer
     (sformat "~d" part))
    (float
     (sformat "~f~@[lf~]" part (typep part 'double-float)))
    (keyword
     (sformat "~a"
              (find part
                    '("writeonly" "while" "volatile" "void" "vec4" "vec3" "vec2" "varying" "uvec4"
                      "uvec3" "uvec2" "using" "usamplerCubeArray" "usamplerCube" "usamplerBuffer"
                      "usampler3D" "usampler2DRect" "usampler2DMSArray" "usampler2DMS"
                      "usampler2DArray" "usampler2D" "usampler1DArray" "usampler1D" "unsigned"
                      "union" "uniform" "uint" "uimageCubeArray" "uimageCube" "uimageBuffer"
                      "uimage3D" "uimage2DRect" "uimage2DMSArray" "uimage2DMS" "uimage2DArray"
                      "uimage2D" "uimage1DArray" "uimage1D" "typedef" "true" "this" "template"
                      "switch" "superp" "subroutine" "struct" "static" "smooth"
                      "smapler2DRectShadow" "sizeof" "short" "shared" "samplerCubeShadow"
                      "samplerCubeArrayShadow" "samplerCubeArray" "samplerCube" "samplerBuffer"
                      "sampler3DRect" "sampler3D" "sampler2DShadow" "sampler2DRect"
                      "sampler2DMSArray" "sampler2DMS" "sampler2DArrayShadow" "sampler2DArray"
                      "sampler2D" "sampler1DShadow" "sampler1DArrayShadow" "sampler1DArray"
                      "sampler1D" "sample" "return" "restrict" "resource" "readonly" "public"
                      "precision" "precise" "patch" "partition" "out" "otput" "notinline"
                      "noperspective" "namespace" "mediump" "mat4x4" "mat4x3" "mat4x2" "mat4"
                      "mat3x4" "mat3x3" "mat3x2" "mat3" "mat2x4" "mat2x3" "mat2x2" "mat2" "lowp"
                      "long" "layout" "ivec4" "ivec3" "ivec2" "isamplerCubeArray" "isamplerCube"
                      "isamplerBuffer" "isampler3D" "isampler2DRect" "isampler2DMSArray"
                      "isampler2DMS" "isampler2DArray" "isampler2D" "isampler1DArray" "isampler1D"
                      "invariant" "interface" "int" "input" "inout" "inline" "in" "imageCubeArray"
                      "imageCube" "imageBuffer" "image3D" "image2DRect" "image2DMSArray" "image2DMS"
                      "image2DArray" "image2D" "image1DArray" "image1D" "iimageCubeArray"
                      "iimageCube" "iimageBuffer" "iimage3D" "iimage2DRect" "iimage2DMSArray"
                      "iimage2DMS" "iimage2DArray" "iimage2D" "iimage1DArray" "iimage1D" "if"
                      "hvec4" "hvec3" "hvec2" "highp" "half" "goto" "fvec4" "fvec3" "fvec2" "for"
                      "float" "flat" "fixed" "filter" "false" "external" "extern" "enum" "else"
                      "dvec4" "dvec3" "dvec2" "double" "do" "dmat4x4" "dmat4x3" "dmat4x2" "dmat4"
                      "dmat3x4" "dmat3x3" "dmat3x2" "dmat3" "dmat2x4" "dmat2x3" "dmat2x2" "dmat2"
                      "discard" "default" "continue" "cont" "common" "coherent" "class" "centroid"
                      "cast" "case" "bvec4" "bvec3" "bvec2" "buffer" "break" "bool" "attribute"
                      "atomic_uint" "asm" "active")
                    :test #'string-equal)))
    (string
     (sformat "~a" part))
    (null)
    ((eql #.no-value))
    (cons
     (with-object-case part 
       (unsigned-int (int)
        (serialize-part int)
        (sformat "u"))
       (preprocessor-directive (directive)
        (sformat "~a" directive))
       (modified-reference (expression &rest modifiers)
        (sformat "~o~{~o~}" expression modifiers))
       (field-modifier (identifier)
        (sformat ".~o" identifier))
       (array-modifier (expression)
        (sformat "[~o]" expression))
       (increment-modifier ()
        (sformat "++"))
       (decrement-modifier ()
        (sformat "--"))
       (call-modifier (&rest values)
        (sformat "(~{~o~^, ~})" values))
       (same-+ (expression)
        (sformat "+~o" expression))
       (negation (expression)
        (sformat "-~o" expression))
       (inversion (expression)
        (sformat "!~o" expression))
       (bit-inversion (expression)
        (sformat "~~~o" expression))
       (prefix-increment (expression)
        (sformat "++~o" expression))
       (prefix-decrement (expression)
        (sformat "--~o" expression))
       (multiplication (left right)
        (sformat "(~o * ~o)" left right))
       (division (left right)
        (sformat "(~o / ~o)" left right))
       (modulus (left right)
        (sformat "(~o % ~o)" left right))
       (addition (left right)
        (sformat "(~o + ~o)" left right))
       (subtraction (left right)
        (sformat "(~o - ~o)" left right))
       (left-shift (left right)
        (sformat "(~o << ~o)" left right))
       (right-shift (left right)
        (sformat "(~o >> ~o)" left right))
       (less-than (left right)
        (sformat "(~o < ~o)" left right))
       (greater-than (left right)
        (sformat "(~o > ~o)" left right))
       (less-equal-than (left right)
        (sformat "(~o <= ~o)" left right))
       (greater-equal-than (left right)
        (sformat "(~o >= ~o)" left right))
       (equal (left right)
        (sformat "(~o == ~o)" left right))
       (not-equal (left right)
        (sformat "(~o != ~o)" left right))
       (bitwise-and (left right)
        (sformat "(~o & ~o)" left right))
       (exclusive-or (left right)
        (sformat "(~o ^ ~o)" left right))
       (inclusive-or (left right)
        (sformat "(~o | ~o)" left right))
       (logical-and (left right)
        (sformat "(~o && ~o)" left right))
       (logical-xor (left right)
        (sformat "(~o ^^ ~o)" left right))
       (logical-or (left right)
        (sformat "(~o || ~o)" left right))
       (conditional (condition expression else)
        (sformat "~o? ~o :~o" condition expression else))
       (assignment (place op value)
        (sformat "~o ~a ~o" place op value))
       (multiple-expressions (&rest expressions)
        (sformat "~{~o~^, ~}" expressions))
       (function-declaration (prototype)
        (sformat "~o" prototype))
       (function-prototype (qualifier specifier identifier &rest parameters)
        (sformat "~o~o ~o(~{~{~o~^ ~}~^, ~})"
                 qualifier specifier identifier parameters))
       (precision-declarator (precision type)
        (sformat "precision ~o ~o" precision type))
       (variable-declaration (qualifier specifier &rest initializers)
        (sformat "~o~o" qualifier specifier)
        (loop for (init . rest) on initializers
              for (identifier array initializer) = init
              do (sformat " ~o~o~@[ = ~o~]~@[,~]" identifier array initializer rest)))
       (layout-qualifier (&rest ids)
        (sformat "layout(~{~o~^, ~})" ids))
       (layout-qualifier-id (identifier &optional value)
        (sformat "~o~@[ = ~o~]" identifier value))
       (type-qualifier (&rest qualifiers)
        (sformat "~{~o ~}" qualifiers))
       (subroutine-qualifier (&optional type-name)
        (sformat "subroutine~@[(~o)~]" type-name))
       (type-specifier (type &optional array)
        (sformat "~o~@[~o~]" type array))
       (array-specifier (&rest specifiers)
        (sformat "~:[[]~;~:*~{[~o]~}~]" specifiers))
       (type-name (identifier)
        (sformat "~o" identifier))
       (struct-specifier (identifier &rest declarations)
        (sformat "struct ~o{~{~o~}}" identifier declarations))
       (struct-declaration (qualifier specifier &rest declarators)
        (sformat "~o~o~{~{~o~^ ~}~^, ~};" qualifier specifier declarators))
       (array-initializer (initializer &rest initializers)
        (sformat "{~o~{, ~o~}}" initializer initializers))
       (compound-statement (&rest statements)
        (sformat "{")
        (with-indentation ()
          (dolist (statement statements)
            (indent) (sformat "~o;" statement)))
        (indent) (sformat "}"))
       (selection-statement (expression statement &optional else)
        (sformat "if(~o)~o~@[else~o~]" expression statement else))
       (condition-declarator (qualifier specifier identifier initializer)
        (sformat "~o~o ~o = ~o" qualifier specifier identifier initializer))
       (switch-statement (expression statement)
        (sformat "switch(~o)~o" expression statement))
       (case-label (case)
        (indent)
        (if (eql :default case)
            (sformat "default: ")
            (sformat "case ~o: " case)))
       (while-statement (condition statement)
        (sformat "while(~o)~o" condition statement))
       (do-statement (statement expression)
         (sformat "do~o" statement)
         (indent) (sformat "while(~o)" expression))
       (for-statement (declaration condition expression statement)
        (sformat "for(~o; ~o; ~o)~o" declaration condition expression statement))
       (continue ()
        (sformat "continue"))
       (break ()
        (sformat "break"))
       (return (&optional value)
        (sformat "return~@[ ~o~]" value))
       (discard ()
        (sformat "discard"))
       (function-definition (prototype statement)
        (sformat "~%~o~o" prototype statement))
       (shader (&rest items)
        (dolist (item items)
          (indent) (serialize-part item)
          (sformat ";")))))))
