(in-package #:glsl-parser)

(define-struct shader
    (* (v (or preprocessor
              variable-definition
              function-definition
              struct-definition
              ";"))))

(define-struct preprocessor
    (and "#" (* (v (notany (#\Newline))))))

(define-struct variable-definition
    (and (v type)
         (v variable-declaration)
         (* (and "," (v variable-declaration)))
         ";"))

(define-struct variable-declaration
    (and (v identifier)
         (when "="
           (v value))))

(define-struct function-definition
    (and (v basic-type)
         (v identifier)
         (v definition-arglist)
         (or ";"
             (v block))))

(define-struct definition-arglist
    (and "(" (? (v definition-argument)) (* (and "," (v definition-argument))) ")"))

(define-struct definition-argument
    (and (v basic-type) (v identifier)))

(define-struct block
    (and "{" (* (v expression)) "}"))

(define-struct struct-definition
    (and (? (v qualifiers)) "struct" (v identifier) "{"
         (* (v struct-field))
         "};"))

(define-struct struct-field
    (and (v basic-type) (v identifier) ";"))

(define-rule value
    (v (or block-value
           math-value
           ternary
           call
           float-constant
           integer-constant
           variable)))

(define-rule block-value
    (and "(" (v value) ")"))

;; FIXME: proper binding order
(define-struct math-value
    (and (v value) (v (or "+" "-" "*" "/" "^" "%" "|" "&" "||" "&&")) (v value)))

(define-struct ternary
    (and (v value) "?" (v value) ":" (v value)))

(define-struct integer-constant
    (and (v (or decimal-constant
                hexadecimal-constant
                octal-constant))
         (v (? (any "uU")))))

(define-struct decimal-constant
    (or (and (v (any "123456789")) (* (v (any "0123456789")))))
  (parse-integer (map 'string #'identity v) :radix 10))

(define-struct octal-constant
    (and "0" (* (v (any "01234567"))))
  (parse-integer (map 'string #'identity v) :radix 8))

(define-struct hexadecimal-constant
    (and "0x" (* (v (any 0123456789abcdefABCDEF))))
  (parse-integer (map 'string #'identity v) :radix 16))

(define-struct float-constant
    (and (* (v (any "0123456789"))) "." (* (v (any "0123456789")))
         (when (any "eE") (v (any "+-")) (* (v (any "0123456789"))))
         (v (? (or "f" "F" "lf" "LF") "f")))
  (let ((type (if (string-equal "f" (car (last v)))
                  'single-float 'double-float)))
    (parse-float (map 'string (lambda (s) (char s 0)) (butlast v)) :type type)))

(define-rule expression
    (or ";"
        (v (or variable-definition
               (and value ";")
               block
               if
               switch
               loop
               return
               "break;"
               "continue;"))))

(define-struct if
    (and "if" (v value)
         (v (or block expression))
         (when "else"
           (v (or block expression)))))

(define-struct switch
    (and "switch" (v value)
         (* (and (or (and "case" (v integer))
                     "default") ":"
                 (* (v expression)) (? (v "break"))))))

(define-rule loop
    (v (or for while do)))

(define-struct for
    (and "for" "(" (v variable-definition) ";" (v value) ";" (v value) ")"
         (v (or block expression))))

(define-struct while
    (and "while" (v value) (v (or block expression))))

(define-struct do
    (and "do" (v (or block expression))
         (when "while" (v expression))))

(define-struct return
    (and "return" (v value) ";"))

(define-struct variable
    (and (v identifier)
         (when "[" (v value) "]")))

(define-struct function-call
    (and (v identifier) (v call-arglist)))

(define-struct call-arglist
    (and "(" (? (v identifier)) (* (and "," (v identifier))) ")"))

(define-struct identifier
    (* (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")))
  (map 'string #'identity v))

(define-rule basic-type
    (v (or ;; Transparent Types
           "void" "bool" "int" "uint" "float" "double"
           "vec2" "vec3" "vec4"
           "dvec2" "dvec3" "dvec4" "bvec2" "bvec3" "bvec4"
           "ivec2" "ivec3" "ivec4" "uvec2" "uvec3" "uvec4"
           "mat2" "mat3" "mat4"
           "mat2x2" "mat2x3" "mat2x4"
           "mat3x2" "mat3x3" "mat3x4"
           "mat4x2" "mat4x3" "mat4x4"
           "dmat2" "dmat3" "dmat4"
           "dmat2x2" "dmat2x3" "dmat2x4"
           "dmat3x2" "dmat3x3" "dmat3x4"
           "dmat4x2" "dmat4x3" "dmat4x4"
           ;; Floating-Point Opaque Types
           "sampler1D" "image1D"
           "sampler2D" "image2D"
           "sampler3D" "image3D"
           "samplerCube" "imageCube"
           "sampler2DRect" "image2DRect"
           "sampler1DArray" "image1DArray"
           "sampler2DArray" "image2DArray"
           "samplerBuffer" "imageBuffer"
           "sampler2DMS" "image2DMS"
           "sampler2DMSArray" "image2DMSArray"
           "samplerCubeArray" "imageCubeArray"
           "sampler1DShadow"
           "sampler2DShadow"
           "smapler2DRectShadow"
           "sampler1DArrayShadow"
           "sampler2DArrayShadow"
           "samplerCubeShadow"
           "samplerCubeArrayShadow"
           ;; Signed Integer Opaque Types
           "isampler1D" "iimage1D"
           "isampler2D" "iimage2D"
           "isampler3D" "iimage3D"
           "isamplerCube" "iimageCube"
           "isampler2DRect" "iimage2DRect"
           "isampler1DArray" "iimage1DArray"
           "isampler2DArray" "iimage2DArray"
           "isamplerBuffer" "iimageBuffer"
           "isampler2DMS" "iimage2DMS"
           "isampler2DMSArray" "iimage2DMSArray"
           "isamplerCubeArray" "iimageCubeArray"
           ;; Unsigned Integer Opaque Types
           "atomic_uint"
           "usampler1D" "uimage1D"
           "usampler2D" "uimage2D"
           "usampler3D" "uimage3D"
           "usamplerCube" "uimageCube"
           "usampler2DRect" "uimage2DRect"
           "usampler1DArray" "uimage1DArray"
           "usampler2DArray" "uimage2DArray"
           "usamplerBuffer" "uimageBuffer"
           "usampler2DMS" "uimage2DMS"
           "usampler2DMSArray" "uimage2DMSArray"
           "usamplerCubeArray" "uimageCubeArray")))
