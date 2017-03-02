(in-package #:glsl-parser)

(define-struct boolean-constant
    (v (or "true" "false")))

(define-struct integer-constant
    (and (v (or decimal-constant
                hexadecimal-constant
                octal-constant))
         (? (v (any "uU")))))

(define-struct decimal-constant
    (or (and (v (any "123456789")) (* (v (any "0123456789")))))
  (parse-integer (map 'string #'identity v) :radix 10))

(define-struct octal-constant
    (and "0" (* (v (any "01234567"))))
  (parse-integer (map 'string #'identity v) :radix 8))

(define-struct hexadecimal-constant
    (and "0x" (* (v (any "0123456789abcdefABCDEF"))))
  (parse-integer (map 'string #'identity v) :radix 16))

(define-struct float-constant
    (and (* (v (any "0123456789"))) (v #\.) (* (v (any "0123456789")))
         (? (when (any "eE") (v (any "+-")) (* (v (any "0123456789")))))
         (v (? (or "f" "F" "lf" "LF") "f")))
  (let ((type (if (string-equal "f" (car (last v)))
                  'single-float 'double-float)))
    (parse-float:parse-float (map 'string #'identity (butlast v)) :type type)))

(define-struct identifier
    (* (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")))
  (map 'string #'identity v))

(define-struct variable-identifier
    (v identifier))

(define-rule primary-expression
    (or (v variable-identifier)
        (v integer-constant)
        (v float-constant)
        (v boolean-constant)
        (and "(" (v expression) ")")))

(define-struct field-reference
    (and (v postfix-expression) "." (v identifier)))

(define-struct array-reference
    (and (v postfix-expression) "[" (v expression) "]"))

(define-rule postfix-expression
  primary-expression
  array-reference
  function-call
  field-reference
  postfix-increment
  postfix-decrement)

(define-struct postfix-increment
    (and (v postfix-expression) "++"))

(define-struct postfix-decrement
    (and (v postfix-expression) "--"))

(define-rule function-identifier
  type-specifier
  postfix-expression)

(define-struct function-call
    (and (v function-identifier) "(" (v function-call-arglist) ")"))

(define-struct function-call-arglist
    (? (or (v "void")
           (and (v assignment-expression)
                (* "," (v assignment-expression))))))

(define-struct same-+
    (and "+" (v unary-expression)))

(define-struct negation
    (and "-" (v unary-expression)))

(define-struct inversion
    (and "!" (v unary-expression)))

(define-struct bit-inversion
    (and "~" (v unary-expression)))

(define-struct prefix-increment
    (and "++" (v unary-expression)))

(define-struct prefix-decrement
    (and "--" (v unary-expression)))

(define-rule unary-op-expression
  same-+
  negation
  inversion
  bit-inversion)

(define-rule unary-expression
  postfix-expression
  prefix-increment
  prefix-decrement
  unary-op-expression)

(define-rule multiplicative-expression
  unary-expression
  multiplication
  division
  modulus)

(define-struct multiplication
    (and (v multiplicative-expression) "*" (v unary-expression)))

(define-struct division
    (and (v multiplicative-expression) "/" (v unary-expression)))

(define-struct modulus
    (and (v multiplicative-expression) "%" (v unary-expression)))

(define-rule additive-expression
  multiplicative-expression
  addition
  subtraction)

(define-struct addition
    (and (v additive-expression) "+" (v multiplicative-expression)))

(define-struct subtraction
    (and (v additive-expression) "-" (v multiplicative-expression)))

(define-rule shift-expression
  additive-expression
  left-shift
  right-shift)

(define-struct left-shift
    (and (v shift-expression) "<<" (v additive-expression)))

(define-struct right-shift
    (and (v shift-expression) ">>" (v additive-expression)))

(define-rule relational-expression
  shift-expression
  less-than
  greater-than
  less-equal-than
  greater-equal-than)

(define-struct less-than
    (and (v relational-expression) "<" (v shift-expression)))

(define-struct greater-than
    (and (v relational-expression) ">" (v shift-expression)))

(define-struct less-equal-than
    (and (v relational-expression) "<=" (v shift-expression)))

(define-struct greater-equal-than
    (and (v relational-expression) ">=" (v shift-expression)))

(define-rule equality-expression
  relational-expression
  equal
  not-equal)

(define-struct equal
    (and (v equality-expression) "==" (v relational-expression)))

(define-struct not-equal
    (and (v equality-expression) "!=" (v relational-expression)))

(define-rule and-expression
  equality-expression
  bitwise-and)

(define-struct bitwise-and
    (and (v and-expression) "&" (v equality-expression)))

(define-rule exclusive-or-expression
  and-expression
  exclusive-or)

(define-struct exclusive-or
    (and (v exclusive-or-expression) "^" (v and-expression)))

(define-rule inclusive-or-expression
  exclusive-or-expression
  inclusive-or)

(define-struct inclusive-or
    (and (v inclusive-or-expression) "|" (v exclusive-or-expression)))

(define-rule logical-and-expression
  inclusive-or-expression
  logical-and)

(define-struct logical-and
    (and (v logical-and-expression) "&&" (v inclusive-or-expression)))

(define-rule logical-xor-expression
  logical-and-expression
  logical-xor)

(define-struct logical-xor
    (and (v logical-xor-expression) "^^" (v logical-and-expression)))

(define-rule logical-or-expression
  logical-xor-expression
  logical-or)

(define-struct logical-or
    (and (v logical-or-expression) "||" (v logical-xor-expression)))

(define-rule conditional-expression
  logical-or-expression
  conditional)

(define-struct conditional
    (and (v logical-or-expression) "?" (v expression) ":" (v assignment-expression)))

(define-rule assignment-expression
  conditional-expression
  assignment)

(define-struct assignment
    (and (v unary-expression) (v (or "=" "*=" "/=" "%=" "+=" "-=" "<=" ">=" "&=" "^=" "|=")) (v assignment-expression)))

(define-struct expression
    (and (v assignment-expression (* (and "," (v assignment-expression))))))

(define-rule constant-expression
  conditional-expression)

(define-rule declaration
  (and (v (or function-prototype
              init-declarator-list
              precision-declarator
              struct-declaration
              variable-declaration))
       ";"))

(define-struct variable-declaration
    (and (v type-qualifier) (v identifier) (* (and "," (v identifier)))))

(define-struct function-prototype
    (and (v fully-specified-type) (v identifier) (v function-prototype-parameters)))

(define-struct function-prototype-parameters
    (and "(" (* (v parameter-declaration)) ")")
  v)

(define-struct parameter-declaration
    (and (v (? type-qualifier)) (v type-specifier)
         (v (? identifier)) (? (v array-specifier))))

(define-struct precision-declarator
    (and "precision" (v (or "highp" "mediump" "lowp")) (v type-specifier)))

(define-struct init-declarator-list
    (and (v fully-specified-type) (v single-declaration)
         (* (and "," (v single-declaration)))))

(define-struct single-declaration
    (and (v identifier) (v (? array-specifier)) (? (and "=" (v initializer)))))

(define-struct fully-specified-type
    (and (v (? type-qualifier)) (v type-specifier)))

(define-struct invariant-qualifier
    "invariant")

(define-struct interpolation-qualifier
    (v (or "smooth" "flat" "noperspective")))

(define-struct layout-qualifier
    (and "layout" "(" (v layout-qualifier-id) (* (and "," (v layout-qualifier-id))) ")"))

(define-struct layout-qualifier-id
    (or (v "shared")
        (and (v identifier) (? (and "=" (v constant-expression))))))

(define-struct precise-qualifier
    "precise")

(define-struct type-qualifier
    (+ (v (or storage-qualifier
              subroutine-qualifier
              layout-qualifier
              precision-qualifier
              interpolation-qualifier
              invariant-qualifier
              precise-qualifier))))

(define-struct storage-qualifier
    (v (or "const" "inout" "in" "out" "centroid" "patch" "sample"
           "uniform" "buffer" "shared" "coherent" "volatile"
           "restrict" "readonly" "writeonly")))

(define-struct subroutine-qualifier
    (and "subroutine" (? (and "(" (v type-name) ")"))))

(define-struct precision-qualifier
    (v (or "highp" "mediump" "lowp")))

(define-struct type-specifier
    (and (v type-specifier-nonarray) (? (v array-specifier))))

(define-struct array-specifier
    (+ (and "[" (? (v constant-expression)) "]")))

(define-rule type-specifier-nonarray
    basic-type
  struct-specifier
  type-name)

(define-struct type-name
    (v identifier))

(define-rule basic-type
  ;; Transparent Types
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
  "usamplerCubeArray" "uimageCubeArray")

(define-struct struct-specifier
    (and "struct" (v (? identifier)) "{" (+ (v struct-declaration)) "}"))

(define-struct struct-declaration
    (and (v (? type-qualifier)) (v type-specifier)
         (v struct-declarator) (* (and "," (v struct-declarator))) ";"))

(define-struct struct-declarator
    (and (v identifier) (? (v array-specifier))))

(define-struct initializer
    (or (v assignment-expression)
        (and "{" (v initializer) (* (and "," (v initializer))) (? ",") "}")))

(define-rule statement
  compound-statement
  simple-statement)

(define-rule simple-statement
  declaration
  expression-statement
  selection-statement
  switch-statement
  case-label
  iteration-statement
  jump-statement)

(define-struct compound-statement
    (and "{" (* (v statement)) "}"))

(define-rule expression-statement
    (and (? (v expression)) ";"))

(define-struct selection-statement
    (and "if" "(" (v expression) ")" (v statement) (? (and "else" (v statement)))))

(define-rule condition
    expression
  condition-declarator)

(define-struct condition-declarator
    (and (v fully-specified-type) (v identifier) "=" (v initializer)))

(define-struct switch-statement
    (and "switch" "(" (v expression) ")"
         "{" (* (v statement)) "}"))

(define-struct case-label
    (or (and "case" (v expression) ":")
        (and (v "default") ":")))

(define-rule iteration-statement
    while-statement
  do-statement
  for-statement)

(define-struct while-statement
    (and "while" "(" (v condition) ")" (v statement)))

(define-struct do-statement
    (and "do" (v statement) "while" "(" (v expression) ")" ";"))

(define-struct for-statement
    (and "for" "("
         (v (or expression-statement declaration))
         (v (? condition)) ";"
         (v (? expression)) ")"
         (v statement)))

(define-rule jump-statement
    continue
  break
  return
  discard)

(define-struct continue
    (and "continue" ";"))

(define-struct break
    (and "break" ";"))

(define-struct return
    (and "return" (? (v expression)) ";"))

(define-struct discard
    (and "discard" ";"))

(define-struct translation-unit
    (* (v external-declaration)))

(define-rule external-declaration
    function-definition
  declaration)

(define-struct function-definition
    (and (v function-prototype) (v compound-statement)))
