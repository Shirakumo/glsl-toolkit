#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

;;; Lexer
(define-rule whitespace
  #\Newline #\Space #\Tab)

(define-struct integer-token
    (and (v (or decimal-token
                hexadecimal-token
                octal-token))
         (? (v (any "uU")))))

(define-struct decimal-token
    (or (and (v (any "123456789")) (* (v (any "0123456789")))))
  (parse-integer (map 'string #'identity v) :radix 10))

(define-struct octal-token
    (and (v (any "0")) (* (v (any "01234567"))))
  (parse-integer (map 'string #'identity v) :radix 8))

(define-struct hexadecimal-token
    (and "0x" (* (v (any "0123456789abcdefABCDEF"))))
  (parse-integer (map 'string #'identity v) :radix 16))

(define-struct float-token
    (and (* (v (any "0123456789"))) (v #\.) (* (v (any "0123456789")))
         (? (when (any "eE") (v (any "+-")) (* (v (any "0123456789")))))
         (v (? (or "f" "F" "lf" "LF") "f")))
  (let ((type (if (string-equal "f" (car (last v)))
                  'single-float 'double-float)))
    (parse-float:parse-float (map 'string #'identity (butlast v)) :type type)))

(define-struct identifier-token
    (and (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))
         (* (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))))
  (map 'string #'identity v))

(define-struct keyword-token
    (v (or "writeonly" "while" "volatile" "void" "vec4" "vec3" "vec2" "varying" "uvec4"
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
           "atomic_uint" "asm" "active"))
  (intern (string-upcase (first v)) :keyword))

(define-struct preprocessor-directive
    (and (v #\#) (v (notany '(#\Newline))))
  (list 'preprocessor-directive (map 'string #'identity v)))

(defmacro define-operator-structs (&body names)
  `(progn
     ,@(loop for name in names
             collect `(define-struct ,name ,(string name) ,(intern (string name) :keyword)))
     (define-rule operator
       ,@names)))

(define-operator-structs
  = += -= *= /= %= <<= >>= &= ^= \|=
  ++ -- << >>  ^^ \|\| && < > <= >=
  + - * / % & ^ ! \|
  \( \) \[ \] \{ \} \; \. ? \: \,)

(define-rule token
  (and (* whitespace)
       (or keyword-token
           identifier-token
           integer-token
           float-token
           operator)))

(define-struct tokenize
    (* (v token))
  v)

;;; Parser
(define-struct integer-constant
    (and (consp (peek))
         (integerp (second (peek))))
  (consume))

(define-struct float-constant
    (floatp (peek))
  (consume))

(define-struct boolean-constant
    (v (or :true :false))
  v)

(define-struct identifier
    (stringp (peek))
  (consume))

(define-struct variable-identifier
    (v identifier))

(define-rule primary-expression
  integer-constant
  float-constant
  boolean-constant
  (and :\( (v expression) :\))
  variable-identifier)

(define-rule postfix-expression
  primary-expression
  modified-reference)

(define-struct modified-reference
    (and (v primary-expression)
         (v (or call-modifier
                field-modifier
                array-modifier
                increment-modifier
                decrement-modifier))))

(define-struct field-modifier
    (and :\. (v identifier)))

(define-struct array-modifier
    (and :\[ (v expression) :\]))

(define-struct increment-modifier
    :++)

(define-struct decrement-modifier
    :--)

(define-struct call-modifier
    (and :\( (v function-call-arglist) :\)))

(define-struct function-call-arglist
    (? (or (v :void)
           (and (v assignment-expression)
                (* :\, (v assignment-expression))))))

(define-struct same-+
    (and :+ (v unary-expression)))

(define-struct negation
    (and :- (v unary-expression)))

(define-struct inversion
    (and :! (v unary-expression)))

(define-struct bit-inversion
    (and :~ (v unary-expression)))

(define-struct prefix-increment
    (and :++ (v unary-expression)))

(define-struct prefix-decrement
    (and :-- (v unary-expression)))

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
    (and (v unary-expression) :* (v multiplicative-expression)))

(define-struct division
    (and (v unary-expression) :/ (v multiplicative-expression)))

(define-struct modulus
    (and (v unary-expression) :% (v multiplicative-expression)))

(define-rule additive-expression
  multiplicative-expression
  addition
  subtraction)

(define-struct addition
    (and (v multiplicative-expression) :+ (v additive-expression)))

(define-struct subtraction
    (and (v multiplicative-expression) :- (v additive-expression)))

(define-rule shift-expression
  additive-expression
  left-shift
  right-shift)

(define-struct left-shift
    (and (v additive-expression) :<< (v shift-expression)))

(define-struct right-shift
    (and (v additive-expression) :>> (v shift-expression)))

(define-rule relational-expression
  shift-expression
  less-than
  greater-than
  less-equal-than
  greater-equal-than)

(define-struct less-than
    (and (v shift-expression) :< (v relational-expression)))

(define-struct greater-than
    (and (v shift-expression) :> (v relational-expression)))

(define-struct less-equal-than
    (and (v shift-expression) :<= (v relational-expression)))

(define-struct greater-equal-than
    (and (v shift-expression) :>= (v relational-expression)))

(define-rule equality-expression
  relational-expression
  equal
  not-equal)

(define-struct equal
    (and (v relational-expression) :== (v equality-expression)))

(define-struct not-equal
    (and (v relational-expression) :!= (v equality-expression)))

(define-rule and-expression
  equality-expression
  bitwise-and)

(define-struct bitwise-and
    (and (v equality-expression) :& (v and-expression)))

(define-rule exclusive-or-expression
  and-expression
  exclusive-or)

(define-struct exclusive-or
    (and (v and-expression) :^ (v exclusive-or-expression)))

(define-rule inclusive-or-expression
  exclusive-or-expression
  inclusive-or)

(define-struct inclusive-or
    (and (v exclusive-or-expression) :\| (v inclusive-or-expression)))

(define-rule logical-and-expression
  inclusive-or-expression
  logical-and)

(define-struct logical-and
    (and (v inclusive-or-expression) :&& (v logical-and-expression)))

(define-rule logical-xor-expression
  logical-and-expression
  logical-xor)

(define-struct logical-xor
    (and (v logical-and-expression) :^^ (v logical-xor-expression)))

(define-rule logical-or-expression
  logical-xor-expression
  logical-or)

(define-struct logical-or
    (and (v logical-xor-expression) :\|\| (v logical-or-expression)))

(define-rule conditional-expression
  logical-or-expression
  conditional)

(define-struct conditional
    (and (v logical-or-expression) :? (v expression) :\: (v assignment-expression)))

(define-rule assignment-expression
  conditional-expression
  assignment)

(define-struct assignment
    (and (v unary-expression)
         (v (or := :*= :/= :%= :+= :-= :<= :>= :&= :^= :\|=))
         (v assignment-expression)))

(define-struct expression
    (and (v assignment-expression (* (and :\, (v assignment-expression))))))

(define-rule constant-expression
  conditional-expression)

(define-rule declaration
  (and (v (or function-prototype
              variable-declaration
              precision-declarator
              struct-declaration))
       :\;))

(define-struct function-prototype
    (and (v fully-specified-type) (v identifier) (v function-prototype-parameters)))

(define-struct function-prototype-parameters
    (and :\( (? (v parameter-declaration)) (* (and :\, (v parameter-declaration))) :\)))

(define-struct parameter-declaration
    (and (v (? type-qualifier)) (v type-specifier)
         (v (? identifier)) (? (v array-specifier))))

(define-struct precision-declarator
    (and :precision (v (or :highp :mediump :lowp)) (v type-specifier)))

(define-struct variable-declaration
    (and (v fully-specified-type) (v variable-initializer)
         (* (and :\, (v variable-initializer)))))

(define-struct variable-initializer
    (and (v identifier) (v (? array-specifier)) (? (and := (v initializer)))))

(define-struct fully-specified-type
    (and (v (? type-qualifier)) (v type-specifier)))

(define-rule invariant-qualifier
  :invariant)

(define-rule interpolation-qualifier
  :smooth :flat :noperspective)

(define-struct layout-qualifier
    (and :layout :\( (v layout-qualifier-id) (* (and :\, (v layout-qualifier-id))) :\)))

(define-struct layout-qualifier-id
    (or (v :shared)
        (and (v identifier) (? (and := (v constant-expression))))))

(define-rule precise-qualifier
  :precise)

(define-struct type-qualifier
    (+ (v (or storage-qualifier
              subroutine-qualifier
              layout-qualifier
              precision-qualifier
              interpolation-qualifier
              invariant-qualifier
              precise-qualifier))))

(define-rule storage-qualifier
  :const :inout :in :out :centroid :patch :sample
  :uniform :buffer :shared :coherent :volatile
  :restrict :readonly :writeonly)

(define-struct subroutine-qualifier
    (and :subroutine (? (and :\( (v type-name) :\)))))

(define-rule precision-qualifier
  :highp :mediump :lowp)

(define-struct type-specifier
    (and (v type-specifier-nonarray) (? (v array-specifier))))

(define-struct array-specifier
    (+ (and :\[ (? (v constant-expression)) :\])))

(define-rule type-specifier-nonarray
  basic-type
  struct-specifier
  type-name)

(define-struct type-name
    (v identifier))

(define-rule basic-type
  ;; Transparent Types
  :void :bool :int :uint :float :double
  :vec2 :vec3 :vec4
  :dvec2 :dvec3 :dvec4 :bvec2 :bvec3 :bvec4
  :ivec2 :ivec3 :ivec4 :uvec2 :uvec3 :uvec4
  :mat2 :mat3 :mat4
  :mat2x2 :mat2x3 :mat2x4
  :mat3x2 :mat3x3 :mat3x4
  :mat4x2 :mat4x3 :mat4x4
  :dmat2 :dmat3 :dmat4
  :dmat2x2 :dmat2x3 :dmat2x4
  :dmat3x2 :dmat3x3 :dmat3x4
  :dmat4x2 :dmat4x3 :dmat4x4
  ;; Floating-Point Opaque Types
  :sampler1D :image1D
  :sampler2D :image2D
  :sampler3D :image3D
  :samplerCube :imageCube
  :sampler2DRect :image2DRect
  :sampler1DArray :image1DArray
  :sampler2DArray :image2DArray
  :samplerBuffer :imageBuffer
  :sampler2DMS :image2DMS
  :sampler2DMSArray :image2DMSArray
  :samplerCubeArray :imageCubeArray
  :sampler1DShadow
  :sampler2DShadow
  :smapler2DRectShadow
  :sampler1DArrayShadow
  :sampler2DArrayShadow
  :samplerCubeShadow
  :samplerCubeArrayShadow
  ;; Signed Integer Opaque Types
  :isampler1D :iimage1D
  :isampler2D :iimage2D
  :isampler3D :iimage3D
  :isamplerCube :iimageCube
  :isampler2DRect :iimage2DRect
  :isampler1DArray :iimage1DArray
  :isampler2DArray :iimage2DArray
  :isamplerBuffer :iimageBuffer
  :isampler2DMS :iimage2DMS
  :isampler2DMSArray :iimage2DMSArray
  :isamplerCubeArray :iimageCubeArray
  ;; Unsigned Integer Opaque Types
  :atomic_uint
  :usampler1D :uimage1D
  :usampler2D :uimage2D
  :usampler3D :uimage3D
  :usamplerCube :uimageCube
  :usampler2DRect :uimage2DRect
  :usampler1DArray :uimage1DArray
  :usampler2DArray :uimage2DArray
  :usamplerBuffer :uimageBuffer
  :usampler2DMS :uimage2DMS
  :usampler2DMSArray :uimage2DMSArray
  :usamplerCubeArray :uimageCubeArray)

(define-struct struct-specifier
    (and :struct (v (? identifier)) :\{ (+ (v struct-declaration)) :\}))

(define-struct struct-declaration
    (and (v (? type-qualifier)) (v type-specifier)
         (v struct-declarator) (* (and :\, (v struct-declarator))) :\;))

(define-struct struct-declarator
    (and (v identifier) (? (v array-specifier))))

(define-struct initializer
    (or (v assignment-expression)
        (and :\{ (v initializer) (* (and :\, (v initializer))) (? :\,) :\})))

(define-rule statement
  simple-statement
  compound-statement)

(define-rule simple-statement
  declaration
  expression-statement
  selection-statement
  switch-statement
  case-label
  iteration-statement
  jump-statement
  :\;)

(define-struct compound-statement
    (and :\{ (* (v statement)) :\}))

(define-rule expression-statement
  (and (v expression) :\;))

(define-struct selection-statement
    (and :if :\( (v expression) :\) (v statement) (? (and :else (v statement)))))

(define-rule condition
  expression
  condition-declarator)

(define-struct condition-declarator
    (and (v fully-specified-type) (v identifier) := (v initializer)))

(define-struct switch-statement
    (and :switch :\( (v expression) :\)
         :\{ (* (v statement)) :\}))

(define-struct case-label
    (or (and :case (v expression) :\:)
        (and (v :default) :\:)))

(define-rule iteration-statement
  while-statement
  do-statement
  for-statement)

(define-struct while-statement
    (and :while :\( (v condition) :\) (v statement)))

(define-struct do-statement
    (and :do (v statement) :while :\( (v expression) :\) :\;))

(define-struct for-statement
    (and :for :\(
         (v (or expression-statement declaration))
         (v (? condition)) :\;
         (v (? expression)) :\)
         (v statement)))

(define-rule jump-statement
  continue
  break
  return
  discard)

(define-struct continue
    (and :continue :\;))

(define-struct break
    (and :break :\;))

(define-struct return
    (and :return (? (v expression)) :\;))

(define-struct discard
    (and :discard :\;))

(define-struct function-definition
    (and (v function-prototype) (v compound-statement)))

(define-struct shader
    (* (v (or function-definition declaration))))
