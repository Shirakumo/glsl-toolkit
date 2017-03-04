#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl.parser)

;;; Lexer
(define-reference whitespace
  #\Newline #\Space #\Tab)

(define-object integer-token
    (and (v (or decimal-token
                hexadecimal-token
                octal-token))
         (? (v (any "uU")))))

(define-object decimal-token
    (or (and (v (any "123456789")) (* (v (any "0123456789")))))
  (parse-integer (coerce v 'string) :radix 10))

(define-object octal-token
    (and (v (any "0")) (* (v (any "01234567"))))
  (parse-integer (coerce v 'string) :radix 8))

(define-object hexadecimal-token
    (and "0x" (* (v (any "0123456789abcdefABCDEF"))))
  (parse-integer (coerce v 'string) :radix 16))

(define-object float-token
      (and (or (and (+ (v (any "0123456789"))) (v #\.) (* (v (any "0123456789"))))
               (and (* (v (any "0123456789"))) (v #\.) (+ (v (any "0123456789")))))
         (? (when (any "eE") (v (any "+-")) (* (v (any "0123456789")))))
         (v (? (or "f" "F" "lf" "LF") "f")))
  (let ((type (if (string-equal "f" (car (last v)))
                  'single-float 'double-float)))
    (parse-float:parse-float (coerce (butlast v) 'string) :type type)))

(define-object identifier-token
    (and (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))
         (* (v (any "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))))
  (coerce v 'string))

(define-object keyword-token
      (and (v (or "writeonly" "while" "volatile" "void" "vec4" "vec3" "vec2" "varying" "uvec4"
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
           (! (or whitespace operator)))
  (intern (string-upcase (first v)) :keyword))

(define-object preprocessor-token
    (and (v #\#) (* (v (notany (#\Newline)))))
  (list 'org.shirakumo.trial.glsl.parser.rules::preprocessor-directive (coerce v 'string)))

(defmacro define-operator-structs (&body names)
  `(progn
     ,@(loop for name in names
             collect `(define-object ,name ,(string name) ,(intern (string name) :keyword)))
     (define-reference operator
       ,@names)))

(define-operator-structs
  = += -= *= /= %= <<= >>= &= ^= \|=
  ++ -- << >>  ^^ \|\| && <= >= < >
  + - * / % & ^ ! \|
  \( \) \[ \] \{ \} \; \. ? \: \,)

(define-reference token
  (and (* whitespace)
       (or keyword-token
           identifier-token
           float-token
           integer-token
           operator
           preprocessor-token)))

(define-object tokenize
    (* (v token))
  v)

;;; Parser
(define-object integer-constant
    (and (consp (peek))
         (integerp (second (peek))))
  (consume))

(define-object float-constant
    (floatp (peek))
  (consume))

(define-object boolean-constant
    (v (or :true :false))
  (first v))

(define-object identifier
    (stringp (peek))
  (consume))

(define-object preprocessor-directive
    (and (consp (peek))
         (stringp (second (peek))))
  (consume))

(define-reference primary-expression
  integer-constant
  float-constant
  boolean-constant
  (and :\( (v expression) :\))
  identifier
  basic-type)

(define-reference postfix-expression
  modified-reference
  primary-expression)

(define-object modified-reference
    (and (v primary-expression)
         (v reference-modifier)
         (* (v reference-modifier))))

(define-reference reference-modifier
  call-modifier
  field-modifier
  array-modifier
  increment-modifier
  decrement-modifier)

(define-object field-modifier
    (and :\. (v identifier)))

(define-object array-modifier
    (and :\[ (v expression) :\]))

(define-object increment-modifier
    :++)

(define-object decrement-modifier
    :--)

(define-object call-modifier
    (and :\( (or :void
                 (? (and (v assignment-expression)
                         (* (and :\, (v assignment-expression))))))
         :\)))

(define-object same-+
    (and :+ (v unary-expression)))

(define-object negation
    (and :- (v unary-expression)))

(define-object inversion
    (and :! (v unary-expression)))

(define-object bit-inversion
    (and :~ (v unary-expression)))

(define-object prefix-increment
    (and :++ (v unary-expression)))

(define-object prefix-decrement
    (and :-- (v unary-expression)))

(define-reference unary-op-expression
  same-+
  negation
  inversion
  bit-inversion)

(define-reference unary-expression
  postfix-expression
  prefix-increment
  prefix-decrement
  unary-op-expression)

(define-reference multiplicative-expression
  multiplication
  division
  modulus)

(defmacro define-binary-op (name left op right)
  `(define-object ,name
       (and (v ,left) (? (and ,op (v ,right))))
     (if (second v)
         (list* ',name v)
         (first v))))

(define-binary-op multiplication
    unary-expression :* multiplication)

(define-binary-op division
    multiplication :/ division)

(define-binary-op modulus
    division :% modulus)

(define-binary-op addition
    modulus :+ addition)

(define-binary-op subtraction
    addition :- subtraction)

(define-binary-op left-shift
    subtraction :<< left-shift)

(define-binary-op right-shift
    left-shift :>> right-shift)

(define-binary-op less-than
    right-shift :< less-than)

(define-binary-op greater-than
    less-than :> greater-than)

(define-binary-op less-equal-than
    greater-than :<= less-equal-than)

(define-binary-op greater-equal-than
    less-equal-than :>= greater-equal-than)

(define-binary-op equal
    greater-equal-than :== equal)

(define-binary-op not-equal
    equal :!= not-equal)

(define-binary-op bitwise-and
    not-equal :& bitwise-and)

(define-binary-op exclusive-or
    bitwise-and :^ exclusive-or)

(define-binary-op inclusive-or
    exclusive-or :\| inclusive-or)

(define-binary-op logical-and
    inclusive-or :&& logical-and)

(define-binary-op logical-xor
  logical-and :^^ logical-xor)

(define-binary-op logical-or
  logical-xor :\|\| logical-or)

(define-reference conditional-expression
  conditional
  logical-or)

(define-object conditional
    (and (v logical-or) :? (v expression) :\: (v assignment-expression)))

(define-reference assignment-expression
  assignment
  conditional-expression)

(define-object assignment
    (and (v unary-expression)
         (v (or := :*= :/= :%= :+= :-= :<= :>= :&= :^= :\|=))
         (v assignment-expression)))

(define-reference expression
  assignment-expression
  multiple-expressions)

(define-object multiple-expressions
    (and (v assignment-expression (+ (and :\, (v assignment-expression))))))

(define-reference constant-expression
  conditional-expression)

(define-reference declaration
  (and (v (or function-prototype
              variable-declaration
              precision-declarator
              struct-declaration))
       :\;))

(define-object function-prototype
    (and (? (v type-qualifier)) (v type-specifier) (v identifier)
         :\( (? (v parameter-declaration)) (* (and :\, (v parameter-declaration))) :\)))

(define-object parameter-declaration
    (and (? (v type-qualifier)) (v type-specifier)
         (? (v identifier)) (? (v array-specifier))))

(define-object precision-declarator
    (and :precision (v (or :highp :mediump :lowp)) (v type-specifier)))

(define-object variable-declaration
    (and (? (v type-qualifier)) (v type-specifier) (v variable-initializer)
         (* (and :\, (v variable-initializer)))))

(define-object variable-initializer
    (and (v identifier) (? (v array-specifier)) (? (v (and := initializer))))
  v)

(define-reference invariant-qualifier
  (any (:invariant)))

(define-reference interpolation-qualifier
  (any (:smooth :flat :noperspective)))

(define-object layout-qualifier
    (and :layout :\( (v layout-qualifier-id) (* (and :\, (v layout-qualifier-id))) :\)))

(define-object layout-qualifier-id
    (or (v :shared)
        (and (v identifier) (? (and := (v constant-expression))))))

(define-reference precise-qualifier
  :precise)

(define-object type-qualifier
    (+ (v (or storage-qualifier
              subroutine-qualifier
              layout-qualifier
              precision-qualifier
              interpolation-qualifier
              invariant-qualifier
              precise-qualifier))))

(define-reference storage-qualifier
  (any (:const :inout :in :out :centroid :patch :sample
        :uniform :buffer :shared :coherent :volatile
        :restrict :readonly :writeonly)))

(define-object subroutine-qualifier
    (and :subroutine (? (and :\( (v type-name) :\)))))

(define-reference precision-qualifier
  (any (:highp :mediump :lowp)))

(define-object type-specifier
    (and (v type-specifier-nonarray) (? (v array-specifier))))

(define-object array-specifier
    (+ (and :\[ (? (v constant-expression)) :\])))

(define-reference type-specifier-nonarray
  basic-type
  struct-specifier
  type-name)

(define-object type-name
    (v identifier))

(define-reference basic-type
  (any (;; Transparent Types
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
        :usamplerCubeArray :uimageCubeArray)))

(define-object struct-specifier
    (and :struct (? (v identifier)) :\{ (+ (v struct-declaration)) :\}))

(define-object struct-declaration
    (and (? (v type-qualifier)) (v type-specifier)
         (v struct-declarator) (* (and :\, (v struct-declarator))) :\;))

(define-object struct-declarator
    (and (v identifier) (? (v array-specifier))))

(define-reference initializer
  array-initializer
  assignment-expression)

(define-object array-initializer
    (and :\{ (v initializer) (* (and :\, (v initializer))) (? :\,) :\}))

(define-reference statement
  simple-statement
  compound-statement
  preprocessor-directive)

(define-reference simple-statement
  declaration
  expression-statement
  selection-statement
  switch-statement
  case-label
  iteration-statement
  jump-statement
  :\;)

(define-object compound-statement
    (and :\{ (* (v statement)) :\}))

(define-reference expression-statement
  (and (v expression) :\;))

(define-object selection-statement
    (and :if :\( (v expression) :\) (v statement) (? (and :else (v statement)))))

(define-reference condition
  expression
  condition-declarator)

(define-object condition-declarator
    (and (? (v type-qualifier)) (v type-specifier)
         (v identifier) := (v initializer)))

(define-object switch-statement
    (and :switch :\( (v expression) :\)
         :\{ (* (v statement)) :\}))

(define-object case-label
    (or (and :case (v expression) :\:)
        (and (v :default) :\:)))

(define-reference iteration-statement
  while-statement
  do-statement
  for-statement)

(define-object while-statement
    (and :while :\( (v condition) :\) (v statement)))

(define-object do-statement
    (and :do (v statement) :while :\( (v expression) :\) :\;))

(define-object for-statement
    (and :for :\(
         (v (or expression-statement declaration))
         (v (? condition)) :\;
         (v (? expression)) :\)
         (v statement)))

(define-reference jump-statement
  continue
  break
  return
  discard)

(define-object continue
    (and :continue :\;))

(define-object break
    (and :break :\;))

(define-object return
    (and :return (? (v expression)) :\;))

(define-object discard
    (and :discard :\;))

(define-object function-definition
    (and (v function-prototype) (v compound-statement)))

(define-object shader
    (* (v (or function-definition declaration preprocessor-directive))))
