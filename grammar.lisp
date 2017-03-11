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
           (v (? (any "uU") "s"))))

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
      (and (v #.(list* 'or *glsl-keywords*))
           (! (or whitespace operator)))
  (intern (string-upcase (first v)) :keyword))

(define-object preprocessor-token
      (and (v #\#) (* (v (notany (#\Newline)))))
  (list 'preprocessor-directive (coerce v 'string)))

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
  (destructuring-bind (type int sign) (consume)
    (declare (ignore type))
    (if (string= "s" sign)
        int
        `(unsigned-int ,int))))

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

(define-reference unary-expression
  postfix-expression
  prefix-increment
  prefix-decrement
  same-+
  negation
  inversion
  bit-inversion)

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
  (or function-declaration
      variable-declaration
      precision-declaration
      struct-declaration))

(define-object function-declaration
      (and (v function-prototype) :\;))

(define-object function-prototype
      (and (v (? type-qualifier)) (v type-specifier) (v identifier)
           :\( (? (v parameter-declaration)) (* (and :\, (v parameter-declaration))) :\)))

(define-object parameter-declaration
      (and (? (v type-qualifier)) (v type-specifier)
           (? (v identifier)) (? (v array-specifier)))
  v)

(define-object precision-declaration
      (and :precision (v (or :highp :mediump :lowp)) (v type-specifier) :\;))

(define-object variable-declaration
      (and (v (? type-qualifier)) (v type-specifier)
           (v variable-initializer) (* (and :\, (v variable-initializer)))
           :\;)
  (destructuring-bind (qualifier specifier &rest initializers) v
    (if (rest initializers)
        (list* 'multiple-statements
               (loop for initializer in initializers
                     collect (list* 'variable-declaration qualifier specifier initializer)))
        (list* 'variable-declaration qualifier specifier (first initializers)))))

(define-object variable-initializer
      (and (v identifier) (v (? array-specifier)) (? (v (and := initializer))))
  v)

(define-reference invariant-qualifier
  :invariant)

(define-reference interpolation-qualifier
  (any (:smooth :flat :noperspective)))

(define-object layout-qualifier
      (and :layout :\( (v layout-qualifier-id) (* (and :\, (v layout-qualifier-id))) :\)))

(define-object layout-qualifier-id
      (or (v :shared)
          (and (v identifier) (? (and := (v constant-expression))))))

(define-reference precise-qualifier
  :precise)

(define-reference storage-qualifier
  (any (:const :inout :in :out :centroid :patch :sample
               :uniform :buffer :shared :coherent :volatile
        :restrict :readonly :writeonly)))

(define-object subroutine-qualifier
      (and :subroutine (? (and :\( (v type-name) :\)))))

(define-reference precision-qualifier
  (any (:highp :mediump :lowp)))

(define-object type-qualifier
    (+ (v (or storage-qualifier
              subroutine-qualifier
              layout-qualifier
              precision-qualifier
              interpolation-qualifier
              invariant-qualifier
              precise-qualifier))))

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
        :vec2 :vec3 :vec4 :mat2 :mat3 :mat4
        :bvec2 :bvec3 :bvec4 :ivec2 :ivec3 :ivec4
        :uvec2 :uvec3 :uvec4 :dvec2 :dvec3 :dvec4
        :mat2x2 :mat2x3 :mat2x4 :mat3x2 :mat3x3 :mat3x4
        :mat4x2 :mat4x3 :mat4x4 :dmat2 :dmat3 :dmat4
        :dmat2x2 :dmat2x3 :dmat2x4 :dmat3x2
        :dmat3x3 :dmat3x4 :dmat4x2 :dmat4x3 :dmat4x4
        ;; Floating-Point Opaque Types
              :sampler1DShadow
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
        :sampler2DShadow :smapler2DRectShadow
        :sampler1DArrayShadow :sampler2DArrayShadow
        :samplerCubeShadow :samplerCubeArrayShadow
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
        :usamplerCubeArray :uimageCubeArray
        :atomic_uint)))

(define-object struct-specifier
      (and :struct (v identifier)))

(define-object struct-declaration
      (and :struct (v (? identifier)) (? (and :\{ (+ (v struct-declarator)) :\}))))

(define-object struct-declarator
      (and (v (? type-qualifier)) (v type-specifier)
           (v struct-field-declarator) (* (and :\, (v struct-field-declarator))) :\;))

(define-object struct-field-declarator
      (and (v identifier) (? (v array-specifier)))
  v)

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
      (or (and :\{ (* (v statement)) :\})
          (v simple-statement)))

(define-reference expression-statement
  (and (v expression) :\;))

(define-object selection-statement
      (and :if :\( (v expression) :\) (v compound-statement)
           (? (and :else (v compound-statement)))))

(define-reference condition
  expression
  condition-declarator)

(define-object condition-declarator
      (and (v (? type-qualifier)) (v type-specifier)
           (v identifier) := (v initializer)))

(define-object switch-statement
      (and :switch :\( (v expression) :\)
           (v compound-statement)))

(define-object case-label
      (or (and :case (v expression) :\:)
          (and (v :default) :\:)))

(define-reference iteration-statement
  while-statement
  do-statement
  for-statement)

(define-object while-statement
      (and :while :\( (v condition) :\) (v compound-statement)))

(define-object do-statement
      (and :do (v compound-statement) :while :\( (v expression) :\) :\;))

(define-object for-statement
      (and :for :\(
           (v (or expression-statement declaration))
           (v (? condition)) :\;
           (v (? expression)) :\)
           (v compound-statement)))

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
      (* (or (v (or function-definition declaration preprocessor-directive))
             #\;)))
