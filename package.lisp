#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:glsl-parser
  (:nicknames #:org.shirakumo.trial.glsl.parser)
  (:use #:cl)
  ;; grammar.lisp
  (:export
   #:whitespace
   #:integer-token
   #:decimal-token
   #:octal-token
   #:hexadecimal-token
   #:float-token
   #:identifier-token
   #:keyword-token
   #:preprocessor-token
   #:= #:+= #:-= #:*= #:/= #:%= #:<<= #:>>= #:&= #:^= #:\|=
   #:++ #:-- #:<< #:>>  #:^^ #:\|\| #:&& #:<= #:>= #:< #:>
   #:+ #:- #:* #:/ #:% #:& #:^ #:! #:\|
   #:\( #:\) #:\[ #:\] #:\{ #:\} #:\; #:\. #:? #:\: #:\,
   #:token
   #:tokenize
   #:integer-constant
   #:float-constant
   #:boolean-constant
   #:identifier
   #:preprocessor-directive
   #:primary-expression
   #:postfix-expression
   #:modified-reference
   #:reference-modifier
   #:field-modifier
   #:array-modifier
   #:increment-modifier
   #:decrement-modifier
   #:call-modifier
   #:same-+
   #:negation
   #:inversion
   #:bit-inversion
   #:prefix-increment
   #:prefix-decrement
   #:unary-expression
   #:multiplication
   #:division
   #:modulus
   #:addition
   #:subtraction
   #:left-shift
   #:right-shift
   #:less-than
   #:greater-than
   #:less-equal-than
   #:greater-equal-than
   #:equal
   #:not-equal
   #:bitwise-and
   #:exclusive-or
   #:inclusive-or
   #:logical-and
   #:logical-xor
   #:logical-or
   #:conditional-expression
   #:conditional
   #:assignment-expression
   #:assignment
   #:expression
   #:multiple-expressions
   #:constant-expression
   #:declaration
   #:function-declaration
   #:function-prototype
   #:parameter-declaration
   #:precision-declaration
   #:variable-declaration
   #:variable-initializer
   #:invariant-qualifier
   #:interpolation-qualifier
   #:layout-qualifier
   #:layout-qualifier-id
   #:precise-qualifier
   #:storage-qualifier
   #:subroutine-qualifier
   #:precision-qualifier
   #:type-qualifier
   #:type-specifier
   #:array-specifier
   #:type-specifier-nonarray
   #:type-name
   #:basic-type
   #:struct-specifier
   #:struct-declaration
   #:struct-declarator
   #:struct-field-declarator
   #:initializer
   #:array-initializer
   #:statement
   #:simple-statement
   #:compound-statement
   #:expression-statement
   #:selection-statement
   #:condition
   #:condition-declarator
   #:switch-statement
   #:case-label
   #:iteration-statement
   #:while-statement
   #:do-statement
   #:for-statement
   #:jump-statement
   #:continue
   #:break
   #:return
   #:discard
   #:function-definition
   #:shader)
  ;; merge.lisp
  (:export
   #:uniquify
   #:matching-qualifiers-p
   #:matching-specifiers-p
   #:matching-declarators-p
   #:merge-shaders
   #:merge-shader-sources)
  ;; parser.lisp
  (:export
   #:no-value
   #:end-of-stream-p
   #:advance
   #:backtrack
   #:peek
   #:consume
   #:with-token-input
   #:rule
   #:remove-rule
   #:consume-whitespace
   #:consume-string
   #:consume-any
   #:consume-notany
   #:compile-rule
   #:define-rule
   #:define-reference
   #:define-object
   #:normalize-shader-source
   #:lex
   #:parse
   #:trace-parse
   #:untrace-parse)
  ;; printer.lisp
  (:export
   #:serialize
   #:sformat
   #:with-indentation
   #:indent
   #:serializer
   #:remove-serializer
   #:define-serializer
   #:define-serialization
   #:serialize-part)
  ;; toolkit.lisp
  (:export
   #:*glsl-keywords*
   #:*glsl-keyword-symbols*)
  ;; walker.lisp
  (:export
   #:environment
   #:binding
   #:make-environment
   #:root-environment-p
   #:preprocessor-p
   #:constant-p
   #:declaration-p
   #:expression-p
   #:control-flow-p
   #:keyword-p
   #:statement-p
   #:identifier-p
   #:global-identifier-p
   #:local-identifier-p
   #:variable-identifier-p
   #:function-identifier-p
   #:walk
   #:walk-part
   #:walker
   #:remove-walker
   #:define-walker
   #:define-walking-body
   #:define-unary-op-walker
   #:define-binary-op-walker
   #:define-empty-op-walker))

(defpackage #:glsl-parser-rules
  (:nicknames #:org.shirakumo.trial.glsl.parser.rules)
  (:use))
