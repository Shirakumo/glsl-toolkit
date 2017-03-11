#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

;; grammar.lisp
(docs:define-docs
  (function define-operator-objects
    "Shorthand to define an object that parses a string and returns the keyword version of that string.

Used to define the various token objects.

See DEFINE-OBJECT")

  (function define-binary-op
    "Shorthand to define a binary operator object.

This takes care to avoid duplicate parsing of the same sequence
if the subsequence should succeed. Meaning that if the left
expression matches, but the operator or the right one does not
it simply returns the left expression, instead of failing to
match and causing a re-matching lower down in the tree.

Without this optimisation, performance would suffer greatly.

See DEFINE-OBJECT"))

;; merge.lisp
(docs:define-docs
  (variable *unique-counter*
    "Counter to hold the current index used to compute names for unique identifiers.

See UNIQUIFY")

  (function uniquify
    "Create a (hopefully) unique identifier for the given name.

The returned name is prefixed by two underscores. Identifiers
like that are reserved for use by the underlying library or
framework (us), so there should not be any clash with user
identifiers unless the shader is not conforming to begin with.

See *UNIQUE-COUNTER*")

  (function matching-qualifiers-p
    "Returns true if the two given qualifier lists are considered to match.

The following qualifier parts are not considered:
 :HIGHP :MEDIUMP :LOWP :INVARIANT :PRECISE :SMOOTH :FLAT :NOPERSPECTIVE

All other qualifiers must match by EQUAL, but don't have to be
in the same order.

See https://www.khronos.org/opengl/wiki/Shader_Compilation#Qualifier_matching")

  (function matching-specifiers-p
    "Returns true if the two given specifier lists are considered to match.

In order to match, the two lists have to be EQUAL.")

  (function matching-declarators-p
    "Returns true if the two variable declarations are considered to match.

This is true if:
- The first of both lists (qualifiers) match by MATCHING-QUALIFIERS-P
- The second of both lists (specifiers) match by MATCHING-SPECIFIERS-P
- The fourth of both lists (array-identifiers) match by EQUAL

The third of both lists (identifiers) must not match.
The fifth of both lists (initializers) must not match.")

  (function find-layout-qualifier
    "Find the layout qualifier object in the qualifiers list.")

  (function find-direction-qualifier
    "Find the direction qualifier keyword in the qualifiers list.")

  (function handle-declaration
    "Handles a declaration during a shader merging operation.

This will take care of registering the identifier with the global
environment, substituting the name if necessary, merging the
declarations if possible, and erroring if there is a severe
mismatch that cannot be resolved.

The merging of variable declarations is in part according to the
OpenGL specification on interface matching between shader stages.
More specifically, the following strategy is employed here:
- If the declaration is a pipeline declaration (in/out/inout)
  - If it has a layout qualifier
    - If there is no known matching layout qualifier, register it
    - If there is one and the declarations match, map the name to
      that of the previous declaration and remove the current one
    - Otherwise error as there are conflicting declarations that
      cannot be rectified
  - If the identifier is already known
    - If the declarations of this one and the previous declaration
      of the same identifier match, map the name to that of the
      previous declaration and remove the current one
    - Otherwise warn about the possible mismatch and remap the
      current identifier to a new one
  - Store the identifier directly and return the AST as-is
- If the identifier is already known
  - Remap the identifier to a new one
- Otherwise store the identifier directly and return the AST as-is

See https://www.khronos.org/opengl/wiki/Shader_Compilation#Interface_matching
See FIND-DIRECTION-QUALIFIER
See FIND-LAYOUT-QUALIFIER
See MATCHING-DECLARATORS-P
See MERGE-SHADERS")

  (function handle-identifier
    "Handles an identifier during a shader merging operation.

This will take care of substituting the identifier if it has
been remapped globally.

See GLOBAL-IDENTIFIER-P
See MERGE-SHADERS")

  (function merge-shaders
    "Merge the given shader ASTs into a single AST.

The top-level AST nodes must be AST objects of type SHADER.

The merging will attempt to conflate declarations where
possible and rename variables where necessary, in order to
create a single shader that is internally consistent.

It also emits a single main function at the end, which
does nothing but call the main function of each
sub-shader in the sequence that the shaders were passed.

See HANDLE-DECLARATION
See HANDLE-IDENTIFIER
See WALK
See MERGE-SHADER-SOURCES")

  (function merge-shader-sources
    "Convenience function to merge the sources of multiple shaders into a single one.

See PARSE
See MERGE-SHADERS
See SERIALIZE"))

;; parser.lisp
(docs:define-docs
  (variable *token-array*
    "Holds a vector of tokens to be processed by the parser.

See PEEK
See CONSUME
See *TOKEN-INDEX*
See WITH-TOKEN-INPUT")

  (variable *token-index*
    "Holds the index into the token array that represents the current parser position.

See ADVANCE
See BACKTRACK
See *TOKEN-ARRAY*
See WITH-TOKEN-INPUT")

  (variable no-value
    "Evaluates to itself, namely the symbol NO-VALUE

Represents the absence of a value in AST objects.")

  (type index
    "Type specifier for a token index.")

  (function end-of-tokens-p
    "Returns true if the end of the token array has been reached.

See *TOKEN-ARRAY*
See *TOKEN-INDEX*")

  (function advance
    "Advances the current token index.

See *TOKEN-INDEX*
See BACKTRACK")

  (function backtrack
    "Reduces the current token index.

See *TOKEN-INDEX*
See ADVANCE")

  (function peek
    "Returns the token at the index relative to the current position.

See *TOKEN-ARRAY*
See *TOKEN-INDEX*")

  (function consume
    "Returns the token at the current index and advances the index by one.

See PEEK
See ADVANCE")

  (function with-token-index
    "Readies the environment for token parsing.

This binds *TOKEN-ARRAY* to the given vector and binds *TOKEN-INDEX* to 0.

See *TOKEN-ARRAY*
See *TOKEN-INDEX*")

  (function rule
    "Returns the symbol that identifies the parsing rule of the given name.

This is a place that can be set with the function object
that should be used to parse the rule of the given name.

If no such rule exists, an error is signalled.")

  (function remove-rule
    "Removes the parsing rule of the given name.")

  (function consume-whitespace
    "Consumes all spaces and newlines in the token array from the current position on.")

  (function consume-string
    "Attempts to consume the given string from the token array.

If the string matches, it is returned. Otherwise, NIL is
returned instead. If the match succeeds, the token index
is modified. Otherwise it is reset to the point where it
was before the match was attempted.")

  (function consume-any
    "Consume any of the tokens in the choices sequence, if possible.

If a token matches, it is returned. Otherwise NIL is
returned instead. The index is only modified if a match
occurs.")

  (function consume-notany
    "Consume any token that is not one of the tokens in the choices sequence.

If a token matches, it is returned. Otherwise NIL is
returned instead. The index is only modified if a match
occurs.")

  (function compile-rule
    "Compile the rule s-expression.

The following types are handled specially:
- NULL       NIL is returned
- KEYWORD    Attempts to match a token that is EQ to this
             keyword. On success returns the keyword.
- SYMBOL     Attempts to match the rule given named by the
             symbol. Returns whatever the rule returns.
- CHARACTER  Attempts to match a token that is EQL to this
             character. Returns the character on match.
- STRING     Attempts to match the string against the tokens.
             Returns the string on successful match.
- CONS       One of the following compound, identified by the
             first symbol.
  - AND      Matches if all of the sub-rules match. Returns
             the last rule's return value on successful match.
  - OR       Matches if any of the sub-rules match.
             Returns the first successful rule's return value.
  - NOTANY   Matches if none of the choices match.
             Returns the token that did not match.
  - ANY      Matches if any of the choices match.
             Returns the token that did match.
  - WHEN     Performs all the other sub-rules only if the
             first sub-rule matches. Returns the last sub-rule's
             return value.
  - V        Makes sure the result of the sub-rule is added to
             the values list if the sub-rule matches. Returns
             what the sub-rule returned.
  - *        Repeatedly matches the sub-rule as many times as
             possible. Returns T.
  - +        Attempts to match the sub-rule at least once.
             Returns T on success.
  - ?        Attempts to match the sub-rule. If it does not
             match the secondary form is returned, or NO-VALUE.
  - !        Evaluates the sub-rule and returns its result, but
             always resets the token index to its initial value.
  - Otherwise the rule is returned unchanged.

See CONSUME-STRING
See CONSUME-ANY
See CONSUME-NOTANY
See DEFINE-RULE")

  (function define-rule
    "Defines a new parsing rule of the given name.

This will create a function definition in the
ORG.SHIRAKUMO.TRIAL.GLSL.RULES package by
re-interning the symbol in that package.

A default lexical binding named V is provided.

See DEFINE-REFERENCE
See DEFINE-OBJECT")

  (function define-reference
    "Defines a reference parsing rule.

The body should be a number of sub-rules that may be matched
in order to match this rule. Either the value stored in V
by the V function, or the return value of the first matching
sub-rule is returned.

See DEFINE-RULE")

  (function define-object
    "Defines a parsing object.

The RULE should be a parsing rule to match against. It should
probably contain calls to the V rule in order to populate the
V values list. This list is used to store the return values
of the object.

TRANSFORM is an optional list of forms to be evaluated to
transform the values list on a successful match. It acts as
an implicit PROGN and the last value is returned as the value
of the rule.

If no TRANSFORM is given, the return value is the V values
list prepended with the name of the rule.

See DEFINE-RULE")

  (function newline-p
    "Returns true if the input is a newline character and thus either CR or LF.")

  (function normalize-shader-source
    "Attempts to normalise the shader source code.

This does the following:
- Removes any and all comments from the code
- Handles the backslash-before-newline trick to get multiple
  lines to act as one.
- Converts CRLF/LFCR/LFLF/CRCR into NEWLINE
- Converts TAB to SPACE
- Converts consecutive whitespace into singular whitespace
  while preserving newlines.

The input may be one of the following types:
- PATHNAME
- STRING
- STREAM

See NEWLINE-P")

  (function lex
    "Lex the input string into a token array for use in parsing.

See NORMALIZE-SHADER-SOURCE
See RULE
See PARSE")

  (function parse
    "Parses the given GLSL shader source input into an AST.

The input may be of the following types:
- STRING STREAM PATHNAME
  The input is lexed before parsing as by LEX
- LIST
  The input is converted into a vector
- VECTOR
  The input is parsed by the given toplevel parsing rule.

See LEX
See RULE")

  (variable *traced*
    "Hash table to hold associate names with the original function definitions.")

  (variable *trace-level*
    "Integer to represent the current stack level during tracing.")

  (function call-traced-function
    "Wrapper to output trace information around the call to the given function.

See *TRACE-LEVEL*
See *TRACED*")

  (function trace-parse-func
    "Ensures the given function is being traced for parsing, if it isn't already.

This replaces the global function definition.

See CALL-TRACED-FUNCTION
See *TRACED*
See UNTRACE-PARSE-FUNC")

  (function untrace-parse-func
    "Ensures the given functions restored to its original definition, if it isn't already.

This replaces the global function definition.

See *TRACED*
See TRACE-PARSE-FUNC")

  (function trace-parse
    "Cause all parse rule functions to emit tracing information.

See UNTRACE-PARSE
See TRACE-PARSE-FUNC")

  (function untrace-parse
    "Make all parse rule functions cease to emit tracing information.

See TRACE-PARSE
See UNTRACE-PARSE-FUNC"))

;; printer.lisp
(docs:define-docs
  (variable *serialize-stream*
    "The stream to which the serializing output is sent to.

This has to be bound when SERIALIZE-PART is called.")

  (function serialize
    "Serializes the AST part to shader source.

TO may be one of the following:
- NULL
  The output is gathered into a string and returned.
- T
  The output is sent to *STANDARD-OUTPUT*.
- STREAM
  The output is sent to this stream.
- PATHNAME
  The output is written to the file. If the file already
  exists, an error is signalled.

See *SERIALIZE-STREAM*
See SERIALIZE-PART")

  (function sformat
    "Convenience function used to format to the serializing stream.

A special format directive ~O is provided as well, which
causes SERIALIZE-PART to be called on the respective object.")

  (function %format-object
    "Helper function to call SERIALIZE-PART in a format string.")

  (variable *indent*
    "Variable to represent the current indenting level.

See WITH-INDENTATION
See INDENT")

  (function with-indentation
    "Makes sure the body is evaluated with an increased indentation level.

See *INDENT*
See INDENT")

  (function indent
    "Starts a fresh line and emits as many spaces as the *INDENT* variable dictates.")

  (function compile-format-string
    "Rewrite the format string so that the ~O directive is acceptable.")

  (variable *serializers*
    "Hash table associating AST object types to serializer functions.

The function must accept a single argument, which is the
AST object itself.

See SERIALIZER
See REMOVE-SERIALIZER")

  (function serializer
    "Accessor to the serializing function for AST objects of the given type.

See *SERIALIZERS*
See DEFINE-SERIALIZER
See REMOVE-SERIALIZER")

  (function remove-serializer
    "Removes the serializer function for AST objects of the given type.

See *SERIALIZERS*
See SERIALIZER")

  (function define-serializer
    "Convenience function to define a serializer function for AST objects of the given type.

See SERIALIZER")

  (function define-serialization
    "Convenience function to define the serialization of AST objects of the given type.

ARGS must be a lambda-list to destructure the contents of the
AST object.

See SERIALIZER")

  (function serialize-part
    "Serializes the AST part.

This appropriately handles all values that can be contained in the AST.
For AST objects, an appropriate serializer function is called if possible.
Should an unknown AST object occur, an error is signalled.

See SERIALIZER"))

;; toolkit.lisp
(docs:define-docs
  (function enlist
    "Ensures that LIST is a list.

If it is not, it is combined with ITEMS to form a list.")

  (function mapcar*
    "Like CL:MAPCAR, but only gathers non-NIL results.")

  (function find-any
    "Like CL:FIND, but the item to find is a sequence of things that can be found.")

  (function merge-plists
    "Merges the two plists together by appending their values for the same keys.

Returns a fresh plist.")

  (variable *glsl-keywords*
    "List to all the keywords in GLSL shader files.

This does not include terminals and other keywords
such as {}/*+ etc.")

  (variable *glsl-keyword-symbols*
    "List to all the keywords in GLSL shader files but as interned and upcased keyword symbols.

See *GLSL-KEYWORDS*"))

;; walker.lisp
(docs:define-docs
  (type environment
    "Struct to hold information about the lexical environment during code walking.

See MAKE-ENVIRONMENT
See ROOT
See BINDINGS")

  (function %make-environment
    "Construct a new environment object.

See ENVIRONMENT
See MAKE-ENVIRONMENT")

  (function root
    "Accessor to the root environment.

The root environment is the top-level lexical environment that
holds all global definitions. On root environments, this must
resolve to the environment instance itself.

See ENVIRONMENT")

  (function bindings
    "Accessor to the table associating identifiers to bindings.

The values must be lists where the first item is a keyword
that identifies the type of binding as either a :FUNCTION
or :VARIABLE binding.

See ENVIRONMENT
See BINDING")

  (function binding
    "Accessor to the binding in the environment for the given name.

See BINDINGS
See ENVIRONMENT")

  (function make-environment
    "Create a new environment object.

If not parent environment is passed in, the environment is
assumed to be a top-level root environment.

See ENVIRONMENT")

  (function root-environment-p
    "Returns T if the environment is a top-level root environment.

See ENVIRONMENT
See ROOT")

  (function preprocessor-p
    "Returns T if the given AST node is a preprocessor instruction.")

  (function constant-p
    "Returns T if the given AST node is a constant value.")

  (function declaration-p
    "Returns T if the given AST node is a declaration statement.")

  (function expression-p
    "Returns T if the given AST node is an expression.")

  (function control-flow-p
    "Returns T if the given AST node is a control-flow instruction.")

  (function keyword-p
    "Returns T if the given AST node is a GLSL keyword.

See *GLSL-KEYWORD-SYMBOLS*")

  (function statement-p
    "Returns T if the given AST node is a statement.

See DECLARATION-P
See EXPRESSION-P
See CONTROL-FLOW-P")

  (function identifier-p
    "Returns T if the given AST node might be an identifier.

This is not always accurate, as some identifiers can also be
types at the same time. It thus depends on the context.")

  (function global-identifier-p
    "Returns T if the given AST node is an identifier that refers to a global definition.")

  (function local-identifier-p
    "Returns T if the given AST node is an identifier that refers to a local definition.")

  (function variable-identifier-p
    "Returns T if the given AST node is an identifier for a variable.")

  (function function-identifier-p
    "Returns T if the given AST node is an identifier for a function.")

  (function walk
    "Walk over the AST, calling FUNCTION on each interesting node.

Returns a fresh AST that was constructed by the function.

The function will be called with three arguments:
- The AST node at the current point
- The surrounding context in which the AST node is
- The environment object that maintains lexical information

The function should return a single value, which is the
value that should be put into a fresh AST in place of the
original node.

Note that calling any of the environment inspection functions
on an identifier in a lower level than the current AST node
that the function received is not going to work. The lexical
information is only guaranteed to be ready by the time the
function is called with the identifier itself.

See ROOT-ENVIRONMENT-P
See PREPROCESSOR-P
See CONSTANT-P
See DECLARATION-P
See EXPRESSION-P
See CONTROL-FLOW-P
See KEYWORD-P
See STATEMENT-P
See IDENTIFIER-P
See GLOBAL-IDENTIFIER-P
See LOCAL-IDENTIFIER-P
See VARIABLE-IDENTIFIER-P
See FUNCTION-IDENTIFIER-P
See ENVIRONMENT
See WALK-PART")

  (function walk-part
    "Walk over the given AST node.

On AST objects, this will call out to the respective
walker function.

See WALKER")

  (variable *walkers*
    "Hash table associating AST object types to walker functions.

A walker function must accept three arguments:
- The AST object to process
- The function that should walk over the AST
- The current lexical environment
It must return an AST object to use in place of the
current one in the resulting AST.

See WALKER
See REMOVE-WALKER
See DEFINE-WALKER")

  (function walker
    "Accessor to the walker function for AST objects of the given type.

See *WALKERS*
See REMOVE-WALKER
See DEFINE-WALKER")

  (function remove-walker
    "Removes the walker function for AST objects of the given type.

See WALKER
See *WALKERS*")

  (function define-walker
    "Define a new walker function that is responsible for walking over a particular type of AST object node.

See *WALKERS*
See WALKER
See DEFINE-WALKING-BODY")

  (function define-walking-body
    "Convenience definition macro.

The ARGS should be a destructuring-bind lambda-list to
destructure the contents of the object.

The body should be forms that provide the values to use in the
resulting AST object. The last value should be the tail of the
object's list. Thus this is about equivalent to

  (define-walker type (o)
    (destructuring-bind .. (o)
      (list* 'type body)))

Within the body the WALK function is rebound to one that can
be conveniently used to recursively walk the AST. It only needs
the new node to walk over. It optionally takes a new environment
to supply.

You can reach the other function values like the full AST, the
walk function, and the environment by changing the type to a
list and providing the binding symbols as keyword arguments in
it with :KEY :FUNC and :ENV.

See DEFINE-WALKER")

  (function define-empty-op-walker
    "Define a walker for an empty AST object.

This walker function does nothing but construct a fresh return
value.")

  (function define-unary-op-walker
    "Define a walker for a unary AST object.

This walker recurses over the single node in the object
and returns a fresh value constructed from it.")

  (function define-binary-op-walker
    "Define a walker for a binary AST object.

This walker recurses over the left and right nodes in the object
and returns a fresh value constructed from them."))
