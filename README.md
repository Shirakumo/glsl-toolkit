## About GLSL-Toolkit
This is a collection of tools written to allow you to wrangle OpenGL Shader Language (GLSL) source files. The library was written for GLSL4.5 sources, but should work with higher or lower versions as well.

## How To
The primary functionality that this library gives you is parsing, serialising, and walking GLSL source code.

    (glsl-toolkit:parse "int a = 0;
                         void main(){
                           int b = 1;
                           a = b;
                         }")

This will lex and parse the source into an AST. You can then turn the AST back into a code representation.

    (glsl-toolkit:serialize *)

The library will take certain liberties at transforming the code while parsing and printing. This is done to normalise the code and make it easier to walk and change. The semantic meaning of the code should however be preserved verbatim. If this is not the case, please [file an issue](https://github.com/Shirakumo/glsl-toolkit/issues).

Aside from simply printing the source out again, you can also walk over it and transform it in a syntactically and semantically useful way. This way you can refactor the code. A simple example is in order.

    (glsl-toolkit:serialize
      (glsl-toolkit:walk **
        (lambda (ast context environment)
          (if (glsl-toolkit:global-identifier-p ast environment)
              (format NIL "__~a" ast)
              ast))))

The code walker provides access to a number of predicates that allow you to figure out properties of the current node and the lexical meaning of identifiers. Have a look at the symbol index for the various predicate functions.

Finally, the library provides a way to merge individual shader files together. It does so while attempting to either unify or rename global identifiers that might clash. This is useful when multiple shader effects need to be chained together in a single shader pass.

    (glsl-toolkit:merge-shader-sources '("
      out layout (location = 0) vec4 position;
      
      void foo();
      
      void main(){
        position += vec4(1, 2, 3, 4);
      }" "
      out layout (location = 0) vec4 pos;
      in layout (location = 0) vec4 col;
      
      void foo(){
        a();
      }
      
      void main(){
        pos += col;
      }"))

The merging does not come without its caveats. At times, it may be impossible to merge due to conflicting type declarations, and other times the system may not recognise a possible merge due to name or qualifier mismatch. While it should work for most cases, some exotic cases are likely to fail at may need manual intervention.

Some caveats exist in the parser as well. Since we cannot implement the behaviour of the preprocessor ourselves, we instead must opt for allowing preprocessor directives verbatim at certain points in the parse tree. We opt for allowing them at any point where a statement or declaration might occur, but not within expressions.
