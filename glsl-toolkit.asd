(asdf:defsystem glsl-toolkit
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to parse and modify OpenGL Shader Language (GLSL) source code"
  :homepage "https://Shirakumo.github.io/glsl-toolkit/"
  :bug-tracker "https://github.com/Shirakumo/glsl-toolkit/issues"
  :source-control (:git "https://github.com/Shirakumo/glsl-toolkit.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "parser")
               (:file "grammar")
               (:file "printer")
               (:file "walker")
               (:file "merge")
               (:file "sexpr")
               (:file "transform")
               (:file "method-combination")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :parse-float
               :trivial-indent
               :cl-ppcre))
