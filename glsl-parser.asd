#|
 This file is a part of glsl-parser
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem for
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to parse OpenGL Shader Language (GLSL)"
  :homepage "https://github.com/Shinmera/glsl-parser"
  :serial T
  :components ((:file "package")
               (:file "parser")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :parse-float))
