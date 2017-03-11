#|
 This file is a part of glsl-toolkit
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trial.glsl)

(defun enlist (list &rest items)
  (if (listp list) list (list* list items)))

(defun mapcar* (function list)
  (loop for item in list
        for result = (funcall function item)
        when result collect result))

(defun find-any (choices sequence)
  (find choices sequence :test (lambda (a b) (find b a))))

(defun merge-plists (a b)
  (let ((res (copy-list a)))
    (loop for (key val) on b by #'cddr
          do (setf (getf key res)
                   (append (getf key res) val)))
    res))

(defvar *glsl-keywords*
  '("writeonly" "while" "volatile" "void" "vec4" "vec3" "vec2" "varying" "uvec4"
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

(defvar *glsl-keyword-symbols*
  (loop for item in *glsl-keywords*
        collect (intern (string-upcase item) :keyword)))
