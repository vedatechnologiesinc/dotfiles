;;;; -*- mode: lisp; syntax: common-lisp; base: 10; encoding: utf-8 -*-

(add-to-registry "quicklisp/quicklisp/")

#-quicklisp
(let ((file (home-path "quicklisp/setup.lisp")))
  (when (probe-file file)
    (load file)))

#+(and quicklisp clisp abcl ecl)
(ql:quickload :asdf)

