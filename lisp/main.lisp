;;;; -*- mode: lisp; syntax: common-lisp; base: 10; encoding: utf-8 -*-

(in-package #:cl-user)

;; (declaim (optimize (speed 0) (compilation-speed 0) (safety 3) (debug 3)))

(defun home-path (path)
  (merge-pathnames path (user-homedir-pathname)))

(defun load-config (c)
  (let ((base (home-path "~/etc/lisp/")))
    (load (merge-pathnames c base))))

(load-config "asdf")
(load-config "quicklisp")
(load-config "impl")
(load-config "vega")
