;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :info.read-eval-print.climacs.dired
  (:use :clim :clim-extensions :drei-buffer :drei-base
	:drei-syntax :drei-fundamental-syntax :flexichain :drei
	:drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io
	:drei-lr-syntax
        :climacs-core
        :quek)
  (:shadowing-import-from :clim-lisp-patch #:interactive-stream-p)
  (:shadowing-import-from :drei-syntax #:syntax)
  (:export :dired-syntax))
