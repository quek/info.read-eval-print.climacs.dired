;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :info.read-eval-print.climacs.dired
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "info.read-eval-print.climacs.dired"))
  :depends-on (#+nil :cl-ppcre))
