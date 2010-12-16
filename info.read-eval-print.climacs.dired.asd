;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :info.read-eval-print.climacs.dired
  :serial t
  ;; add new files to this list:
  :components ((:file "package")
               (:file "dired-syntax"))
  :depends-on (:climacs :quek))
