;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :info.read-eval-print.climacs.dired)

(define-syntax-command-table dired-table
    :errorp nil)

;;;; dired-syntax
(define-syntax dired-syntax (fundamental-syntax)
  ()
  (:name "Dired")
  (:command-table dired-table))

(defmethod name-for-info-pane ((syntax dired-syntax) &key pane)
  (declare (ignore pane))
  (format nil "Dired"))

(defmethod display-syntax-name ((syntax dired-syntax)
				(stream extended-output-stream) &key pane)
  (declare (ignore pane))
  (princ "Dired" stream))


;;;; dired-mode
(define-syntax-mode dired-mode ()
  ()
  (:documentation "A mode for Directory editing.")
  (:applicable-syntaxes dired-syntax))


(defmethod syntax-command-tables append ((syntax dired-mode))
  '(dired-table))

(defun ensure-pathname-for-dired (path)
  (make-pathname :directory (namestring path)
                 :name :wild))

(defun make-dired-buffer (path)
  (let ((buffer (make-new-buffer)))
    (iterate ((file (scan-directory (ensure-pathname-for-dired path))))
      ;; TODO presentation
      (insert-buffer-object buffer (size buffer)
                            (if (directory-pathname-p file)
                                (car (last (pathname-directory file)))
                                (file-namestring file)))
      (insert-buffer-object buffer (size buffer) #\newline))
    (clear-undo-history buffer)
    buffer))


(defun find-directory (path)
  (cond ((null path)
	 (display-message "No file name given.")
	 (beep))
	((not (directory-pathname-p path))
	 (display-message "~A is a not directory name." path)
	 (beep))
        ((not (probe-file path))
         (display-message "~A is a not found." path)
	 (beep))
        (t
         (let* ((buffer (make-dired-buffer path))
                (view (climacs-core::make-new-view-for-climacs
                       *esa-instance* 'textual-drei-syntax-view
                       :name (namestring path)
                       :buffer buffer)))
           (unless (climacs-core::buffer-pane-p (current-window))
             (climacs-core::other-window (or (find-if (^ typep _window 'climacs-core::climacs-pane)
                                                      (windows *esa-instance*))
                                             (climacs-core::split-window t))))
           (setf (offset (point buffer)) (offset (point view))
                 (syntax view) (make-syntax-for-view view 'dired-syntax)
                 (file-write-time buffer) nil
                 (needs-saving buffer) nil
                 (name buffer) (namestring path))
           (setf (current-view (current-window)) view)
           (evaluate-attribute-line view)
           (setf (filepath buffer) (pathname path)
                 (read-only-p buffer) nil)
           (beginning-of-buffer (point view))
           buffer))))

(define-command (com-dired :name t :command-table esa-io-table)
    ((path 'pathname
           :prompt "Dired (directory): "
           :prompt-mode :raw
           :default (esa-io::directory-of-current-buffer)
           :default-type 'pathname
           :insert-default t))
  "Prompt for a directory name then edit that directory."
  (handler-case (find-directory path)
    (file-error (e)
      (display-message "~a" e))))

#|
(define-command (com-set-syntax :name t :command-table buffer-table) 
    ((syntax 'syntax
      :prompt "Name of syntax"))
  "Prompts for a syntax to set for the current buffer.
   Setting a syntax will cause the buffer to be reparsed using the new syntax."
  (set-syntax (current-view) syntax))
|#
