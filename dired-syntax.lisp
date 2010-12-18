;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :info.read-eval-print.climacs.dired)

(define-syntax-command-table dired-table :errorp nil
  :inherit-from '(drei:movement-table climacs-gui:window-table))

;;;;;; presentation
;;(define-presentation-type dired ()
;;  :inherit-from 't)
;;
;;(define-presentation-method presentation-typep (object (type dired))
;;  (pathnamep object))
;;
;;(define-presentation-method present (object (type dired) stream
;;                                            (view textual-view)
;;                                            &key)
;;  (let ((stat (sb-posix:stat (namestring object)))
;;        (name (if (directory-pathname-p object)
;;                  (car (last (pathname-directory object)))
;;                  (file-namestring object))))
;;    (format stream
;;            "  ~o ~a ~a ~6,d ~a ~a"
;;            (sb-posix:stat-mode stat)
;;            (file-author object)
;;            (sb-posix:group-name (sb-posix:getgrgid (sb-posix:stat-gid stat)))
;;            (sb-posix:stat-size stat)
;;            (dt:|yyyy-mm-dd hh:mm| (dt:from-posix-time (sb-posix:stat-mtime stat)))
;;            name)))


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
  ((path :initarg :path))
  (:documentation "A mode for Directory editing.")
  (:applicable-syntaxes fundamental-syntax))


(defmethod syntax-command-tables append ((syntax dired-mode))
  '(dired-table))

(defun ensure-pathname-for-dired (path)
  (make-pathname :directory (namestring path)
                 :name :wild))

;;(defun make-buffer-contents (path)
;;  (loop for x in (directory (ensure-pathname-for-dired path))
;;                               do (defmethod presentation-type-of ((object (eql x)))
;;                                    'dired)
;;                               nconc (list x #\Newline)))

(defun make-buffer-contents (path)
  (str (namestring  path) #\Newline
       (trivial-shell:shell-command #"""ls -al #,(namestring path)""")))

(defun make-dired-buffer (path)
  (let ((buffer (make-new-buffer)))
    (insert-buffer-sequence buffer 0 (make-buffer-contents path))
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
           (enable-mode view 'dired-mode :path path)
           (setf (current-view (current-window)) view)
           (evaluate-attribute-line view)
           (setf (filepath buffer) (pathname path)
                 (read-only-p buffer) t)
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

(defun dired-find-file (path)
  (unless path
    (let* ((dir (slot-value (syntax (current-view)) 'path))
           (mark (point))
           (line (region-to-string (beginning-of-line (clone-mark mark))
                                   (end-of-line (clone-mark mark)))))
      (ppcre:register-groups-bind (d file)
          ("(.)(?:[^ ]+ +){7}(.*\)" line)
        (setf path (merge-pathnames (str file (when (string= "d" d) "/")) dir)))))
  (when path
    (if (directory-pathname-p path)
        (find-directory path)
        (find-file path))))


(define-command (com-dired-find-file :name t :command-table dired-table)
    ((path 'pathname
           :prompt "File: "
           :prompt-mode :raw
           :default-type 'pathname))
  "Find file."
  (dired-find-file path))

(set-key 'com-dired-find-file 'dired-table '((#\Newline)))

(set-key `(drei-commands::com-forward-line ,*numeric-argument-marker*) 'dired-table
         '((#\n)))

(set-key `(drei-commands::com-backward-line ,*numeric-argument-marker*) 'dired-table
         '((#\p)))

(set-key `(climacs-commands::com-kill-view (current-view)) 'dired-table
	 '((#\q)))
