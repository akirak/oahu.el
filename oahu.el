;;; oahu.el --- Manage Org searches -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.5"))
;; Keywords: matching
;; URL: https://git.sr.ht/~akirak/oahu.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Macros to help define processes

;; Put the same definition in the autoload to allow use of the macro in the
;; user's configuration.
;;;###autoload (defmacro oahu-ignore-first-argument (fn) `(lambda (_ignored &rest args) (apply ',fn args)))
(defmacro oahu-ignore-first-argument (fn)
  `(lambda (_ignored &rest args)
     (apply ',fn args)))

;;;; Custom variables

(define-widget 'oahu-view-name-type 'lazy
  "Name of a view.

A view name can be either a string, which is the literal view
name, or a function that takes the context argument and returns a
string or nil. The returned string is a view name. If the
function returns nil, the view is temporarily unavailable, and it
will be omitted in completion."
  :tag "View name"
  :type '(choice string
                 (function :tag "Function that takes a context\
 and returns a name or nil")))

(define-widget 'oahu-view-body-type 'lazy
  "Implementation of a view.

This is a cons cell of a function and a list of arguments. The
function takes two or more arguments, and the first two arguments
are passed from the process:

 * The context argument returned by the context function, which
   is defined as :context in `oahu-process-alist'.

 * The Org files returned by :files function in `oahu-process-alist'.

The cdr is appended to these arguments, so if the arguments is a
N-ary list, the function must accept N+2 arguments."
  :tag "View"
  :type '(cons function
               (repeat :tag "Rest of the arguments" sexp)))

(defcustom oahu-process-alist nil
  "Alist of process definitions.

Each entry in the alist consists of a process type, which is a
symbol, and a process properties, which is a plist. The plist in
each entry must have the following properties:

 * :context, a function that takes no argument and returns a context argument.

 * :context-selector, a function that interactively selects and
   returns a context argument. It is used in `oahu-view-global'.

 * :files, a function that takes the context argument and returns
   a list of Org files.

 * :views, an alist defining views in the context. Each entry
   consists of a name, which is of `oahu-view-name-type', and a
   view body, which is of `oahu-view-body-type'."
  :type '(alist :key-type (symbol :tag "Type of the process")
                :value-type
                (plist :options
                       (((const :context)
                         (function :tag "Function that takes no argument"))
                        ((const :context-selector)
                         (function :tag "Function that takes no argument"))
                        ((const :files)
                         (function :tag "Function that takes a context and returns Org files"))
                        ((const :views)
                         (alist :tag "List of view definitions"
                                :key-type oahu-view-name-type
                                :value-type oahu-view-body-type))))))

;;;; Variables

(defvar oahu-last-view nil)

(defvar oahu-view-history nil)

;;;; Commands

;;;###autoload
(defun oahu-view (type argument &optional view-name)
  "Display a view."
  (interactive (cond
                ((and (equal current-prefix-arg '(16))
                      oahu-view-history)
                 (oahu-prompt-view-history))
                ((or (equal current-prefix-arg '(4))
                     (not oahu-last-view))
                 (let ((context (oahu-prompt-context "Process: ")))
                   (append context
                           (list (car (apply #'oahu--select-view context))))))
                (oahu-last-view)))
  (let* ((files (oahu-org-files type argument))
         (view (or (oahu--view type argument view-name)
                   (oahu--select-view type argument))))
    (apply (cadr view) argument files (cddr view))
    (cl-pushnew (setq oahu-last-view (list type argument (car view)))
                oahu-view-history
                :test #'equal)))

;;;###autoload
(defun oahu-alternative-view (type argument)
  "Display another view in the same argument without saving it."
  (interactive (or (seq-take oahu-last-view 2)
                   (oahu-prompt-context "Process: ")))
  (let* ((files (oahu-org-files type argument))
         (view (oahu--select-view type argument)))
    (apply (car view) argument files (cdr view))))

;;;###autoload
(defun oahu-view-global ()
  "Display a view selected from global contexts."
  (interactive)
  (let* ((type (intern (completing-read "Process: "
                                        (mapcar #'car oahu-process-alist))))
         (plist (cdr (or (assq type oahu-process-alist)
                         (error "No process named %s" type))))
         (argument (funcall (or (plist-get plist :context-selector)
                                (error "No :context-selector property")))))
    (oahu-view type argument)))

(defun oahu-bookmark-last-view ()
  "Bookmark the last view."
  (interactive)
  (unless oahu-last-view
    (user-error "No last view"))
  (let ((record (oahu--make-bookmark-record oahu-last-view))
        (name (read-from-minibuffer "Name of the bookmark: ")))
    (bookmark-store name record nil)))

;;;; Functions

(defun oahu-prompt-view-history ()
  (let* ((alist (mapcar (lambda (list)
                          (cons (format "%s" list)
                                list))
                        oahu-view-history))
         (string (completing-read "View history: " alist nil t)))
    (cdr (assoc string alist))))

(defun oahu-prompt-context (prompt)
  (let* ((alist (oahu--available-contexts))
         (type (intern (completing-read prompt alist nil t))))
    (list type (cdr (assq type alist)))))

(defun oahu--available-contexts ()
  (thread-last
    oahu-process-alist
    (mapcar (pcase-lambda (`(,type . ,plist))
              (when-let (ctx (oahu--eval-context
                              (plist-get plist :context)))
                (cons type ctx))))
    (delq nil)))

(defun oahu--eval-context (context)
  (pcase context
    ((pred functionp) (funcall context))
    (`nil t)))

(defun oahu-org-files (type argument)
  (thread-first
    (cdr (assq type oahu-process-alist))
    (plist-get :files)
    (funcall argument)))

(defun oahu--view-alist (type argument)
  (cl-flet
      ((mapname (cell)
         (let ((name (car cell)))
           (cons (if (functionp name)
                     (funcall name argument)
                   name)
                 (cdr cell)))))
    (thread-last
      (plist-get (cdr (assq type oahu-process-alist))
                 :views)
      (mapcar #'mapname)
      (seq-filter #'car))))

(defun oahu--view (type argument view-name)
  (assoc view-name (oahu--view-alist type argument)))

(defun oahu--select-view (type argument)
  (let ((view-alist (oahu--view-alist type argument)))
    (assoc (completing-read "View: " view-alist nil t)
           view-alist)))

(defun oahu--make-bookmark-record (view-triple)
  `((handler . oahu-bookmark-handler)
    (view . ,view-triple)))

;;;###autoload
(defun oahu-bookmark-handler (bookmark)
  (cl-assert (eq (alist-get 'handler bookmark)
                 'oahu-bookmark-handler))
  (apply #'oahu-view (alist-get 'view bookmark)))

(provide 'oahu)
;;; oahu.el ends here
