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

(defcustom oahu-process-alist nil
  "Alist of process definitions.

Each entry in the alist consists of a process name, which is a
string, and a process properties, which is a plist. The plist in
each entry must have the following properties:

 * :context, a function that takes no argument and returns a context argument.

 * :files, a function that takes the context argument and returns
   a list of Org files.

 * :views, an alist defining views in the context.

Each view entry in the :views property is a cons cell that
consists of a view name and a plist.

A view name can be either a string, which is the literal view
name, or a function that takes the context argument and returns a
string or nil. The returned string is a view name. If the
function returns nil, the view is temporarily unavailable, so it
will be omitted in completion."
  :type '(alist :key-type (string :tag "Name of the process")
                :value-type
                (plist :options
                       (((const :context)
                         (function :tag "Function that takes no argument"))
                        ((const :files)
                         (function :tag "Function that takes a context and returns Org files"))
                        ((const :views)
                         (alist :key-type
                                (choice (string :tag "Name of the view")
                                        (function :tag "Function that takes a context\
 and returns a name or nil"))
                                :value-type
                                (cons function
                                      (repeat :tag "Rest of the arguments" sexp))))))))

;;;; Variables

(defvar oahu-last-view nil)

(defvar oahu-view-history nil)

;;;; Commands

;;;###autoload
(defun oahu-view (type context view-name)
  "Display a view."
  (interactive (cond
                ((and (equal current-prefix-arg '(16))
                      oahu-view-history)
                 (oahu-prompt-view-history))
                ((or (equal current-prefix-arg '(4))
                     (not oahu-last-view))
                 (let ((context (oahu-prompt-context "Process: ")))
                   (append context
                           (list (completing-read "View: "
                                                  (apply #'oahu--view-alist context)
                                                  nil t)))))
                (oahu-last-view)))
  (let ((files (oahu-org-files type context))
        (view (oahu--view type context view-name)))
    (apply (car view) context files (cdr view))
    (cl-pushnew (setq oahu-last-view (list type context view-name))
                oahu-view-history
                :test #'equal)))

(defun oahu-another-view (type context)
  "Display another view in the same context without saving it."
  (interactive (or (seq-take oahu-last-view 2)
                   (oahu-prompt-context "Process: ")))
  (let ((files (oahu-org-files type context))
        (view (completing-read "View: "
                               (oahu--view-alist type context)
                               nil t)))
    (apply (car view) context files (cdr view))))

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
         (type (completing-read prompt alist nil t)))
    (list type (cdr (assoc type alist)))))

(defun oahu--available-contexts ()
  (thread-last
    oahu-process-alist
    (mapcar (pcase-lambda (`(,name . ,plist))
              (when-let (ctx (oahu--eval-context
                              (plist-get plist :context)))
                (cons name ctx))))
    (delq nil)))

(defun oahu--eval-context (context)
  (pcase context
    ((pred functionp) (funcall context))
    (`nil t)))

(defun oahu-org-files (type value)
  (thread-first
    (cdr (assoc type oahu-process-alist))
    (plist-get :files)
    (funcall value)))

(defun oahu--view-alist (type value)
  (cl-flet
      ((mapname (cell)
         (let ((name (car cell)))
           (cons (if (functionp name)
                     (funcall name value)
                   name)
                 (cdr cell)))))
    (thread-last
      (plist-get (cdr (assoc type oahu-process-alist))
                 :views)
      (mapcar #'mapname)
      (seq-filter #'car))))

(defun oahu--view (type value view-name)
  (cdr (assoc view-name (oahu--view-alist type value))))

(provide 'oahu)
;;; oahu.el ends here
