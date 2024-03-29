;;; oahu-memento.el --- Org-Memento integration for oahu -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (oahu "0.1") (org-memento "0.1"))
;; Keywords:
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

(require 'org)
(require 'org-memento)
(require 'oahu)

(defvar oahu-last-view)

(defgroup oahu-memento nil
  "Org-Memento integration for oahu."
  :group 'oahu
  :group 'org-memento)

(defcustom oahu-memento-view-derive-fn #'ignore
  "Function that derives the view name of an entry.

This function takes three or more arguments. The following
arguments are mandatory:

 1. A process type.

 2. A process argument.

 3. A list of view names available in the process.

The rest of the arguments is a plist. The argument plist contains
the following properties:

 * :title, the headline of the entry in string.

 * :tags, a list of tags of the entry.

 * :properties, which is an alist of properties of the entry.
   Keys are strings."
  :type 'function)

;;;###autoload
(defun oahu-memento-save (&optional heading)
  "Save the last view to the current block entry.

If HEADING is specified, the entry with the heading will be the
target instead of the current block.

With a single universal argument, the user can choose a heading
interactively."
  (interactive (list (when (equal current-prefix-arg '(4))
                       (org-memento-read-title "Select a block: "
                         :date (format-time-string "%F")
                         :select-existing-heading t))))
  (if oahu-last-view
      (org-memento-with-block-title (if (stringp heading)
                                        heading
                                      org-memento-current-block)
        ;; Save to separate properties as the structure of `oahu-last-view' may
        ;; change in the future.
        (apply #'oahu-memento--save-to-entry oahu-last-view))
    (message "No last view")))

;;;###autoload
(defun oahu-memento-set-entry-at-point ()
  "Set the view of a journal entry at point."
  (interactive)
  (unless (and (derived-mode-p 'org-mode)
               (file-equal-p org-memento-file
                             (buffer-file-name (org-base-buffer (current-buffer))))
               (not (org-before-first-heading-p)))
    (user-error "You must run this command inside the buffer of org-memento-file"))
  (apply #'oahu-memento--save-to-entry
         (oahu-read-context-globally))
  (when (org-memento-current-block-p)
    (org-memento-rerun-block-hook)))

(defun oahu-memento--save-to-entry (type argument &optional view-name)
  (org-entry-put nil "OAHU_PROCESS_NAME" (symbol-name type))
  (org-entry-put nil "OAHU_PROCESS_ARGUMENT" (oahu-memento--prin1-to-string argument))
  (when view-name
    (org-entry-put nil "OAHU_VIEW_NAME" view-name)))

;;;###autoload
(defun oahu-memento-load ()
  "Load a view from the current block entry."
  (interactive)
  (setq oahu-last-view (org-memento-with-current-block
                         (oahu-memento--view-from-entry))))

(defun oahu-memento--view-from-entry ()
  (when-let* ((alist (org-entry-properties nil 'standard))
              (process-name (cdr (assoc "OAHU_PROCESS_NAME" alist)))
              (process (when process-name
                         (intern process-name)))
              (argument (read (cdr (assoc "OAHU_PROCESS_ARGUMENT" alist))))
              (view-names (mapcar #'car (oahu--view-alist process argument))))
    (list process argument (or (cdr (assoc "OAHU_VIEW_NAME" alist))
                               (apply oahu-memento-view-derive-fn
                                      process argument
                                      view-names
                                      :properties alist
                                      (save-excursion
                                        (org-back-to-heading)
                                        (list :title (org-get-heading)
                                              :tags (org-get-tags (point) 'local))))))))

(defun oahu-memento--prin1-to-string (sexp)
  (let ((print-level nil)
        (print-circle nil)
        (print-length nil))
    (prin1-to-string sexp)))

(defun oahu-memento-context ()
  "Return (PROCESS ARGUMENT) of the current entry, if any."
  (when-let* ((alist (org-entry-properties nil 'standard))
              (process (cdr (assoc "OAHU_PROCESS_NAME" alist)))
              (argument (read (cdr (assoc "OAHU_PROCESS_ARGUMENT" alist)))))
    (list (and process (intern process)) argument)))

;;;###autoload
(defun oahu-memento-template-arguments (context)
  "Return a plist of template arguments denoting CONTEXT.

This function should be used as :template property of a
corresponding group level in `org-memento-group-taxonomy'."
  (pcase-exhaustive context
    (`(,type ,argument)
     `(:properties
       (("OAHU_PROCESS_NAME" . ,(symbol-name type))
        ("OAHU_PROCESS_ARGUMENT" . ,(oahu-memento--prin1-to-string argument)))))
    (`nil
     nil)))

;;;###autoload
(defun oahu-memento-rerun-view (marker)
  "Dispatch the view for the journal entry."
  (interactive (list (cl-case (derived-mode-p 'org-mode
                                              'org-agenda-mode
                                              'org-memento-timeline-mode)
                       (org-mode
                        (point-marker))
                       (org-agenda-mode
                        (or (get-char-property (point) 'org-marker)
                            (get-char-property (point) 'org-hd-marker)
                            (user-error "No marker at point")))
                       (org-memento-timeline-mode
                        (if-let* ((section (magit-current-section))
                                  (value (oref section value))
                                  (marker (nth 4 value)))
                            marker
                          (user-error "No Org entry at point"))))))
  (unless (markerp marker)
    (user-error "Not a marker"))
  (when-let (view (save-current-buffer
                    (org-with-point-at marker
                      (oahu-memento--view-from-entry))))
    (apply #'oahu-view view)))

;;;###autoload
(defun oahu-memento-group-view (block-marker-or-group)
  "Dispatch a view for the group at point."
  (interactive (list (or (oahu-memento--block-marker-at-point)
                         (org-memento-read-group "Dispatch a view for a group: "
                           :group-path (org-memento-timeline-group-at-point)))))
  (when-let (view (or (and (markerp block-marker-or-group)
                           (org-with-point-at block-marker-or-group
                             (oahu-memento--view-from-entry)))
                      (org-memento-with-group-entry block-marker-or-group
                        (oahu-memento--view-from-entry))))
    (apply #'oahu-view view)))

(defun oahu-memento-group-view-at-point ()
  "Return the view at point in the timeline."
  (when-let (marker (oahu-memento--block-marker-at-point))
    (org-with-point-at marker
      (oahu-memento--view-from-entry))))

(defun oahu-memento--block-marker-at-point ()
  (when-let (section (magit-current-section))
    (when (eq (oref section type) 'block)
      (nth 3 (oref section value)))))

;;;###autoload
(defun oahu-memento-export (directory)
  "Export views to a directory."
  (interactive "DExport to directory: ")
  (let ((start (float-time))
        (i 0)
        logs)
    (unwind-protect
        (dolist (view (oahu-memento--exported-views))
          (let ((buffer (apply #'oahu-alternative-view view)))
            (if-let (filename (with-current-buffer (cl-etypecase buffer
                                                     (string (get-buffer buffer))
                                                     (buffer buffer))
                                (run-hook-with-args-until-success
                                 'oahu-view-export-hook
                                 directory
                                 (format "_%s_%d"
                                         (format-time-string "%s" start)
                                         (cl-incf i)))))
                (push `((view . ,(apply #'vector (mapcar #'prin1-to-string view)))
                        (filename . ,(file-relative-name filename directory))
                        (datetime . ,(format-time-string "%Y-%m-%dT%H:%M:%S%:z")))
                      logs)
              (error "None of oahu-view-export-hook return non-nil for %s" view))))
      (with-temp-buffer
        (dolist (log (reverse logs))
          (json-insert log)
          (unless (bolp)
            (insert "\n")))
        (append-to-file (point-min) (point-max)
                        (expand-file-name "export.ndjson" directory))))
    (message "Exported %d views in %.1f sec" (length logs) (- (float-time) start))))

(defun oahu-memento--exported-views ()
  (let (views)
    (dolist (group (oahu-memento--zone-groups))
      (when-let (view (org-memento-with-group-entry group
                        (oahu-memento--view-from-entry)))
        (cl-pushnew view views)))
    (dolist (block (org-memento--blocks))
      (when-let (view (org-with-point-at (org-memento-block-hd-marker block)
                        (oahu-memento--view-from-entry)))
        (cl-pushnew view views)))
    views))

(defun oahu-memento--zone-groups ()
  (when org-memento-zone-taxy
    (let (result)
      (cl-labels
          ((go (taxy)
             (dolist (group (plist-get (cdr (taxy-name taxy)) :groups))
               (cl-pushnew group result))
             (dolist (subtaxy (taxy-taxys taxy))
               (go subtaxy))))
        (dolist (taxy (taxy-taxys org-memento-zone-taxy))
          (go taxy)))
      result)))

(provide 'oahu-memento)
;;; oahu-memento.el ends here
