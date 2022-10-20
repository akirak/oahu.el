;;; oahu-memento.el --- Org-Memento integration for oahu -*- lexical-binding: t -*-

(require 'org)
(require 'org-memento)
(require 'oahu)

(defvar oahu-last-view)

;;;###autoload
(defun oahu-memento-save ()
  "Save the last view to the current block entry."
  (interactive)
  (when oahu-last-view
    (org-memento-with-current-block
      ;; Save to separate properties as the structure of `oahu-last-view' may
      ;; change in the future.
      (org-entry-put nil "oahu_process_name" (nth 0 oahu-last-view))
      (org-entry-put nil "oahu_process_argument"
                     (let ((print-level nil)
                           (print-circle nil)
                           (print-length nil))
                       (prin1-to-string (nth 1 oahu-last-view))))
      (org-entry-put nil "oahu_view_name" (nth 2 oahu-last-view)))))

;;;###autoload
(defun oahu-memento-load ()
  "Load a view from the current block entry."
  (interactive)
  (setq oahu-last-view (oahu-memento--view-from-entry)))

(defun oahu-memento--view-from-entry ()
  (when-let* ((alist (org-memento-with-current-block
                       (org-entry-properties nil 'standard)))
              (process (cdr (assoc "OAHU_PROCESS_NAME" alist)))
              (argument (read (cdr (assoc "OAHU_PROCESS_ARGUMENT" alist))))
              (view (cdr (assoc "OAHU_VIEW_NAME" alist))))
    (list process argument view)))

(defun oahu-memento-context ()
  "Return (PROCESS ARGUMENT) of the current entry, if any."
  (when-let* ((alist (org-entry-properties nil 'standard))
              (process (cdr (assoc "OAHU_PROCESS_NAME" alist)))
              (argument (read (cdr (assoc "OAHU_PROCESS_ARGUMENT" alist)))))
    (list process argument)))

(provide 'oahu-memento)
;;; oahu-memento.el ends here
