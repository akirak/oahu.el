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
  (setq oahu-last-view (org-memento-with-current-block
                         (oahu-memento--view-from-entry))))

(defun oahu-memento--view-from-entry ()
  (when-let* ((alist (org-entry-properties nil 'standard))
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

(provide 'oahu-memento)
;;; oahu-memento.el ends here
