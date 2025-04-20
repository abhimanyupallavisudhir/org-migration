(defun org--resolve-abbrev-path (abbrev-key)
  "Return the expanded path from `org-link-abbrev-alist` for ABBREV-KEY."
  (let ((entry (assoc abbrev-key org-link-abbrev-alist)))
    (when entry (expand-file-name (cdr entry)))))

(defun make-org-links-relative-to-root (directory abbrev-key)
  "Convert file-relative links in all Org files under DIRECTORY
to links relative to ABBREV-KEY (as defined in `org-link-abbrev-alist`)."
  (interactive
   (list
    (read-directory-name "Org root directory: ")
    (completing-read "Choose abbreviation: " (mapcar #'car org-link-abbrev-alist) nil t)))
  (let ((root (org--resolve-abbrev-path abbrev-key)))
    (unless root (error "No such abbrev key in `org-link-abbrev-alist`"))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (let ((modified nil)
              (replacements '()))
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (let* ((type (org-element-property :type link))
                     (path (org-element-property :path link))
                     (raw-link (org-element-property :raw-link link))
                     (begin (org-element-property :begin link))
                     (end (org-element-property :end link)))
                (when (and (string= type "file")
                           (not (string-match-p (concat "^" abbrev-key ":") raw-link))
                           (not (file-name-absolute-p path)))
                  (let* ((absolute (expand-file-name path (file-name-directory file)))
                         (relative-to-root (file-relative-name absolute root))
                         (new-link (format "[[%s:%s]]" abbrev-key relative-to-root)))
                    (push (list begin end new-link) replacements))))))
          ;; Apply edits in reverse order
          (dolist (rep (sort replacements (lambda (a b) (> (car a) (car b)))))
            (goto-char (nth 0 rep))
            (delete-region (nth 0 rep) (nth 1 rep))
            (insert (nth 2 rep))
            (setq modified t))
          (when modified (save-buffer)))
        (kill-buffer)))))

(defun make-org-links-relative-to-file (directory abbrev-key)
  "Convert links like ABBREV-KEY:... in Org files under DIRECTORY
to relative file: links based on the current file's location."
  (interactive
   (list
    (read-directory-name "Org root directory: ")
    (completing-read "Choose abbreviation: " (mapcar #'car org-link-abbrev-alist) nil t)))
  (let ((root (org--resolve-abbrev-path abbrev-key)))
    (unless root (error "No such abbrev key in `org-link-abbrev-alist`"))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (let ((modified nil)
              (replacements '()))
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (let* ((type (org-element-property :type link))
                     (path (org-element-property :path link))
                     (raw-link (org-element-property :raw-link link))
                     (begin (org-element-property :begin link))
                     (end (org-element-property :end link)))
                (when (and (string= type abbrev-key))
                  (let* ((absolute (expand-file-name path root))
                         (relative-to-file (file-relative-name absolute (file-name-directory file)))
                         (new-link (format "[[file:%s]]" relative-to-file)))
                    (push (list begin end new-link) replacements))))))
          ;; Apply edits in reverse order
          (dolist (rep (sort replacements (lambda (a b) (> (car a) (car b)))))
            (goto-char (nth 0 rep))
            (delete-region (nth 0 rep) (nth 1 rep))
            (insert (nth 2 rep))
            (setq modified t))
          (when modified (save-buffer)))
        (kill-buffer)))))
