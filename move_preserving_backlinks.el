(defun mpb--resolve-abbrev-root (key)
  (let ((entry (assoc key org-link-abbrev-alist)))
    (when entry (expand-file-name (cdr entry)))))

(defun mpb--abbrev-link (path)
  "Return first matching abbrev-style link for PATH."
  (let ((absolute (expand-file-name path)))
    (cl-loop for (key . root) in org-link-abbrev-alist
             for full-root = (expand-file-name root)
             when (string-prefix-p full-root absolute)
             return (concat key ":" (file-relative-name absolute full-root)))))

(defun mpb--find-files-containing-link (link)
  "Use ripgrep to find all Org files that contain LINK."
  (let ((roots (mapcar (lambda (x) (expand-file-name (cdr x)))
                       org-link-abbrev-alist))
        (results '()))
    (dolist (root roots)
      (let* ((default-directory root)
             (cmd (format "rg -l --no-heading %s" (shell-quote-argument link)))
             (output (shell-command-to-string cmd)))
        (setq results (append results
                              (mapcar (lambda (f) (expand-file-name f root))
                                      (split-string output "\n" t))))))
    results))

(defun mpb--replace-in-file (file replacements)
  "Replace all OLD → NEW links in FILE using REPLACEMENTS ((old . new))."
  (with-current-buffer (find-file-noselect file)
    (let ((modified nil))
      (dolist (rep replacements)
        (goto-char (point-min))
        (while (search-forward (car rep) nil t)
          (replace-match (cdr rep) 'fixedcase 'literal)
          (setq modified t)))
      (when modified (save-buffer))
      (kill-buffer))))

;;;###autoload
(defun move-preserving-backlinks ()
  "Rename marked files in Dired and update Org backlinks using ripgrep."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "Must be run in a Dired buffer"))
  (let* ((marked (dired-get-marked-files))
         (replacements '())
         (move-pairs '()))
    (cond
     ;; One file: ask for new name
     ((= (length marked) 1)
      (let* ((src (car marked))
             (dest (read-file-name (format "Move %s to: " (file-name-nondirectory src))
                                   (dired-dwim-target-directory))))
        (rename-file src dest)
        (push (cons src dest) move-pairs)))
     ;; Multiple files: ask for target directory
     (t
      (let* ((target-dir (read-directory-name "Move files to directory: "
                                              (dired-dwim-target-directory))))
        (dolist (src marked)
          (let ((dest (expand-file-name (file-name-nondirectory src) target-dir)))
            (rename-file src dest)
            (push (cons src dest) move-pairs)))))
    ;; Build replacements
    (setq replacements
          (cl-loop for (old . new) in move-pairs
                   for old-link = (mpb--abbrev-link old)
                   for new-link = (mpb--abbrev-link new)
                   when (and old-link new-link)
                   collect (cons old-link new-link)))
    ;; Update backlinks using ripgrep
    (let ((touched-files (cl-remove-duplicates
                          (apply #'append
                                 (mapcar (lambda (rep)
                                           (mpb--find-files-containing-link (car rep)))
                                         replacements))
                          :test #'equal)))
      (dolist (file touched-files)
        (mpb--replace-in-file file replacements)))
    (message "Moved %d file(s) and updated backlinks in %d files."
             (length move-pairs) (length replacements)))))

(defun mpb--abbrev-link (path)
  "Return the first abbrev-style link (e.g., orgr:foo/bar.png) for PATH."
  (let ((absolute (expand-file-name path)))
    (cl-loop for (key . root) in org-link-abbrev-alist
             for full-root = (expand-file-name root)
             when (string-prefix-p full-root absolute)
             return (concat key ":" (file-relative-name absolute full-root)))))

(defun mpb--ripgrep-backlinks (link)
  "Return an alist of (file . ((line . snippet) ...)) where LINK is found."
  (let ((roots (mapcar (lambda (x) (expand-file-name (cdr x))) org-link-abbrev-alist))
        (results (make-hash-table :test 'equal)))
    (dolist (root roots)
      (let* ((default-directory root)
             (cmd (format "rg --no-heading --color never --line-number --context 2 %s"
                          (shell-quote-argument link)))
             (lines (split-string (shell-command-to-string cmd) "\n"))
             current-file context-lines)
        (dolist (line lines)
          (cond
           ;; Match lines like: somefile.org:42: content
           ((string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
            (let ((file (expand-file-name (match-string 1 line) root))
                  (linenum (string-to-number (match-string 2 line)))
                  (content (match-string 3 line)))
              (push (cons linenum content)
                    (gethash file results))))
           ;; Separator line between blocks (rg prints --)
           ((string= line "--") nil)
           ;; Otherwise: continuation lines (context), we skip
           )))
      )
    ;; Convert hash-table to sorted alist
    (let (alist)
      (maphash (lambda (file matches)
                 (push (cons file (sort matches (lambda (a b) (< (car a) (car b)))))
                       alist))
               results)
      alist)))

(defun mpb-show-backlinks (&optional file)
  "Show all backlinks to FILE in a nicely formatted org buffer using ripgrep.
If FILE is nil, use the current buffer's file."
  (interactive)
  (let* ((file (or file (buffer-file-name)
                   (read-file-name "File to search backlinks for: ")))
         (link (mpb--abbrev-link file))
         (results (mpb--ripgrep-backlinks link))
         (buf (get-buffer-create "*Org Backlinks*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Backlinks to: %s\n\n" link))
        (if (null results)
            (insert "No backlinks found.\n")
          (dolist (file-entry results)
            (let* ((filepath (car file-entry))
                   (matches (cdr file-entry))
                   (lines (mapcar #'car matches))
                   (content (mapcar #'cdr matches)))
              (insert (format "** [[file:%s][%s]]\n"
                              filepath (abbreviate-file-name filepath)))
              (insert (format "   - Matches: %s\n\n"
                              (mapconcat #'number-to-string lines ", ")))
              (insert "#+begin_src org\n")
              (dolist (line content)
                (insert (concat line "\n")))
              (insert "#+end_src\n\n"))))
        (goto-char (point-min))
        (view-mode 1)
        (setq buffer-read-only t)
        (setq-local header-line-format
                    "Backlink viewer: RET to follow links, TAB to fold, q to quit")))
    (pop-to-buffer buf)))
