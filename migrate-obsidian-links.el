;;; migrate-obsidian-links.el --- Convert file links in Org from Obsidian-style

;;; Commentary:
;; Usage:
;;   M-x load-file RET /path/to/migrate-obsidian-links.el RET
;;   M-x migrate-all-file-links-decode-and-md-to-org
;;
;; This will recursively process all .org files in a given directory, decoding
;; URL-encoded characters (e.g. %20 → space) and replacing .md links with .org.

;;; Code:

(defun migrate-all-file-links-decode-and-md-to-org (directory)
  "Convert all file: links in Org files under DIRECTORY by:
- Decoding URL-encoded characters (e.g., %20 to space)
- Replacing .md extensions with .org

Handles both described and raw links like [[file:...]]"
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively directory "\\.org$"))
    (with-current-buffer (find-file-noselect file)
      (let ((modified nil)
            (replacements '()))
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (let* ((type (org-element-property :type link))
                   (path (org-element-property :path link))
                   (begin (org-element-property :begin link))
                   (end (org-element-property :end link)))
              (when (string= type "file")
                (let* ((decoded-path (url-unhex-string path))
                       (final-path (if (string-match "\\.md$" decoded-path)
                                       (replace-regexp-in-string "\\.md$" ".org" decoded-path)
                                     decoded-path))
                       (full-link-text (buffer-substring-no-properties begin end))
                       (new-link-text
                        (replace-regexp-in-string
                         (regexp-quote path)
                         final-path
                         full-link-text
                         'fixedcase 'literal)))
                  (push (list begin end new-link-text) replacements))))))
        ;; Apply replacements in reverse order
        (dolist (rep (sort replacements (lambda (a b) (> (car a) (car b)))))
          (let ((begin (nth 0 rep))
                (end (nth 1 rep))
                (new-text (nth 2 rep)))
            (goto-char begin)
            (delete-region begin end)
            (insert new-text)
            (setq modified t)))
        (when modified (save-buffer)))
      (kill-buffer))))

(provide 'migrate-obsidian-links)

;;; migrate-obsidian-links.el ends here
