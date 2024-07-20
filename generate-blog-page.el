;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(require 'org)

(defun parse-date (date)
  "Parse DATE to dd de mm, yyyy."
  (format-time-string "%d de %B, %Y" date))

(defun org-get-date (file)
  "Extract the DATE property from an org mode FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+DATE: \\(.*\\)$" nil t)
	  (date-to-time
	   (substring-no-properties
		(match-string 1))))))

(defun get-blog-posts (posts-folder)
  "Get blog posts stored on POSTS-FOLDER."
  (sort
   (seq-map
	(lambda (post)
	  `(,post ,(org-get-date (concat posts-folder post))))
	(directory-files posts-folder nil ".org"))
   :key (lambda (v) (cdr v))
   ;; most recent posts before
   :reverse t))

(with-temp-file "content/blog.org"
  (let ((posts-folder "./content/posts/"))
	(seq-do
	 (lambda (post)
	   (let ((post-filename (car post))
			 (post-publish-date (nth 1 post)))
		 (insert (format "** [[../blog/%s][%s]]\n\n"
						 (car (string-split post-filename ".org"))
						 (org-get-title (concat posts-folder post-filename))))
		 (insert (format "%s por Maurício Mussatto Scopel\n"
						 (parse-date post-publish-date)))))
	 (get-blog-posts posts-folder))))
