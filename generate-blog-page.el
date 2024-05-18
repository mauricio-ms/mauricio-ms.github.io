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
(require 'vc-git)

(defun get-commit-date (filepath)
  (string-trim-right
   (with-output-to-string
	 (with-current-buffer standard-output
	   (vc-git-command t nil nil "log" "--max-count=1" "--date=short" "--format=%cd" filepath)))))

(defun parse-date (date)
  "Parse DATE to dd de mm, yyyy."
  (format-time-string "%d de %B, %Y" date))

(with-temp-file "content/blog.org"
  (let ((posts-folder "./content/posts/"))
	(seq-do
	 (lambda (post)
	   (insert (format "[[../%s][%s]]\n\n"
					   (car (string-split post ".org"))
					   (org-get-title (concat posts-folder post))))
	   (insert (format "%s por Maurício Mussatto Scopel\n"
					   (parse-date
						(date-to-time
						 (get-commit-date (concat posts-folder post)))))))
	 (directory-files posts-folder nil ".org"))))
