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

;; Install dependencies
(package-install 'htmlize)

;; Load the publishing system
(require 'ox-publish)

(require 'cl-lib)
(require 'vc-git)

(use-package esxml
  :pin "melpa-stable"
  :ensure t)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(defvar dw/site-url (if (string-equal (getenv "CI") "true")
                        "https://mauricio-ms.github.io" ;;"https://vidaem8bits.com"
                      "http://localhost:8080")
  "The URL for the site being generated.")

(defun dw/site-header ()
  (list `(header (@ (class "site-header"))
                 (div (@ (class "container"))
                      (div (@ (class "site-title"))
                           (img (@ (class "logo")
                                   (src ,(concat dw/site-url "/img/vida-em-8-bits.png"))
                                   (alt "Vida em 8 bits")))))
                 (div (@ (class "site-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (a (@ (class "nav-link") (href "/")) "Home") " "
                                (a (@ (class "nav-link") (href "/blog/")) "Blog") " "))))))

(defun dw/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (p "Fique a vontade para me enviar um email: ms -dot- mauricio93 -at- gmail -dot- com")))))

(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dw/org-html-template)))

(cl-defun dw/generate-page (title
                            content
                            info
                            &key
                            (publish-date)
                            (head-extra)
                            (pre-content)
                            (exclude-header)
                            (exclude-footer))
  (concat
   "<!-- Generated from " (dw/get-commit-hash)  " on " (format-time-string "%Y-%m-%d @ %H:%M") " with " org-export-creator-string " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "Vida em 8 Bits - Maurício Mussatto Scopel")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            (link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/iosevka-aile/iosevka-aile.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/jetbrains-mono/jetbrains-mono.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/code.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/site.css"))))
            ,(when head-extra head-extra)
            (title ,(concat title " - Vida em 8 Bits")))
           (body ,@(unless exclude-header
                     (dw/site-header))
                 (div (@ (class "container"))
                      (div (@ (class "site-post"))
                           (h1 (@ (class "site-post-title"))
                               ,title)
                           ,(when publish-date
                              `(p (@ (class "site-post-meta")) ,publish-date))
                           ,(when pre-content pre-content)
                           (div (@ (id "content"))
                                ,content)))
                 ,@(unless exclude-footer
                     (dw/site-footer)))))))

(defun dw/org-html-template (contents info)
  (dw/generate-page (org-export-data (plist-get info :title) info)
                    contents
                    info
                    :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))
          article-dir))))

(defun dw/get-commit-hash ()
  "Get the short hash of the latest commit in the current repository."
  (string-trim-right
   (with-output-to-string
     (with-current-buffer standard-output
       (vc-git-command t nil nil "rev-parse" "--short" "HEAD")))))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(defun parse-date (date)
  "Parse DATE to dd de mm, yyyy."
  (format-time-string "%d de %B, %Y" date))

(defun get-branch ()
  (string-trim-right
   (with-output-to-string
	 (with-current-buffer standard-output
	   (vc-git-command t nil nil "branch")))))

(defun get-commit-date (filepath)
  (string-trim-right
   (with-output-to-string
	 (with-current-buffer standard-output
	   (vc-git-command t nil nil "log" "master" "--max-count=1" "--date=short" "--format=%cd" filepath)))))

(with-temp-file "content/blog.org"
  (let ((posts-folder "./content/posts/"))
	(seq-do
	 (lambda (post)
	   (insert (format "[[../%s][%s]]\n\n"
					   (car (org-split-string post ".org"))
					   "(org-get-title (concat posts-folder post))"
					   ))
	   (insert (format "%s %s por Maurício Mussatto Scopel\n"
					   ;;(parse-date
					   ;;(date-to-time
					   (get-branch)
					   (get-commit-date (concat posts-folder post))
					   ;;(get-commit-date "./content/posts/criando-um-blog-no-emacs.org")
						 ;;))
					   )))
	 (directory-files posts-folder nil ".org"))))

;; Define the publishing project
(setq org-publish-project-alist
      (list '("blog:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
			'("blog:posts"
              :base-directory "./content/posts"
              :base-extension "org"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
			'("blog:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./public"
              :recursive t
              :publishing-function org-publish-attachment)))

;; Generate the site output
(org-publish-all t)

(message "Build complete!")
