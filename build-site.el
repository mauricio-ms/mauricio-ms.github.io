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
(use-package htmlize :ensure t)

;; Load the publishing system
(require 'ox-publish)

(require 'cl-lib)
(require 'vc-git)

(use-package esxml
  :pin "melpa-stable"
  :ensure t)

;; OpenGraph variables
(defvar og-type nil
  "Variable to control the OpenGraph page type.")
(defvar og-url nil
  "Variable to control the OpenGraph url page.")

(setq org-publish-use-timestamps-flag t
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      org-html-prefer-user-labels t
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)

(set-language-environment "Brazilian Portuguese")

(defvar dw/site-url (if (string-equal (getenv "CI") "true")
                        "https://vidaem8bits.com"
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
                                (a (@ (class "nav-link") (href "/blog/")) "Blog") " "
								(a (@ (class "nav-link") (target "_blank") (href "https://github.com/mauricio-ms")) "GitHub") " "))))))

(defun dw/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (p "Fique a vontade para me enviar um email: ms -dot- mauricio93 -at- gmail -dot- com")))))

(defun og-tags (title og-description)
  (list `(meta (@ (property "og:locale")
				  (content "pt_BR")))
		`(meta (@ (property "og:site_name")
				  (content "Vida em 8 Bits")))
		`(meta (@ (property "og:title")
				  (content ,title)))
		`(meta (@ (property "og:type")
				  (content ,og-type)))
		`(meta (@ (property "og:image")
				  (content ,(concat dw/site-url "/img/vida-em-8-bits-banner.png"))))
		`(meta (@ (property "og:image:secure_url")
				  (content ,(concat dw/site-url "/img/vida-em-8-bits-banner.png"))))
		`(meta (@ (property "og:image:width")
				  (content "1272")))
		`(meta (@ (property "og:image:height")
				  (content "664")))
		`(meta (@ (property "og:url")
				  (content ,og-url)))
		(if (equal og-type "article")
			`(meta (@ (property "article:section")
					  (content "blog"))))))

(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dw/org-html-template)))

(cl-defun dw/generate-page (title content info &key (og-description) (publish-date))
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
			,@(og-tags title og-description)
			(link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/iosevka-aile/iosevka-aile.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/fonts/jetbrains-mono/jetbrains-mono.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/code.css"))))
            (link (@ (rel "stylesheet") (href ,(concat dw/site-url "/css/site.css"))))
			(script (@ (async "")
                       (data-id "101454377")
                       (src "//static.getclicky.com/js"))
                    ;; Empty string to cause a closing </script> tag
                    "")
            (title ,(blog-title title)))
           (body ,@(dw/site-header)
                 (div (@ (class "container"))
                      (div (@ (class "site-post"))
                           (h1 (@ (class "site-post-title")) ,title)
                           ,(when publish-date
                              `(p (@ (class "site-post-meta")) ,publish-date))
                           (div (@ (id "content")) ,content)))
                     ,@(dw/site-footer))))))

(defun blog-title (title)
  "Generate blog title tag from TITLE."
  (if (string-empty-p title)
	  "Vida em 8 Bits"
	(concat title " - Vida em 8 Bits")))

(defun dw/org-html-template (contents info)
  (dw/generate-page
   (substring-no-properties
	(org-export-data (plist-get info :title) info))
   contents
   info
   :og-description nil ;; use (org-property "description") when needed
   :publish-date (org-export-data (org-export-get-date info "%e de %B, %Y") info)))

(defun org-property (key)
  "Get org property KEY."
  (car (cdr (car (org-collect-keywords '(key))))))

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

(defun org-html-publish-to-html (plist filepath pub-dir)
  "Publish an org file to HTML, using the FILEPATH as the output directory."
  (let ((article-path (get-article-output-path filepath pub-dir))
		(filename (file-name-nondirectory filepath)))

	(setq og-type (if (string-match-p "/posts/" filepath) "article" "website"))
	(setq og-url (if (string= filename "index.org")
					 "https://vidaem8bits.com"
				   (concat "https://vidaem8bits.com/" (car (split-string filename ".org")))))
	
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= filename "404.org") "404" "index")
                         extension))))
	  (org-publish-org-to 'site-html
                          filepath
                          (concat "." (or (plist-get plist :html-extension) "html"))
                          plist
                          article-path))))

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
