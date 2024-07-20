#!/bin/sh
rm -rf public/
emacs -Q --script generate-blog-page.el
emacs -Q --script build-site.el
