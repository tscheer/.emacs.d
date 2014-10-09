;; Add ability to look up Clojure community packages in Marmalade
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; list packages to load
(defvar my-packages '(better-defaults
		      clojure-mode
		      clojure-test-mode
		      cider
                      markdown-mode
                      sass-mode
                      evil
                      evil-leader
                      solarized-theme
                      zenburn-theme))

;; install missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;(dolist (p my-packages)
;  (when (not (package-installed-p p))
;    (package-install p)))

;; alway use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; load theme
(load-theme 'solarized-dark t)
