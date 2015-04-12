;; Add ability to look up community packages in
;; melpa and marmalade
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

;; list packages to load
(defvar my-packages '(better-defaults
		      clojure-mode
		      cider
		      markdown-mode
                      sass-mode
                      elixir-mode
                      erlang
		      evil
		      evil-leader
                      helm
                      org
		      solarized-theme
		      zenburn-theme))

;; install missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; load theme
(load-theme 'zenburn t)

;; enable IDO mode (Interactively Do Things)
(ido-mode 1)
(setq ido-enable-flex-matching t)

;; disable the splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; enable auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; enable evil mode (VIM emulation mode)
;; this command needs to be last in the init.el file
(require 'evil)
(evil-mode 1)

;; evil key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
