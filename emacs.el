; Teh One Macro To Rule Them All
(defmacro later (&rest body)
  "Runs after init when packages are loaded"
  (if  (and (eq (length body) 1) (eq (caar body) 'quote))
      `(add-hook 'after-init-hook ,(car body))
    `(add-hook 'after-init-hook (lambda ()
				  ,@body))))

;;,---------------
;;| Manage windows
;;`---------------

(global-set-key (kbd "C-&") 'delete-other-windows)
(global-set-key (kbd "C-é") 'split-window-vertically)
(global-set-key (kbd "C-\"") 'split-window-horizontally)
(global-set-key (kbd "C-à") 'delete-window)
(global-set-key (kbd "C-²") 'delete-window)

(set-keyboard-coding-system 'utf-8)

;;,-----------
;;| Appearance
;;`-----------

;; load monokai theme
(setq custom-safe-themes (quote ("bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" default)))
(later (load-theme 'monokai)
       (global-hl-line-mode t)
       (set-face-background 'hl-line "#171717")
       (set-face-foreground 'highlight nil))

; no menu bar, no toolbar, no scrollbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(add-hook 'prog-mode-hook 'scroll-bar-mode)

;;,----------------
;;| Package / MELPA
;;`----------------

(require 'package)
;(add-to-list 'package-archives
;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;,----------
;;| Undo Tree
;;`----------

(later (require 'undo-tree)
       (global-undo-tree-mode))

;;,----------
;;| Yasnippet
;;`----------

;(later (require 'yasnippet)
;       (yas-global-mode 1))

;;,--------------------------
;;| Company + Jedi for python
;;`--------------------------
(later 'global-company-mode)
(add-hook 'python-mode-hook (lambda ()
			      (jedi:setup)
			      (company-mode nil)))
(later (lambda () (setq jedi:complete-on-dot t)
		 ;(define-key python-mode-map (kbd "C-c f") 'helm-jedi-related-names)
		;(define-key python-mode-map (kbd "C-c d") 'jedi:goto-definition)
	 ))

       ;;,-----------
       ;;| Projectile
       ;;`-----------

(later 'projectile-global-mode)

;;,------------
;;| CSS editing
;;`------------
; rainbow mode sets backgroudn color based on detected color code
(add-hook 'css-mode-hook 'rainbow-mode)

;;,-----
;;| Helm
;;`-----

(later (require 'helm)

       ;; must set before helm-config,  otherwise helm use default
       ;; prefix "C-x c", which is inconvenient because you can
       ;; accidentially pressed "C-x C-c"
       (setq helm-command-prefix-key "C-c h")

       (require 'helm-config)
       (require 'helm-eshell)
       (require 'helm-files)
       (require 'helm-grep)
       (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
       (define-key helm-map (kbd "C-z") 'helm-execute-selection-action)

       (global-set-key (kbd "C-x b") 'helm-mini)

       (require 'helm-projectile)
       (global-set-key (kbd "C-x h") 'helm-projectile)
       (helm-mode t))

;;,--------------------------
;;| Highlight symbol at point
;;`--------------------------

(later (require 'highlight-symbol)

       (highlight-symbol-nav-mode)

       (add-hook 'prog-mode-hook 'highlight-symbol-mode)
       (add-hook 'org-mode-hook 'highlight-symbol-mode)
       (setq highlight-symbol-on-navigation-p t) ; enable highlighting symbol at point automatically
       (setq highlight-symbol-idle-delay 0.3)

       (global-set-key [(control shift mouse-1)]
		       (lambda (event)
			 (interactive "e")
			 (goto-char (posn-point (event-start event)))
			 (highlight-symbol-at-point)))

       (global-set-key (kbd "M-n") 'highlight-symbol-next)
       (global-set-key (kbd "M-p") 'highlight-symbol-prev)
       )

;;,--------------------------
;;| Show matching parentheses
;;`--------------------------
(setq show-paren-delay 0)
(show-paren-mode t)

;;,--------------------
;;| Editing keybindings
;;`--------------------
(global-set-key (kbd "C-z") 'backward-kill-word)

;;,---------------------
;;| Emacs condig editing
;;`---------------------
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (rebox-mode t)
				  (paredit-mode)))

;;,----------------------
;;| Company (autocomple everything)
;;`----------------------
(later (setq company-idle-delay 0.1))


;;,---------
;;| Web mode
;;`---------
(later (require 'web-mode)
       (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
;

;;,-----------------------------------
;;| Jinja 2 mode with custom functions
;;`-----------------------------------
(later (add-to-list 'auto-mode-alist '("\\/nylog\\/templates\\/.*\\.html" . jinja2-mode)))

;;,--------------------
;;| Covenience changes
;;`--------------------
; Answer "yes or no" questions with just y/n
(fset 'yes-or-no-p 'y-or-n-p)
; M-g to goto line
(global-set-key [(meta g)] 'goto-line)
; Start in scratch buffer, in org mode and with no message
(setq inhibit-startup-screen t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;;,-------------
;;| Backup files
;;`-------------
; Don't want those poluting my work directories
(setq backup-directory-alist
      '(("." . "~/.emacs-backup-files/")))

;;,------------------------
;;| Emmet Mode (Zen coding)
;;`------------------------
; Activates in sgml and css modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'php-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(later (setq emmet-preview-default nil))

;;,----------------
;;| Electric Indent
;;`----------------
(add-hook 'prog-mode 'electric-indent-mode)

;;,--------------------
;;| Indent whole buffer
;;`--------------------
(defun iwb ()
  "indent whole buffer"
  (interactive)
  ;(delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;;,--------
;;| Clojure
;;`--------
(later (add-hook 'clojure-mode-hook 'paredit-mode))

;;,-----------------
;;| Multiple cursors
;;`-----------------
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this) ; <=> C-d de
					; sublime text
