;; Run GC every ~40Mo allocated
(setq gc-cons-threshold 40000000) ; 800000 by default

(set-keyboard-coding-system 'utf-8)

;;,------------------------
;;| Initial windows config
;;`------------------------
(defun show-week-agenda-and-scratch ()
  (split-window-horizontally)
  (other-window 1)
  (org-agenda-list)
  (other-window -1))
(add-hook 'after-init-hook #'show-week-agenda-and-scratch)

;; Start in scratch buffer, in org mode and with no message
(setq inhibit-startup-screen t
      initial-major-mode 'org-mode
      initial-scratch-message nil)

;;,-------------------
;;| Customizable parts
;;`-------------------
(defgroup perso nil
  "Configurations for this init file to be used on different computers."
  :group 'emacs)

(defcustom tasks-file "~/org/tasks.org"
  "Full path to this computer's task file"
  :type 'file
  :group 'perso)

(defcustom work-file "~/org/work.org"
  "Full path to work task file"
  :type 'file
  :group 'perso)

(defcustom notes-file "~/org/notes.org"
  "Full path to this computer's notes file"
  :type 'file
  :group 'perso)

;;,----------
;;| Clipboard
;;`----------
(setq save-interprogram-paste-before-kill t)

;;,----------------------------------
;;| Manage windows (azerty keyboard)
;;`----------------------------------

(global-set-key (kbd "C-&") 'delete-other-windows)
(global-set-key (kbd "C-é") 'split-window-vertically)
(global-set-key (kbd "C-\"") 'split-window-horizontally)
(global-set-key (kbd "C-à") 'delete-window)
(global-set-key (kbd "C-²") 'delete-window)

;;,----------------
;;| Package / MELPA
;;`----------------

(require 'package)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;,-----------
;;| Appearance
;;`-----------

;; load monokai theme
(require 'monokai-theme)

; no menu bar, no toolbar, no scrollbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(setq tab-always-indent t)

;;,------------
;;| use-package
;;`------------
(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'use-package)

;;,----------
;;| Undo Tree
;;`----------

(use-package undo-tree
  :config
  (define-key undo-tree-map (kbd "C-?") nil)
  (global-undo-tree-mode))

;;,--------------------------
;;| Company + Jedi for python
;;`--------------------------
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :config
  (add-hook 'python-mode-hook #'ft/python-hook)
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'eldoc-mode))

(defun ft/python-hook ()
  (interactive)
  (require 'anaconda-mode)
  (define-key anaconda-mode-map (kbd "C-*") #'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map (kbd "C-?") #'ft/anaconda-show-doc)
  (anaconda-mode 1)
  (eldoc-mode 1)
  (require 'pytest)
  (local-set-key (kbd "C-c C-a") #'pytest-all)
  (local-set-key (kbd "C-c C-m") #'pytest-module)
  (define-key python-mode-map (kbd "C-x C-e") #'python-shell-send-defun)
  (when (boundp 'project-venv-name)
    (venv-workon project-venv-name)
    (call-interactively 'run-python)))

(defun ft/anaconda-show-doc ()
  (interactive)
  (anaconda-mode-call "goto_definitions" #'ft-anaconda-show-doc-callback))

(defun ft-anaconda-show-doc-callback (result)
  (if result
      (progn
        (anaconda-mode-documentation-view result)
        (other-window -1))
    (message "No doc found")))

;;,-----------
;;| Projectile
;;`-----------
(use-package projectile
  :init
  (setq projectile-enable-caching t
        projectile-git-command "cat <(git ls-files -zco --exclude-standard) <(git --no-pager submodule --quiet foreach 'git ls-files --full-name -co --exclude-standard | sed s!^!$path/!')"
        projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  :config
  (projectile-global-mode))

;;,--------------------
;;| The silver searcher
;;`--------------------
;; Download windows executable here http://blog.kowalczyk.info/software/the-silver-searcher-for-windows.html
(use-package ag
  :init
  (setq ag-highlight-search t))

;;,------------
;;| CSS editing
;;`------------
;; rainbow mode sets backgroudn color based on detected color code

(add-hook 'css-mode-hook 'rainbow-mode)

;;,----
;;| IDO
;;`----
(use-package ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer nil
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (require 'flx-ido)
  :config
  ;; smarter fuzzy matching for ido
  (flx-ido-mode +1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil)

  (ido-vertical-mode t)

  (ido-mode +1)
  (ido-everywhere)
  (ido-ubiquitous-mode +1))

;;,--------------------------
;;| Highlight symbol at point
;;`--------------------------

(use-package highlight-symbol
  :init
  (setq highlight-symbol-on-navigation-p t ; enable highlighting symbol at point automatically
        highlight-symbol-idle-delay 0.3)
  :config
  (highlight-symbol-nav-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'org-mode-hook 'highlight-symbol-mode)
  (defun highligh-at-click-point (event)
    (interactive "e")
    (goto-char (posn-point (event-start event)))
    (highlight-symbol-at-point))

  (global-set-key [(control shift mouse-1)] #'highligh-at-click-point)

  (global-set-key (kbd "M-n") 'highlight-symbol-next)
  (global-set-key (kbd "M-p") 'highlight-symbol-prev))

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
				  (eldoc-mode 1)
				  (paredit-mode)))

;;,-------
;;| Ispell
;;`-------
(use-package ispell
             :init
             (setq ispell-look-command "/usr/bin/look"))

;;,----------------------
;;| Company (autocomple everything)
;;`----------------------
(require 'company)
(require 'company-dabbrev)
(require 'company-capf)

(setq company-idle-delay 0.1
      company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-backends '((company-anaconda) ;; todo: move this to python hook
                         (company-capf company-dabbrev-code company-keywords)
                         company-files ;;company-dabbrev
                         ))

(define-key company-active-map [return] 'newline) ;; I don't want
;; company to get in my way
(define-key company-active-map [tab] 'company-complete-selection) ;; we actually need something smarter like tab once to complete common part, then tap another timee to complete selction
(global-company-mode)

(company-quickhelp-mode 1)

;;,-----------
;;| Yasnippets
;;`-----------
(use-package yasnippet
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt yas-completing-prompt)
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  :config

  ;; When the company popup is on I want tab to complete the selection
  ;; not go to the next field
  (defadvice yas-next-field-or-maybe-expand (around yas-after-company-advice activate compile)
    (if (null company-point)
        ad-do-it
      (call-interactively 'company-complete-selection)))

  (add-hook 'after-init-hook #'yas-global-mode)
  ;; tab is for indent, I use yas-insert-snippet to select a snippet and expand it
  (define-key yas-minor-mode-map [tab] nil))

;;,---------
;;| Web mode
;;`---------
(use-package web-mode
  :mode "\\.html?\\'")

;;,---------
;;| JS2 mode
;;`---------
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq js2-strict-trailing-comma-warning nil)
  :config
  (defun js2-hook ()
    (tern-mode t))
  (add-hook 'js2-mode-hook #'js2-hook)
  (add-to-list 'load-path "~/source/tern/emacs")
  (require 'tern)
  (define-key tern-mode-keymap (kbd "C-?") #'tern-get-docs)
  (define-key tern-mode-keymap (kbd "C-*") #'tern-find-definition))

;;,-----------------------------------
;;| Jinja 2 mode with custom functions
;;`-----------------------------------
(add-to-list 'auto-mode-alist '("\\/nylog\\/templates\\/.*\\.html" . jinja2-mode))

;;,--------------------
;;| Covenience changes
;;`--------------------
; Answer "yes or no" questions with just y/n
(fset 'yes-or-no-p 'y-or-n-p)
; M-g to goto line
(global-set-key [(meta g)] 'goto-line)

;;,-------------
;;| Backup files
;;`-------------
; Don't want those poluting my work directories
(setq backup-directory-alist
      '(("." . "~/.emacs-backup-files/")))

;;,------------------------
;;| Emmet Mode (Zen coding)
;;`------------------------
(require 'emmet-mode)
;; Activates in sgml and css modes
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(setq emmet-preview-default nil)

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
;;| Paredit
;;`--------
(require 'paredit)
;; Swap word movement and barfing/slurping commands bindings
;; Paredit is the only mode using Meta for movement so it
;; breaks my automatisms
;; Now beehave correctly paredit !
;; I also swap barf/slurp because it makes much more
;; sense - visually- to me that way
(define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-<left>") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-<right>") nil)
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)
(define-key paredit-mode-map (kbd "M-é") 'paredit-split-sexp)
(define-key paredit-mode-map (kbd "M-S-<up>") 'paredit-raise-sexp)
(define-key paredit-mode-map (kbd "M-d") nil) ; this key is for multiple-cursors


;;,--------
;;| Clojure
;;`--------
(add-hook 'clojure-mode-hook (lambda ()
			       (paredit-mode)
			       (eldoc-mode 1)))

;;,-----------------
;;| Multiple cursors
;;`-----------------
(require 'multiple-cursors)
(global-set-key (kbd "M-d") 'mc/mark-next-like-this) ; <=> C-d de
					; sublime text
(global-set-key (kbd "M-D") 'mc/skip-to-next-like-this)
(defun mark-next-like-symbol-under-cursor ()
  "Combine this with highlight-symbol, put a cursor at the next symbol equal to the one the cursor is on. At the same position inside the symbol."
  (interactive)
  (mc/create-fake-cursor-at-point)
  (highlight-symbol-next)
  (mc/maybe-multiple-cursors-mode))

(defun skip-to-next-like-symbol-under-cursor ()
  (interactive)
  (highlight-symbol-next)
  (mc/maybe-multiple-cursors-mode))

(global-set-key (kbd "M-e") 'mark-next-like-symbol-under-cursor)
(global-set-key (kbd "M-E") 'skip-to-next-like-symbol-under-cursor)

;;,----
;;| PHP
;;`----
(require 'php-mode)

(require 'php-boris)
(setq php-boris-command "~/source/boris/bin/boris"
      php-boris-prompt "\\[\\d+\\] >>>")

;;,-------------------------------------------------
;;| Save open buffers and remember position in files
;;`-------------------------------------------------
(require 'saveplace)
(setq-default save-place t
              save-place-file "~/.emacs.d/saved-places"
              save-place-forget-unreadable-files nil)

(delete-selection-mode 1)

;;,---------
;;| Org mode
;;`---------
(require 'org)
;; Org mode for todos
(require 'org-install)
(define-key mode-specific-map [?a] 'org-agenda)
(global-set-key (kbd "<f8>") 'org-agenda)
(define-key org-mode-map (kbd "C-c C-*") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c C-*") 'org-edit-src-exit)
(define-key org-src-mode-map (kbd "C-c C-l") 'org-store-link)
(setq org-src-fontify-natively t)

(add-to-list 'org-modules 'habits)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (setq org-use-fast-todo-selection t
	   org-completion-use-ido t
	   org-deadline-warning-days 14
	   org-reverse-note-order t
	   org-fast-tag-selection-single-key 'expert
	   org-todo-keywords '((sequence "TODO(t!)"
					 "STARTED(s!)"
					 "WAITING(w!)"
					 "|"
					 "DONE(d!)"
					 "CANCELLED(f!)"))
	   org-log-into-drawer t
	   org-todo-log-states )))

(require 'org-agenda)
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)
     (defun agenda-todo-choose-status()
       (interactive)
       (let ((current-prefix-arg '(4)))
	 (call-interactively 'org-agenda-todo)))
     (define-key org-agenda-mode-map (kbd "C-c C-c") 'org-agenda-todo-choose-status)

     (setq org-agenda-files (list tasks-file work-file)
           org-default-notes-file notes-file
	   org-agenda-span 7
	   org-agenda-show-all-dates t
	   org-agenda-skip-deadline-if-done t
	   org-agenda-skip-scheduled-if-done t
	   org-agenda-start-on-weekday nil)
  
     (setq org-agenda-custom-commands
    	'(("d" todo "DELEGATED" nil)
    	  ("c" todo "DONE|DEFERRED|CANCELLED" nil)
    	  ("w" todo "WAITING" nil)
    	  ("W" agenda ""
    	   ((org-agenda-ndays 21)))
    	  ("A" agenda ""
    	   ((org-agenda-skip-function
    	     (lambda nil
    	       (org-agenda-skip-entry-if
    		(quote notregexp)
    		"\\=.*\\[#A\\]")))
    	    (org-agenda-ndays 1)
    	    (org-agenda-overriding-header "Today's Priority #A tasks: ")))
    	  ("u" alltodo ""
    	   ((org-agenda-skip-function
    	     (lambda nil
    	       (org-agenda-skip-entry-if
    		(quote scheduled)
    		(quote deadline)
    		(quote regexp)
    		"
]+>")))
	    (org-agenda-overriding-header "Unscheduled TODO entries: ")))))))

(add-hook 'org-mode-hook #'ft/org-hook)
(defun ft/org-hook ()
  (visual-line-mode 1)
  (toggle-word-wrap 1))

;;(add-to-list 'org-mode-hook
;;	     (lambda ()
;;	       (org-babel-do-load-languages 'org-babel-load-languages
;;	        			    '((emacs-lisp . t)
;;	        			      (python . t)
;;	        			      (php . t)
;;	        			      (sqlite . t)
;;	        			      (sh . t)
;;	        			      (ditaa . t)
;;                                              (R . t)))
;;	       (visual-line-mode 1)
;;	       (toggle-word-wrap 1)))
;;
;;(require 'ob-ditaa)
;;(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
;;
;;(require 'htmlize)
;;(require 'ox-html)
;;(setq org-html-htmlize-output-type 'css)
;;
;;(require 'ob-php)
;;
;;(require 'ox-latex)
;;(setq org-latex-listings 'minted)
;;(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-capture-templates
   `(("t" "Task" entry
      (file+headline ,tasks-file "Tasks")
      "* TODO %?
  %u")
     ("w" "Work task" entry
      (file+headline ,work-file "Tasks")
      "* TODO %?
%u")
     ("n" "Note" entry
      (file ,notes-file)
      ,(let ((add-origin-file '(let ((file (org-capture-get :original-file)))
				 (if (> (length file) 0)
				     (string-join (list "From " "[[" file "]]")))))
	     (add-content '(let ((content (plist-get org-store-link-plist :initial)))
			     (if (> (length content) 0)
				 (string-join
				  (list "  #+begin_src "
					(first (split-string
						(with-current-buffer (org-capture-get :buffer)
						  (symbol-name major-mode))
						"-"))
				   "\n" content
				   "\n  #+end_src"))))))
	 ;; build the template string for org-mode
	 (mapconcat 'identity (list "* %U %?\n"
			    "%" (prin1-to-string add-origin-file) "\n"
			    "%" (prin1-to-string add-content))
		    ""))
      :prepend t)))

;;,------------------------------
;;| Tech blog config for org-mode
;;`------------------------------
(require 'org-blog)
(define-key org-mode-map (kbd "C-c b") 'extract-bog-post)

;;,---------
;;| Flycheck
;;`---------
(require 'flycheck)
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
	      flycheck-idle-change-delay 1
	      flycheck-display-errors-delay 0.1
              ;; flycheck-display-errors-function 'flycheck-display-error-messages
              ;; flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list
              )
(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

;;,------------------------------
;;| Type q in a non-editor window
;;`------------------------------

(defun q-other-window ()
  "Simple implementation of q-other-window
It finds the first window where q is not bound to self-insert and type q"
  (interactive)
  (let ((target (get-window-with-predicate
		 (lambda (win)
		   (with-selected-window win
		     (let ((q-binding (key-binding "q")))
		       (not (or
			     (eq q-binding 'org-self-insert-command)
			     (eq q-binding 'self-insert-command)))))))))
    (if target
	(with-selected-window target
	  (call-interactively (key-binding "q")))
      (message "No suitable window found for q-other-window"))))

; More convienent key bindings for commands used all the time
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'magit-status)

; And replacement functions
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; rename the current file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename))))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;;,-----------------------------
;;| Help for symbol under cursor
;;`-----------------------------
;; go to definition
(defun ft/elisp-jump-definition ()
  (interactive)
  (find-function-do-it (function-called-at-point) nil 'switch-to-buffer))
(define-key emacs-lisp-mode-map (kbd "C-*") #'ft/elisp-jump-definition)

;; show doc
(defun ft/elisp-describe-symbol ()
  (interactive)
  (let ((sym (symbol-at-point)))
    (cond ((or (functionp sym) (macrop sym) (special-form-p sym))
           (describe-function sym))
          ((facep sym) (describe-face sym))
          ((boundp sym) (describe-variable sym))
          (t (message (concat (symbol-name sym) " not a known symbol, use C-u to search for sexp "))))))
(define-key emacs-lisp-mode-map (kbd "C-?") #'ft/elisp-describe-symbol)

;;,------
;;| Scala
;;`------
(require 'sbt-mode)
(defun sbt-run ()
  (interactive)
  (sbt-command "run"))
(global-set-key (kbd "<f11>") 'sbt-run)

(require 'scala-mode2)
(add-to-list 'scala-mode-hook
	     (lambda ()
	       (require 'ensime)
	       (setq ensime-ac-enable-argument-placeholders nil
		     ensime-ac-override-settings t
		     ensime-typecheck-idle-interval 0.3
		     ensime-typecheck-interval 1)))

;;,------------------
;;| Visible Mark mode
;;`------------------
;; No transient mode, but I'd like to see
(transient-mark-mode -1)
(require 'visible-mark)
(set-face-attribute 'visible-mark-active nil :background "dark green")
(global-visible-mark-mode)

;;,-----
;;| Smex
;;`-----
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;,-------------------
;;| Dired hide details
;;`-------------------
(require 'dired-details)
(setq dired-details-initially-hide t
      dired-details-hide-extra-lines t)

;;,--------------
;;| Dired subtree
;;`--------------
(require 'dired-subtree)

(defun ft/dired-subtree-cycle ()
  "Insert subtree (and move to first file) if not expanded, remove if expanded"
  (interactive)
  (if (dired-subtree--is-expanded-p)
      (progn (dired-next-line 1)
	     (dired-subtree-remove))
    (dired-subtree-insert)))
(define-key dired-mode-map (kbd "i") 'ft/dired-subtree-cycle)

;;,---------------------------------
;;| Visual regexp with modern syntax
;;`---------------------------------
(require 'visual-regexp-steroids)

;;,----------------------
;;| Smooth page scrolling
;;`----------------------
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

;;,------
;;| Latex
;;`------
(require 'tex-mode)
(setq tex-start-commands "--shell-escape")

;;,------
;;| Magit
;;`------
(require 'magit)
(setq magit-use-overlays nil
      magit-completing-read-function 'magit-ido-completing-read
      magit-auto-revert-mode nil
      magit-last-seen-setup-instructions "1.4.0")

;; from http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*"))
    (unless (member
             fetch-address
             (magit-get-all "remote" "origin" "fetch"))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)

;;,--------------------------
;;| Maximize frame on startup
;;`--------------------------
(defun ft/maximize-frame (frame)
  (unless (eq (frame-parameter frame 'fullscreen) 'maximized)
    (toggle-frame-maximized)))

(ft/maximize-frame nil) ; current frame

;;,-------
;;| Comint
;;`-------
(add-hook 'comint-mode-hook (lambda ()
			      (setq-local undo-limit 1000)))

;;,---------
;;| God Mode
;;`---------
(require 'god-mode)

;; M-t is really easy to press on a bépo keyboard
;; Alt with left thumb, t with right index
(global-set-key (kbd "M-t") 'god-mode-all)

; change cursor according to god-mode state
(defun ft/god-mode-update-cursor ()
  (setq cursor-type (if god-local-mode ;;(or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(add-hook 'god-mode-enabled-hook (lambda ()
				   (company-cancel)
				   ;; (relative-line-numbers-mode)
				   (ft/god-mode-update-cursor)))
(add-hook 'god-mode-disabled-hook (lambda ()
				    ;; (relative-line-numbers-mode 0)
				    (ft/god-mode-update-cursor)))

(defun ft/god-mode-self-insert-for-org ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))
(define-key god-local-mode-map [remap self-insert-command] #'ft/god-mode-self-insert-for-org)

(defadvice q-other-window (around q-other-window-god-mode activate)
  (if (bound-and-true-p god-global-mode)
      (progn
	(god-mode-all)
	ad-do-it
	(god-mode-all))
    ad-do-it))

(add-hook 'after-init-hook #'god-mode-all)

(defun org-mode-change-major-mode ()
  (if (eq major-mode 'fundamental-mode)
      (god-mode-all)))
;; (add-hook 'after-change-major-mode-hook #'god-mode-change-major-mode)

;;,------------
;;| Common Lisp
;;`------------
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(require 'slime-repl)
(slime-setup '(slime-fancy
	       slime-company))

(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(add-hook 'lisp-mode-hook (lambda () 
			    (eldoc-mode 1)
			    (paredit-mode)))
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(define-key slime-repl-mode-map (kbd "C-?") #'slime-documentation)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key)
    nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;,--------------------------
;;| Keymap for bépo keyboards
;;`--------------------------
(require 'bepo_map)

;; from https://tsdh.wordpress.com/2015/03/03/swapping-emacs-windows-using-dragndrop/
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end
window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))
(global-set-key (kbd "<C-S-drag-mouse-1>") #'th/swap-window-buffers-by-dnd)

;;,-----
;;| Rust
;;`-----
(use-package rust-mode
  :config
  (defun ft/rust-config ()
    (require 'racer)
    (racer-activate)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (set (make-local-variable 'company-backends) '(company-racer))
    (setq racer-rust-src-path "/usr/src/rust/src/")
    (setq racer-cmd "/usr/bin/racer"))
  (add-to-list 'rust-mode-hook #'ft/rust-config))

;;,-----
;;| Misc
;;`-----
(setq doc-view-continuous t
      visual-line-fringe-indicators '(left-curly-arrow nil))
;;,------
;;| C/C++
;;`------
;;(defun ft/c-mode-hook ()
;;  (irony-mode 1)
;;  (irony-eldoc 1)
;;  (hide-ifdef-mode 1))
;;(add-hook c-mode-hook #'ft/c-mode-hook)

;; (eval-after-load 'flycheck
;; '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(global-set-key (kbd "C-x C-'") #'next-error)
(global-set-key (kbd "<f5>") #'ff-find-other-file)

;;,----------------------
;;| Clean up the modeline
;;`----------------------
(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (format "%s|%s" (or .error 0) (or .warning 0)))
                   ""))
                (`interrupted "-")
                (`suspicious "?"))))
    (concat " ε" text)))

;;,---------
;;| Flyspell
;;`---------
;; (use-package flyspell
;; :bind (("<f7>" . #'flyspell-buffer)
;; ("C-<f7>" . #'flyspell-mode)
;; ("C-S-<f7>" . #'ispell-change-dictionary)))

;;,------
;;| Ocaml
;;`------

;; Load merlin-mode
(use-package tuareg
  :mode "\\.ml\\'"
  :config
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

  (require 'merlin)
  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)

  ;;(add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (add-hook 'tuareg-mode-hook #'merlin-mode))

;;;;;;

(defun endless/comment-line (n)
  "Comment or urcomment current line and leave point after it."
  (interactive "p")
  (let ((start (line-beginning-position))
        (end (goto-char (line-end-position n))))
    (comment-or-uncomment-region
     (min start end)
     (max start end))
    (forward-line 1)
    (back-to-indentation)))

;; PDF
(use-package pdf-tools
  :config
  (pdf-tools-install))

;;,-------
;;| tcl/tk
;;`-------
(use-package tcl-mode
  :mode "\\.tk\\'")

;;,-----
;;| Ruby
;;`-----
(use-package inf-ruby
  :interpreter "ruby"
  :init
  (setq inf-ruby-default-implementation "pry"))

(use-package robe
  :mode "\\.rb\\'"
  :init
  (setq robe-completing-read-func 'ido-completing-read)
  :config
  (defun setup-robe ()
    (robe-start)
    (robe-mode))
  (add-hook 'ruby-mode-hook #'setup-robe)
  (define-key robe-mode-map (kbd "C-?") #'robe-doc)
  (define-key robe-mode-map (kbd "C-*") #'robe-jump))

(use-package skewer-mode
             :commands (run-skewer skewer-mode)
             :init
             (setq httpd-port 8088
                   httpd-root "~/"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (tide-mode 1))

(use-package tide-mode
  :config
  (define-key tide-mode-map (kbd "C-?") #'tide-documentation-at-point)
  (define-key tide-mode-map (kbd "C-*") #'tide-jump-to-definition))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;,-----------------
;;| Custom powerline
;;`-----------------
(require 'ft-line)
(add-hook 'after-init-hook #'ft/powerline-theme)

;;,--------------------
;;| Handle mac keyboard
;;`--------------------
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/Users/florent/.virtualenvs")



(define-key prog-mode-map (kbd "C-=") #'imenu)

;; The end
(provide 'init)
