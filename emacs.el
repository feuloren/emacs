;; Teh One Macro To Rule Them All
;; Actually it's useless, I had to use it because I forgot to call (package-initialize)
;; But we'll keep it here for History
(defmacro later (&rest body)
  "Runs after init when load-path are set packages are loaded"
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

;;,----------------
;;| Package / MELPA
;;`----------------

(require 'package)
;(add-to-list 'package-archives
;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
      '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;,-----------
;;| Appearance
;;`-----------

;; load monokai theme
(require 'monokai-theme)

;; Highlight current line
(global-hl-line-mode -1)
(set-face-background 'hl-line "#101010")
(set-face-foreground 'highlight nil)

; no menu bar, no toolbar, no scrollbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;;,----------
;;| Undo Tree
;;`----------

(require 'undo-tree)
(global-undo-tree-mode)

;;,----------
;;| Yasnippet
;;`----------
;; Still not convinced
;; (later (require 'yasnippet)
;;        (define-key yas-minor-mode-map [tab] nil) ; Don't disturb my
;;                                      ; tab key yas !
;;        (define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)
;;        (yas-global-mode 1))

;;,--------------------------
;;| Company + Jedi for python
;;`--------------------------
(global-company-mode)
(add-hook 'python-mode-hook (lambda ()
			      (anaconda-mode)
			      (eldoc-mode)
			      (run-python "python")))

;;,-----------
;;| Projectile
;;`-----------

(projectile-global-mode)

;;,------------
;;| CSS editing
;;`------------
; rainbow mode sets backgroudn color based on detected color code
(add-hook 'css-mode-hook 'rainbow-mode)

;;,-----
;;| Helm
;;`-----

(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-adaptive)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-execute-selection-action)

(define-key helm-map (kbd "C-x b") 'helm-mini)

(require 'helm-projectile)
(global-set-key (kbd "C-x h") 'helm-projectile)

(require 'helm-imenu)
(define-key helm-map (kbd "C-c C-j") 'helm-imenu)
(helm-mode t)

;;,--------------------------
;;| Highlight symbol at point
;;`--------------------------

(require 'highlight-symbol)

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
;;,----------------------
;;| Company (autocomple everything)
;;`----------------------
(require 'company)
(setq company-idle-delay 0.1)
(define-key company-active-map [return] 'newline)
(define-key company-active-map [tab] 'company-complete-selection) ;; we actually need something smarter like tab once to complete common part, then tap another timee to complete selction

;(define-key ac-completing-map "\t" 'ac-complete) ; tab to complete
;(define-key ac-completing-map [return] nil) ; not enter
;(define-key ac-completing-map "\r" nil)

;;,---------
;;| Web mode
;;`---------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;

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

;; Paredit
(require 'paredit)
;; Swap word movement and barfing/slurping commands bindings
;; Paredit is the only mode using Meta for movement so it
;; breaks my automatisms
;; Now beehave correctly paredit !
;; I also swap barf/slurp because it makes visually much more
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
  (mc/maybe-multiple-cursors-mode)
  )

(defun fake-cursor-at-point ()
  (let ((end (max (mark) (point)))
	furthest)
    (mc/for-each-fake-cursor
     (when (>= (mc/cursor-beg cursor) end)
       (setq end (mc/cursor-end cursor))
       (setq furthest cursor)))
    furthest))

(defun skip-to-next-like-symbol-under-cursor ()
  (interactive)
  (highlight-symbol-next)
  (mc/maybe-multiple-cursors-mode))

(global-set-key (kbd "M-e") 'mark-next-like-symbol-under-cursor)
(global-set-key (kbd "M-E") 'skip-to-next-like-symbol-under-cursor)

;;,----
;;| PHP
;;`----
(defun php-insert-$this ()
  (interactive)
  (insert "$this->"))

(require 'php-mode)
(define-key php-mode-map (kbd "M-s") 'php-insert-$this)

(require 'php-boris)
(setq php-boris-command "~/source/boris/bin/boris")
(setq php-boris-prompt "\\[\\d+\\] >>>")

;;,-------------------------------------------------
;;| Save open buffers and remember position in files
;;`-------------------------------------------------
;;(setq 
;; [desktop-restore-eager 15]
;; [desktop-save t]
;; [desktop-save-mode t]
;; [save-place t nil (saveplace)]
;; [save-place-limit 400])

(delete-selection-mode 1)

;;,---------------
;;| Key chord mode
;;`---------------
(require 'key-chord)
;;(setq key-chord-two-keys-delay 0.05) ; got to type it real
;;fast !
(key-chord-mode t)
(key-chord-define-global ",;" 'dired-jump)
(key-chord-define-global "df" 'backward-word)
(key-chord-define-global "kj" 'forward-word)
(key-chord-define-global "à)" 'undo)
(key-chord-define-global "0°" 'undo-tree-redo)
(key-chord-define-global ")=" 'helm-imenu)
(key-chord-define-global "ù*" 'org-capture)
					; C-x o is not nice to type, trying to find a good key-binding
(key-chord-define-global "$$" 'other-window)


;;,---------
;;| Org mode
;;`---------
(require 'org)
(define-key org-mode-map (kbd "C-c *") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c *") 'org-edit-src-exit)
(define-key org-src-mode-map (kbd "C-c l") 'org-store-link)
(setq org-src-fontify-natively t)
(require 'htmlize)
(setq org-html-htmlize-output-type 'css)

(require 'ob-php)

(require 'ox-latex)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-capture-templates
   '(("ù" "Task" entry
      (file+headline "~/org/tasks-main.org" "Tasks")
     "* TODO %?
  %u")
     ("*" "Note" entry
      (file "~/org/notes.org")
     "* %U %?
%(let ((file (org-capture-get :original-file)))
   (if (> (length file) 0)
     (string-join (list \"From \" file))))
%(let ((content (plist-get org-store-link-plist :initial)))
    (if (> (length content) 0)
      (string-join
        (list
          \"  #+begin_src \" (first (split-string (with-current-buffer (org-capture-get :buffer) (symbol-name major-mode)) \"-\"))
          \"\\n\" content
          \"\\n  #+end_src\"))))" :prepend t)))

;;,---------
;;| Flycheck
;;`---------
(require 'flycheck)
(global-flycheck-mode)
(eval-after-load 'flycheck
  (lambda () (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;; Org mode for todos
(require 'org-install)
(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (setq org-use-fast-todo-selection t)
     (setq org-todo-keywords
	   '((sequence "TODO(t)"
		       "STARTED(s)"
		       "WAITING(w)"
		       "DELEGATED(l)" "|"
		       "DONE(d)"
		       "DEFERRED(f)")))))
 
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

     (setq org-agenda-files '("~/org/tasks-main.org")
           org-default-notes-file "~/org/notes.org")
  
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

;; Week view TODO
;; (global-set-key (kbd "<f7>") 'org-agenda-week-view)
;; Unclassified tasks view
;; (global-set-key (kbd "<f8>") 'org-agenda)

;;,------------------------------
;;| Tech blog config for org-mode
;;`------------------------------
(load (concat (file-name-directory load-file-name) "org-blog"))
(define-key org-mode-map (kbd "C-c b") 'extract-bog-post)

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

(key-chord-define-global "qq" 'q-other-window)

; More convienent key bindings for commands used all the time
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'magit-status)

; And replacement functions
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;,-----------------------------
;;| Help for symbol under cursor
;;`-----------------------------
(defun help-around-cursor ()
  "Try to be smart and open help/definition for symbol under cursor
Operation depends on the mode :
- In emacs-lisp check if symbol under cursor if a function and display its documentation if so
 - Use universal-argument to search for first symbol in sexp
- Same operation for clojure but lookup in clojure-cheatset
- I'll add more modes as I need it
- In undefined modes act as if in amcs-lisp mode"
  (interactive)
  (let ((sym (symbol-at-point)))
    (case major-mode
      ('clojure-mode (message "Not yet, sorry"))
      (t
       (if (or (functionp sym) (macrop sym) (special-form-p sym))
	   (describe-function sym)
	 (message (concat (symbol-name sym) " not a function, use C-u to search for sexp head"))))))
  )

(global-set-key (kbd "<C-f1>") 'help-around-cursor)

;;,------
;;| Scala
;;`------
(require 'sbt-mode)
(defun sbt-run ()
  (interactive)
  (sbt-command "run"))
(global-set-key (kbd "<f11>") 'sbt-run)
