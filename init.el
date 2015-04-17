;; Run GC every ~40Mo allocated
(setq gc-cons-threshold 40000000) ; 800000 by default

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

; no menu bar, no toolbar, no scrollbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;;,----------
;;| Undo Tree
;;`----------

(require 'undo-tree)
(global-undo-tree-mode)

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

;;,--------------------
;;| The silver searcher
;;`--------------------
;; Download windows executable here http://blog.kowalczyk.info/software/the-silver-searcher-for-windows.html
(require 'ag)
(setq ag-highlight-search t)

;;,------------
;;| CSS editing
;;`------------
; rainbow mode sets backgroudn color based on detected color code
(add-hook 'css-mode-hook 'rainbow-mode)

;;,----
;;| IDO
;;`----
(require 'ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      confirm-nonexistent-file-or-buffer nil
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

(ido-vertical-mode t)

(ido-mode +1)
(ido-ubiquitous-mode +1)

;;,--------------------------
;;| Highlight symbol at point
;;`--------------------------

(require 'highlight-symbol)

(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'org-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-on-navigation-p t ; enable highlighting symbol at point automatically
      highlight-symbol-idle-delay 0.3)

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
(setq company-idle-delay 0.1
      company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
      company-minimum-prefix-length 1
      company-backends '(company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
				      (company-dabbrev-code company-gtags company-etags company-keywords)
				      company-oddmuse company-files company-ispell
				      (company-ispell company-dabbrev)))
(define-key company-active-map [return] 'newline) ;; I don't want
;; company to get in my way
(define-key company-active-map [tab] 'company-complete-selection) ;; we actually need something smarter like tab once to complete common part, then tap another timee to complete selction
(global-company-mode)

;;,-----------
;;| Yasnippets
;;`-----------
(require 'yasnippet)

;; When the company popup is on I want tab to complete the selection
;; not go to the next field
(defadvice yas-next-field-or-maybe-expand (around yas-after-company-advice activate compile)
  (if (null company-point)
      ad-do-it
    (call-interactively 'company-complete-selection)))

(yas-global-mode 1)

;; tab is for indent, I use yas-insert-snippet to select a snippet and expand it
(define-key yas-minor-mode-map [tab] nil)

;;,---------
;;| Web mode
;;`---------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;,---------
;;| JS2 mode
;;`---------
(require 'js2-mode)

;; Set as default for JS file
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js2-strict-trailing-comma-warning nil)


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
(setq inhibit-startup-screen t
      initial-major-mode 'org-mode
      initial-scratch-message nil)

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
(defun php-insert-$this ()
  (interactive)
  (insert "$this->"))

(require 'php-mode)
(define-key php-mode-map (kbd "M-s") 'php-insert-$this)

(require 'php-boris)
(setq php-boris-command "~/source/boris/bin/boris"
      php-boris-prompt "\\[\\d+\\] >>>")

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

;;,---------
;;| Org mode
;;`---------
(require 'org)
(define-key org-mode-map (kbd "C-c *") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c *") 'org-edit-src-exit)
(define-key org-src-mode-map (kbd "C-c l") 'org-store-link)
(setq org-src-fontify-natively t)

(require 'htmlize)
(require 'ox-html)
(setq org-html-htmlize-output-type 'css)

(require 'ob-php)

(require 'ox-latex)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-capture-templates
   `(("t" "Task" entry
      (file+headline "~/org/tasks-main.org" "Tasks")
      "* TODO %?
  %u")
     ("n" "Note" entry
      (file "~/org/notes.org")
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

;;,---------
;;| Flycheck
;;`---------
(require 'flycheck)
(global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
	      flycheck-idle-change-delay 0.2
	      flycheck-display-errors-delay 0.1
	      flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

;;,---------
;;| Org mode
;;`---------

;; Org mode for todos
(require 'org-install)
(define-key mode-specific-map [?a] 'org-agenda)

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
	   org-todo-keywords '((sequence "TODO(t)"
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
           org-default-notes-file "~/org/notes.org"
	   org-agenda-ndays 7
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

(add-to-list 'org-mode-hook
	     (lambda ()
	       (org-babel-do-load-languages 'org-babel-load-languages
					    '((emacs-lisp . t)
					      (python . t)
					      (php . t)
					      (R . t)
					      (sh . t)
					      (ditaa . t)))
	       (visual-line-mode 1)
	       (toggle-word-wrap 1)))

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")

;; Week view TODO
;; (global-set-key (kbd "<f7>") 'org-agenda-week-view)
(global-set-key (kbd "<f8>") 'org-agenda)

;;,------------------------------
;;| Tech blog config for org-mode
;;`------------------------------
(require 'org-blog)
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

; More convienent key bindings for commands used all the time
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'magit-status)

; And replacement functions
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
      ('python-mode (call-interactively 'anaconda-mode-view-doc)
		    ;; I want the focus to stay in the calling window
		    (other-window -1))
      (t
       (cond ((or (functionp sym) (macrop sym) (special-form-p sym))
	      (describe-function sym))
	     ((boundp sym) (describe-variable sym))
	     (t (message (concat (symbol-name sym) " not a known symbol, use C-u to search for sexp head"))))))))

(global-set-key (kbd "<C-f1>") 'help-around-cursor)

(require 'find-func)
(defun ft/go-to-definition ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (find-function-do-it (function-called-at-point) nil 'switch-to-buffer))
   ((bound-and-true-p anaconda-mode) (call-interactively 'anaconda-mode-goto-definitions))
   ((bound-and-true-p semantic-mode) (call-interactively 'semantic-ia-fast-jump)))) ; TODO extend for vars, face and other modes

(defun ft/go-to-forward-mouse (nk-event)
  "Control-click to go to definition like in IntelliJ"
  (interactive "@e")
  (posn-set-point (event-end  nk-event))
  (call-interactively 'ft/go-to-definition))
(global-set-key (kbd "C-<mouse-1>") #'ft/go-to-forward-mouse)

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
;; Helm is great for a lot of inputs but finding what I want in M-x
;; with it takes too long
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

;;,----------------
;;| Smart mode line
;;`----------------
(column-number-mode)
(require 'smart-mode-line)
(sml/setup)

;;,-----------------
;;| Dash integration
;;`-----------------
;;(require 'helm-dash)
;;(defun ft/dash-install ()
;;  (loop for doc in '("Android" "AngularJS" "C" "Clojure" "CodeIgniter" "CSS" "Java_SE7" "jQuery" "MySQL" "PHP" "PHPUnit" "Scala" "SQLite")
;;	do (helm-dash-install-docset doc)))
;;
;;(defmacro ft/mode-set-dash-docsets (mode-hook &rest sets)
;;  `(add-hook (quote ,mode-hook) (lambda ()
;;				  (setq-local helm-dash-docsets (quote ,sets)))))
;;
;;(add-hook 'php-mode-hook (lambda ()
;;			   (setq-local helm-dash-docsets '("PHP" "PHPUnit"))))
;;(ft/mode-set-dash-docsets js-mode-hook "AngularJS" "jQuery")
;;(ft/mode-set-dash-docsets clojure-mode-hook "Clojure" "Java_SE7")
;;(ft/mode-set-dash-docsets php-mode-hook "PHP" "PHPUnit" "CodeIgniter")
;;(ft/mode-set-dash-docsets scala-mode-hook "Scala" "Android")
;;(setq helm-dash-common-docsets '())

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

;;,----------------------
;;| Relative line numbers
;;`----------------------
(require 'relative-line-numbers)
(setq relative-line-numbers-current-line-symbol ">"
      relative-line-numbers-motion-function 'forward-visible-line)

;;,---------
;;| God Mode
;;`---------
(require 'god-mode)

;; M-t is really easy to press on a bépo keyboard
;; Alt with left thumb, t with right index
(global-set-key (kbd "M-t") 'god-mode-all)

; change cursor according to god-mode state
(defun ft/god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(add-hook 'god-mode-enabled-hook (lambda ()
				   (company-cancel)
				   (relative-line-numbers-mode)
				   (ft/god-mode-update-cursor)))
(add-hook 'god-mode-disabled-hook (lambda ()
				    (relative-line-numbers-mode 0)
				    (ft/god-mode-update-cursor)))

(define-key god-local-mode-map (kbd ".") 'repeat)

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

;;,------------
;;| Common Lisp
;;`------------
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup '(slime-fancy
	       slime-company))

(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(add-hook 'lisp-mode-hook (lambda () 
			    (eldoc-mode 1)
			    (paredit-mode)))
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

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
;;(require 'rust-mode)
;;(add-to-list 'rust-mode-hook (lambda ()
;;			       (require 'racer)
;;			       (setq racer-rust-src-path "~/sources/rust/src/")
;;			       (setq racer-cmd "~/source/racer/target/release/racer")))

;;,-----
;;| Misc
;;`-----
(setq doc-view-continuous t
      visual-line-fringe-indicators '(left-curly-arrow nil))

(provide 'init)
