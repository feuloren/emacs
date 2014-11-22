;;; org-blog --- My tech blogging worflow
;;; Commentary:
;;; Save all the drafts in the same file
;;; Each

;;; Code:

(require 'ox-publish)
(require 's)
(require 'dash)
(require 'org-capture)

(defvar tech-blog-path "~/Projets/techBlog/")
(setq org-publish-project-alist
      `(
	;; Publish every org file to HTML
	;; Org-publish manages updated/added files itself (awesome !)
	("tech-blog"
	 :base-directory ,tech-blog-path
	 :base-extension "org"
	 :exclude "export-template.org\\|drafts.org"
	 :publishing-directory ,(concat tech-blog-path "html/")
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :section-numbers nil
	 :headline-levels 2             ; Just the default for this project.
	 :html-preamble "<a href=\"index.html\">Home</a>"
	 :style-include-default nil  ;Disable the default css style
	 :auto-postamble nil         ; Disable auto postamble
	 :html-postamble "<p class=\"postamble\">Florent Thévenet<br/>Last Updated %d.</p>"
	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "<Insert smart name here>"
	 :sitemap-sort-files "anti-chronologically"
	 :sitemap-file-entry-format "%d %t"
	 :sitemap-date-format "%F"
	 :sitemap-function tech-blog-generate-index
	 )
	))

(defun tech-blog-generate-index (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'.
Copied from org-publish-org-sitemap"
  (message "Generating sitemap")
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (localdir (file-name-directory dir))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse
		 (org-publish-get-base-files project exclude-regexp)))
	 (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			  (concat "Sitemap for project " (car project))))
	 (sitemap-sans-extension
	  (plist-get project-plist :sitemap-sans-extension))
	 (visiting (find-buffer-visiting sitemap-filename))
	 file sitemap-buffer)
    (with-current-buffer
	(let ((org-inhibit-startup t))
	  (setq sitemap-buffer
		(or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; Put that stuff in some config variable
      (insert "#+HTML_DOCTYPE: html5" "\n")
      (insert "#+AUTHOR: Florent Thévenet" "\n")
      (insert "#+HTML_HEAD: <LINK rel=\"stylesheet\" type=\"text/css\" href=\"assets/blog-style.css\">" "\n")

      (insert "#+DATE: " (format-time-string (org-time-stamp-format t t) (org-current-time)) "\n")
      (insert "#+OPTIONS: toc:nil" "\n")
      (insert "#+TITLE: " sitemap-title "\n\n")
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file))
	      (link (file-relative-name file dir))
	      (oldlocal localdir))
	  (when sitemap-sans-extension
	    (setq link (file-name-sans-extension link)))
	  ;; sitemap shouldn't list itself
	  (unless (equal (file-truename sitemap-filename)
			 (file-truename file))
	    (let ((summary (extract-org-post-summary file)))
	      (insert "* " (cdr (assoc 'date summary))
		      " [[file:" link "]["
		      (cdr (assoc 'title summary))
		      "]]" "\n"
		      (cdr (assoc 'text summary)) "\n")
	      ))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun org-get-property (name)
  "Return value of first property `name` in current buffer or nil if no such property"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (concat "#+" name ":"))
      (set-mark (point))
      (end-of-line)
      (s-trim (buffer-substring (mark) (point))))))

(defun org-post-get-intro-text ()
  "Get intro text from the current buffer"
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n* ")
    (forward-line)
    (set-mark (point))
    (search-backward "#+")
    (forward-line)
    (buffer-substring (mark) (point))))

(defun extract-org-post-summary (file)
  "Extract date, title and intro text from a pos file"
  (with-temp-buffer
    (insert-file-contents file)
    `((date . ,(org-get-property "DATE"))
      (title . ,(org-get-property "TITLE"))
      (text . ,(org-post-get-intro-text)))))

(defun create-slug (text)
  (->> text
    (s-trim)
    (s-collapse-whitespace)
    (s-downcase)
    (s-replace " " "-")))

(defun extract-bog-post ()
  "Take the draft the point is in and extract it in its own file"
  (interactive)
  ; Go to draft start heading and show everything
  (search-backward-regexp "^* ")
  (org-show-subtree)
  (let* (;; Get post text, title
	 (draft-pos (list (point) (find-draft-end (point))))
	 (draft (apply 'buffer-substring draft-pos))
	 (title (with-temp-buffer
		  (insert draft)
		  (org-get-property "TITLE")))
	 ;; create buffer to store the file
	 (filename (create-slug title))
	 (filepath (concat tech-blog-path filename ".org"))
	 (buffer (create-file-buffer filepath)))
    (with-current-buffer buffer
      (message (concat filename " created"))
      ;; Add text
      (insert draft)
      ;; kill first line
      (goto-char (point-min))
      (kill-whole-line)
      ;; insert export template
      (insert "#+SETUPFILE: export-template.org\n")
      ;; promote every entry to account for main heading deletion
      (while (re-search-forward "^\\*\\*" nil t)
	(replace-match "*"))
      ; set visited file so we just have to press C-x C-s
      (set-visited-file-name filepath))
    ;; remove ex-draft from draft file and save drafts
    (apply 'delete-region draft-pos)
    (save-buffer)
    (message "Drafts saved")
    ;; Display the new buffer, activate org-mode and show the whole outline
    (set-window-buffer (selected-window) buffer)
    (org-mode)
    ; usage of run-at-time is a hack because I can't figure out why
    ; simply calling (show-all) does not work
    (run-at-time 0.1 nil 'show-all)))

(defun find-draft-end (start)
  ;; Return the position at the end of the draft we are in
  (save-excursion
    (goto-char start)
    (forward-line)
    (or (re-search-forward "^* " nil t) (point-max))))

(add-to-list 'org-capture-templates
	     `("t" "Tech Blog post" entry
	       (file ,(concat tech-blog-path "drafts.org"))
     "* New draft
#+DATE: %(format-time-string \"[%F]\")
#+TITLE: %?
" :jump-to-captured t))

(provide 'org-blog)
;;; org-blog.el ends here
