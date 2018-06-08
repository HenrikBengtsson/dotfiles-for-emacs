(message "evaluating ~/.emacs...")

;; ------------------------------------------------------
;; Use a separate file for Emacs custom settings
;; This has two advantages:
;; * It keeps this initialization file clean.
;; * It prevents secret settings from being added to
;;   this file by mistake, e.g. private API keys.
;; ------------------------------------------------------
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/elisp")


;; ------------------------------------------------------
;; ELPA (Emacs Lisp Package Archive)
;; MELPA (Emacs Lisp Package Archive)
;;
;; "Our goal is to make it simple to install, use,
;; and upgrade Emacs Lisp packages."
;;
;; Usage: Menu > Options > Manage Emacs Packages
;; 
;; Source:
;;   http://www.emacswiki.org/emacs/ELPA
;;   http://melpa.org/#/getting-started
;; ------------------------------------------------------
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ))
  (package-initialize)
  )

;; -------------------------------------------------
;; Modifies Ctrl-a
;; -------------------------------------------------
(load-library "smarter-move-beginning-of-line")


;; -------------------------------------------------
;; Key chords
;; -------------------------------------------------
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "hb"     "Henrik Bengtsson")


;; -------------------------------------------------
;; multiple cursors
;; Source: https://github.com/magnars/multiple-cursors.el#readme
;; -------------------------------------------------
(require 'multiple-cursors)
;; Recommended is C-S-c C-S-c, but C-S (Ctrl-Shift)
;; is not picked up by the ssh terminal I'm using
;; resulting in only a C-c (Control-c). /HB 2015-02-01
;; Also C-c C-c is used by ESS.
(global-set-key (kbd "C-c C-m") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; -------------------------------------------------
;; Thinkpad specific
;; -------------------------------------------------
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)


;; Display current column position
(setq column-number-mode t)


;; ------------------------------------------------------
;; WindMove - navigate between windows (multiple buffers
;; displayed at the same time) using arrow keys, i.e.
;; Shift + UP, Shift + DOWN, Shift + LEFT, Shift + RIGHT
;; Source: http://www.emacswiki.org/emacs/WindMove
;; ------------------------------------------------------
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; -------------------------------------------------
;; markdown-mode
;; -------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; RSP-embedded version
(define-derived-mode rsp-markdown-mode markdown-mode "RSP-Markdown"
  "Major mode for editing RSP-embedded Markdown files."
  (add-hook 'rsp-markdown-mode-hook 'turn-off-auto-fill)
)
(add-to-list 'auto-mode-alist '("\\.md\\.rsp$" . rsp-markdown-mode))


;; ------------------------------------------------------
;; ESS
;; ------------------------------------------------------
;; (ess-toggle-underscore nil)

;; Prevent automatic newlines at the end
;; (setq mode-require-final-newline nil)


;; ------------------------------------------------------
;; Source: http://editorconfig.org/
;; ------------------------------------------------------
;; (load "editorconfig")


;; ------------------------------------------------------
;; Show matching brackets
;; http://stackoverflow.com/questions/13575554/display-line-of-matching-bracket-in-the-minibuffer
;; ------------------------------------------------------
;; "I assume you have turned on show-paren-mode so matching
;;  parens are highlighted:"
;; (show-paren-mode t)

;; "Then this will show the matching line if the paren is 
;;  off the screen:"
(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))



;; ------------------------------------------------------
;; Source: http://endlessparentheses.com/super-smart-capitalization.html
;; ------------------------------------------------------
(global-set-key "\M-c" 'endless/capitalize)
(global-set-key "\M-l" 'endless/downcase)
(global-set-key "\M-u" 'endless/upcase)

(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))


;; -------------------------------------------------
;; Chrome: Edit with Emacs
;; -------------------------------------------------
(require `edit-server)
(edit-server-start)

;; -------------------------------------------------
;; Git: magit
;; -------------------------------------------------
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

(message "Evaluating ~/.emacs...done")
(put 'dired-find-alternate-file 'disabled nil)
