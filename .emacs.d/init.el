;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;;----------------------------------------------------------------------------
;;; Information
;;;----------------------------------------------------------------------------

(setq user-full-name "Hideyuki Tai")

;;;----------------------------------------------------------------------------
;;; load-path
;;;----------------------------------------------------------------------------

;; Automatically add sub directories to load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
     (let ((default-directory (expand-file-name
                               (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;; Add directories to load-path
(add-to-load-path "elisp")

;;;----------------------------------------------------------------------------
;;; Key Bindings
;;;----------------------------------------------------------------------------

;; If there is only one window, create other window,
;; else move to another window.
;; See http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; Search the word your cursor looking at
;;   See http://d.hatena.ne.jp/suztomo/20081123/1227466198
(defun isearch-forward-with-heading ()
  "Search the word your cursor looking at."
  (interactive)
  (command-execute 'backward-word)
  (command-execute 'isearch-forward))
(global-set-key "\C-s" 'isearch-forward-with-heading)

;; Go to line N
(global-set-key "\M-g" 'goto-line)

;;;----------------------------------------------------------------------------
;;; Keybaord
;;;----------------------------------------------------------------------------

;; Translate "C-h" to DEL
(keyboard-translate ?\C-h ?\C-?)

;; Use Mac's option key as meta
(when (eq system-type 'ns)
  (setq mac-option-modifier 'meta))

;;;----------------------------------------------------------------------------
;;;  Language Environment
;;;----------------------------------------------------------------------------

;; Language Environment: Japanese
(set-language-environment 'Japanese)

;; Prefer UTF-8 coding system
(prefer-coding-system 'utf-8)

;; Coding Settings for Mac
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Coding Settings for Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;;----------------------------------------------------------------------------
;;; Frame Settings
;;;----------------------------------------------------------------------------

;; Show line number on mode lines
(line-number-mode t)

;; Show column number on mode lines
(column-number-mode t)

;; Hide the tool bar
(tool-bar-mode -1)

;; Hide the menu bar
(menu-bar-mode -1)

;; Hide the scroll bar
(set-scroll-bar-mode nil)

;; Transparency
(if window-system (progn (set-frame-parameter nil 'alpha 75)))

;; Title bar
(setq frame-title-format "%b")

;; Size up to full screen
(let ((ws window-system))
  (cond ((eq ws 'ns) ; for MacBook Pro 15 inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 175 50))))

;;;----------------------------------------------------------------------------
;;;  Window Settings
;;;----------------------------------------------------------------------------

;; Not truncate lines
(set-default 'truncate-lines nil)
;; Not truncate lines in horizontally-split windows
(setq truncate-partial-width-windows nil)

;;;----------------------------------------------------------------------------
;;;  Text Editing
;;;----------------------------------------------------------------------------

;; Set tab size to 4 chars
(setq-default tab-width 4)
;; Not use tabs
(setq-default indent-tabs-mode nil)

;; Selecting rectangular region
(cua-mode t)
(setq cua-enable-cua-keys nil) ; not use cua's compatibility key bindings

;; Kill the entire line plus the newline
(setq kill-whole-line t)

;;;----------------------------------------------------------------------------
;;; Fonts
;;;----------------------------------------------------------------------------

;; For Mac
;; See http://macemacsjp.sourceforge.jp/
(when (eq window-system 'ns)
  (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal"
                                  nil
                                  "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN")
                    nil
                    'append)
  (add-to-list 'default-frame-alist
               '(font . "fontset-menlokakugo"))
  (setq face-font-rescale-alist
        '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0))))

;; For Windows
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 120)
  (set-fontset-font
   nil
   'japanese-jisx0208
   (font-spec :family "Meiryo"))
  (setq face-font-rescale-alist
        '((".*Consolas.*" . 1.0)
          (".*Meiryo.*" . 1.15)
          ("-cdac$" . 1.3))))

;;;----------------------------------------------------------------------------
;;; Emacs Behavior
;;;----------------------------------------------------------------------------

;; Inhibit the startup message
(setq inhibit-startup-message t)

;; Max number of recent opened file
(setq recentf-max-saved-items 1000)

;; Max number of history
(setq history-length 3000)

;; Make backup files
(setq make-backup-files t)
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))

;; Auto save files
(setq auto-save-default t)
(defvar autosave-dir (expand-file-name "~/.emacs.d/backup/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;----------------------------------------------------------------------------
;;; auto-install.el
;;;
;;;   http://www.emacswiki.org/emacs/download/auto-install.el
;;;----------------------------------------------------------------------------

(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/auto-install/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
  (add-to-list 'load-path "~/.emacs.d/auto-install/"))

;;;----------------------------------------------------------------------------
;;; color-theme.el
;;;
;;;   http://code.google.com/p/gnuemacscolorthemetest/
;;;----------------------------------------------------------------------------

(when (and (require 'color-theme nil t) (window-system))
  (color-theme-initialize)
  (color-theme-clarity))

;;;----------------------------------------------------------------------------
;;; uniquify.el
;;;   is part of GNU Emacs
;;;----------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;----------------------------------------------------------------------------
;;; auto-complete.el
;;;
;;;   http://cx4a.org/software/auto-complete/manual.ja.html
;;;
;;;   After installing, create "ac-dict" directory.
;;;----------------------------------------------------------------------------

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (ac-config-default))

;;;----------------------------------------------------------------------------
;;; color-moccur.el/moccur-edit.el
;;;
;;;   multi-buffer grep
;;;
;;;   To install, use auto-install.el
;;;     M-x install-elisp-from-emacswiki color-moccur.el
;;;     M-x install-elisp-from-emacswiki moccur-edit.el
;;;----------------------------------------------------------------------------

(when (and (require 'color-moccur nil t)
           (require 'moccur-edit nil t))
  (define-key global-map (kbd "M-o") 'moccur-grep-find)
  ;; enable AND search
  (setq moccur-split-word t)
  ;; Exclude some files
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store"))

;;;----------------------------------------------------------------------------
;;; undo-tree.el
;;;
;;;   To install, use ELPA (Emacs Lisp Package Archive)
;;;     M-x package-install RET undo-tree RET
;;;----------------------------------------------------------------------------

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;;----------------------------------------------------------------------------
;;; anything.el
;;;
;;;   To install, use auto-install.el
;;;     M-x auto-install-batch anything
;;;----------------------------------------------------------------------------

(when (require 'anything-startup nil t)
  (define-key global-map (kbd "C-x b") 'anything-filelist+)
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring))

;;;----------------------------------------------------------------------------
;;; org-mode.el
;;;
;;;   is part of GNU Emacs
;;;----------------------------------------------------------------------------

(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

;;;----------------------------------------------------------------------------
;;; google-c-style.el
;;;
;;;   http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el
;;;----------------------------------------------------------------------------

(when (require 'google-c-style nil t)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extra Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some settings should be evaluated after load elisp packages

;;;----------------------------------------------------------------------------
;;; Text Decorations
;;;----------------------------------------------------------------------------

;; Highlight parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; Show trailing while spaces
(setq-default show-trailing-whitespace t)
