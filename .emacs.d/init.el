;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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
(if window-system (tool-bar-mode -1))

;; Hide the menu bar
(menu-bar-mode -1)

;; Hide the scroll bar
(if window-system (set-scroll-bar-mode nil))

;; Transparency
(if window-system (progn (set-frame-parameter nil 'alpha 75)))

;; Title bar
(setq frame-title-format "%b")

;; Size up to full screen
(let ((ws window-system))
  (cond ((eq ws 'ns) ; for MacBook Pro 15 inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 88 50))))

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
(when (eq window-system 'ns)
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 160)
  (set-fontset-font nil 'japanese-jisx0208
                    (font-spec :family "Ricty")))

;; For Windows
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 120)
  (set-fontset-font nil
                    'japanese-jisx0208
                    (font-spec :family "Meiryo"))
  (setq face-font-rescale-alist '((".*Consolas.*" . 1.0)
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

;; Use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;;----------------------------------------------------------------------------
;;; Text Decorations
;;;----------------------------------------------------------------------------

;; Highlight parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; Show trailing while spaces
(setq-default show-trailing-whitespace t)
