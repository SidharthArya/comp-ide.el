;;; comp-ide.el - A very basic implementation of a competitive ide on emacs.

;; Copyright (C) 2020-2021 Sidharth Arya

;; Author: Sidharth Arya <sidhartharya10@gmail.com>
;; Maintainer: Sidharth Arya <sidhartharya10@gmail.com>
;; Created: 28 May 2020
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools
;; URL: https://github.com/SidharthArya/comp-ide.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA.

;;; Commentary:

;; comp-ide.el attempts to be a simple and efficient competitive coding IDE.
;; The *Input* buffer is mapped to the stdin of program and *Output* buffer to stdout.
;; 

;;; Code:


(require 'delsel)
(require 'eshell)
(require 'subr-x)

(defun find-from-dict(list option &optional elem)
  "Allows to use list of list as a dictionary."
  (if (equal elem nil)
      (nth 1 (nth (cl-position option list :test (lambda (a b) (member a b))) list))
    (nth elem (nth (cl-position option list :test (lambda (a b) (member a b))) list))
    )
  )
(defun insert-into-string(string identifier repl)
  "replace an identifier in a string"
  (string-join(split-string string identifier) repl)
  )
;; (require 'flycheck)
;; (require 'company)
;; (defvar comp-ide-hook '(yas-minor-mode company-mode flycheck-mode))
(defvar comp-ide-hook '())
(defun ide/comp-ide-init()
  )
(defvar ide-right-perc 30)
(defvar ide-shell-perc 20)
(defvar ide-input-perc 50)
(setq comp-ide-command-map (make-sparse-keymap))
(defun ide/comp-ide-open()
  "Something"
  (interactive)
  (defvar comp-ide nil)
  (defvar ide/extension nil)
  (defvar ide/file-name nil)
  (defvar ide/command nil)
  (defvar ide/temp nil)
  (defvar ide-code-buffer nil)
  (defvar ide-output-buffer nil)
  (defvar ide-input-buffer nil)
  (defvar ide-shell-buffer nil)
  (setq ide-code-buffer (get-buffer-window))
  (split-window-below (/ (* (- 100 ide-shell-perc) (window-height)) 100))
  (other-window 1)
  (eshell)
  (ide-slave-mode t)
  (set-window-dedicated-p (get-buffer-window) t)
  (setq ide-shell-buffer (get-buffer-window))
  (other-window 1)
  (split-window-right (/ (* (- 100 ide-right-perc) (window-width)) 100))
  (other-window 1)
  (switch-to-buffer "*Output*")
  (ide-slave-mode t)
  (set-window-dedicated-p (get-buffer-window) t)
  (setq ide-output-buffer (get-buffer-window))
  (split-window-vertically (/ (* (- 100 ide-input-perc) (window-height)) 100))
  (other-window 1)
  (switch-to-buffer "*Input*")
  (ide-slave-mode t)
  (set-window-dedicated-p (get-buffer-window) t)
  (setq ide-input-buffer (get-buffer-window))
  (other-window 2)
  (setq comp-ide t)
  (run-hooks 'comp-ide-hook)
  (setq split-window-preferred-function nil)
  )


(defun ide/comp-ide-compile()
  "Something"
  (interactive)
  (setq ide/extension (nth 1 (split-string (buffer-name) "\\.")))
  (setq ide/file-name (nth 0 (split-string (buffer-name) "\\.")))
  
  (setq ide/command (find-from-dict ide/comp-ide-compile-recipes ide/extension))
  (setq ide/command (string-join (split-string (string-join (split-string ide/command "%bf") (buffer-name)) "%bo") ide/file-name))
  (compile ide/command)
  )
(defun ide/comp-ide-execute()
  (interactive)
  (setq ide/command (find-from-dict ide/comp-ide-execute-recipes ide/extension))
  (setq ide/command (string-join (split-string (string-join (split-string ide/command "%bf") (buffer-name)) "%bo") ide/file-name))
  
  (defvar ide/file-name (nth 0 (split-string (buffer-name) "\\.")))
  (with-current-buffer (get-buffer "*Output*")
    (erase-buffer)
    )
  (with-current-buffer (get-buffer "*Input*")
    (shell-command-on-region (point-min) (point-max) ide/command)
    )
  (with-current-buffer (get-buffer "*Shell Command Output*")
    (kill-region (point-min) (point-max))
    (kill-buffer "*Shell Command Output*")
    )
  (with-current-buffer (get-buffer "*Output*")
    (yank)
    )
  )  

(defun ide/comp-ide-close() 
  "Something"
  (interactive)
  (kill-buffer "*eshell*")
  (kill-buffer "*Output*")
  ;(kill-buffer "*Input*")
  (makunbound 'ide-code-buffer)
  (makunbound 'ide-shell-buffer)
  (makunbound 'ide-output-buffer)
  (makunbound 'ide-input-buffer)
  (makunbound 'ide/extension)
  (makunbound 'ide/extension)
  (defvar ide/file-name nil)
  (defvar ide/command nil)
  (defvar ide/temp nil)
  (defvar ide-code-buffer nil)
  (defvar ide-output-buffer nil)
  (defvar ide-input-buffer nil)
  (defvar ide-shell-buffer nil)

  (setq comp-ide nil)
  (run-hooks 'comp-ide-hooks)
  (setq split-window-preferred-function 'split-window-sensibly)
  )
(defun ide/goto-shell()
  "Something"
  (interactive)
  (select-window ide-shell-buffer)
  )

(defun ide/goto-output()
  "Something"
  (interactive)
  (select-window ide-output-buffer)
  )
(defun ide/goto-input()
  "Something"
  (interactive)
  (select-window ide-input-buffer)
  )
(defun ide/goto-code()
  "Something"
  (interactive)
  (select-window ide-code-buffer)
  )
(defun ide/send-to-output(string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (kill-append string nil))
  (with-current-buffer (get-buffer "*Output*")
    (mark-whole-buffer)
    (delete-active-region)
    (yank)
    )
  )
(defun ide/syntax-check()
  "After Save Check"
  (interactive)
  (let (( output (shell-command-to-string (insert-into-string (find-from-dict ide/geterrors (file-name-extension (buffer-name))) "%bf" (buffer-name))))
	(line (split-string (shell-command-to-string (concat "echo -e \"" output "\" | awk '{print $2}'")) "\n")) 
	(char (shell-command-to-string (concat "echo -e \"" output "\" | awk '{print $3}'"))))
    (mapcar 'set-fringemark-at-point line))
  )
(add-to-list 'eshell-virtual-targets  '("/dev/ide" (lambda(mode) (with-current-buffer (get-buffer "*Output*") (mark-whole-buffer) (delete-active-region)) (kill-new " ") 'ide/send-to-output) t))

(define-minor-mode comp-ide
  ""
  :lighter " ID"
  :keymap (make-sparse-keymap)
  (if comp-ide
      (ide/comp-ide-open)
    (ide/comp-ide-close)))

(define-minor-mode ide-slave-mode
  ""
  :lighter " ID"
  :keymap (make-sparse-keymap))
(provide 'comp-ide)
;;; comp-ide.el ends here
