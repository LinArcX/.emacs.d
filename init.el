;; init directories----------------------------------------------------------------------------------
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; pacakge.el ----------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; lovely packages that i need :)
(require 'cl-lib)
(defvar my-packages
  '(gruvbox-theme undo-tree
    consult company centaur-tabs))

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; configure packages----------------------------------------------------------------------------------
;; company
;(add-hook 'after-init-hook 'global-company-mode)
(setq company-global-modes '(not org-mode))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-SPC") 'company-complete))

;; undo-tree
(global-undo-tree-mode)

;; centaur-tabs
(centaur-tabs-mode t)
(centaur-tabs-headline-match)
(centaur-tabs-group-by-projectile-project)
(centaur-tabs-change-fonts "Consolas" 130)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-style "slant")
(setq centaur-tabs-modified-marker "∙")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-gray-out-icons 'buffer)
(global-set-key (kbd "C-<left>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<right>") 'centaur-tabs-forward)

;; global settings ----------------------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(line-number-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)
(setq-default display-fill-column-indicator-column 120)
(setq display-fill-column-indicator-column 120)
(global-display-fill-column-indicator-mode 1)
;; Set the default font for regular buffers
(add-to-list 'default-frame-alist '(font . "Consolas-13")) ;FiraCodeMedium / FantasqueSansMono / CascadiaCode
;; Set the font for the Gnu About Emacs buffer
(custom-set-faces
 '(default ((t (:family "Consolas" :height 130)))))
;(set-face-attribute 'default nil :height 145)

;; (setq auto-revert-verbose nil)
;; (global-auto-revert-mode -1)
(global-auto-revert-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Improve Performance in image-view, pdf-view.
(setq jit-lock-defer-time 0.05)
(setq show-paren-style 'mixed)
(setq debug-on-error t)
(setq create-lockfiles nil)
(setq help-window-select t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-follow-symlinks nil)
(setq inhibit-splash-screen t)
(setq delete-by-moving-to-trash t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq comp-deferred-compilation t)
(setq byte-compile-warnings '(cl-functions))
;;(setq calendar-week-start-day 6)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; change theme based on the hour of day
(run-at-time "09:00" nil (lambda () (load-theme 'gruvbox-light-hard t)))
(run-at-time "11:00" nil (lambda () (load-theme 'gruvbox-light-medium t)))
(run-at-time "16:00" nil (lambda () (load-theme 'gruvbox-light-soft t)))
(run-at-time "17:00" nil (lambda () (load-theme 'gruvbox-dark-soft t)))
(run-at-time "19:00" nil (lambda () (load-theme 'gruvbox-dark-medium t)))
(run-at-time "21:00" nil (lambda () (load-theme 'gruvbox-dark-hard t)))
(run-at-time "02:00" nil (lambda () (load-theme 'gruvbox-dark-medium t)))
(run-at-time "05:00" nil (lambda () (load-theme 'gruvbox-dark-soft t)))
(run-at-time "07:00" nil (lambda () (load-theme 'gruvbox-light-soft t)))
(run-at-time "08:00" nil (lambda () (load-theme 'gruvbox-light-medium t)))

;; cunctions
(defun linarcx-term-mode-hook ()
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "C-k")
  (lambda ()
    (interactive)
    (term-send-raw-string "\C-k")
    (kill-line))))

(defun linarcx-delete-trailing-whitespace ()
  (delete-trailing-whitespace)
  ;(if (eq major-mode 'c-mode) (call-process-shell-command (concat "clang-format -i " (buffer-file-name))) (message "wrong major-mode"))
  )

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun linarcx-set-current-theme()
  (interactive)
  (setq linarcx-current-hour (nth 2 (decode-time (current-time))))
  (cond (
    (and (>= linarcx-current-hour 9) (< linarcx-current-hour 11))
      (load-theme 'gruvbox-light-hard t)
    (and (>= linarcx-current-hour 11) (< linarcx-current-hour 16))
      (load-theme 'gruvbox-light-medium t)
    (and (>= linarcx-current-hour 16) (< linarcx-current-hour 17))
      (load-theme 'gruvbox-light-soft t)
    (and (>= linarcx-current-hour 17) (< linarcx-current-hour 19))
      (load-theme 'gruvbox-dark-soft t)
    (and (>= linarcx-current-hour 19) (< linarcx-current-hour 21))
      (load-theme 'gruvbox-dark-medium t)
    (and (>= linarcx-current-hour 21) (< linarcx-current-hour 0))
      (load-theme 'gruvbox-dark-hard t)
    (and (>= linarcx-current-hour 0) (< linarcx-current-hour 2))
      (load-theme 'gruvbox-dark-hard t)
    (and (>= linarcx-current-hour 2) (< linarcx-current-hour 5))
      (load-theme 'gruvbox-dark-medium t)
    (and (>= linarcx-current-hour 5) (< linarcx-current-hour 7))
      (load-theme 'gruvbox-dark-soft t)
    (and (>= linarcx-current-hour 7) (< linarcx-current-hour 8))
      (load-theme 'gruvbox-light-soft t)
    (and (>= linarcx-current-hour 8) (< linarcx-current-hour 9))
      (load-theme 'gruvbox-light-medium t)
    )))

(defun linarcx-files-recursively (root extension)
  "List all files with `extension' in `root'"
  (interactive)
  (setq files (directory-files-recursively root extension)))

(defun linarcx-revert-buffer(&rest _)
  (revert-buffer))

;; hooks
(add-hook 'term-mode-hook 'linarcx-term-mode-hook)
(add-hook 'before-save-hook 'linarcx-delete-trailing-whitespace)
(add-hook 'emacs-startup-hook
  '(lambda ()
  (linarcx-set-current-theme)
	;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
	(setq-default message-log-max nil)
	(when (get-buffer "*Messages*")
	  (kill-buffer "*Messages*"))
	(when (get-buffer "*straight-process*")
	  (kill-buffer "*straight-process*"))
  (run-with-idle-timer 1 nil
    (lambda ()
      (when (get-buffer "*Warnings*")
        (kill-buffer "*Warnings*"))
        (delete-other-windows)
      ))
  ))

;; advices
(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;; auto-generated ----------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(## company undo-tree gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; garbage ----------------------------------------------------------------------------------
;;;; move line up
;;(defun move-line-up ()
;;  (interactive)
;;  (transpose-lines 1)
;;  (previous-line 2))
;;(global-set-key [(control shift up)] 'move-line-up)
;;
;;;; move line down
;;(defun move-line-down ()
;;  (interactive)
;;  (next-line 1)
;;  (transpose-lines 1)
;;  (previous-line 1))
;;(global-set-key [(control shift down)] 'move-line-down)
;;
;;(eval-after-load "prog-mode" '(progn
;;  (define-key prog-mode-map (kbd "C-q") 'kill-this-buffer)))
;;
;;(add-hook 'prog-mode-hook (lambda ()
;;  (hl-line-mode t)
;;	(electric-pair-mode 1)
;;	(company-mode)))
