; init directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

; init pacakge.el
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

; Install packages with package.el
(require 'cl-lib)
(defvar my-packages
  '(gruvbox-theme undo-tree company))

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

; enable packages
(add-hook 'after-init-hook 'global-company-mode)
(global-undo-tree-mode)

; global settings
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
(add-to-list 'default-frame-alist '(font . "Consolas-13")) ;FiraCodeMedium / FantasqueSansMono / CascadiaCode
;(set-face-attribute 'default nil :height 145)

; change theme based on the hour of day
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

;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key [(control shift up)] 'move-line-up)

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key [(control shift down)] 'move-line-down)

(eval-after-load "prog-mode" '(progn
  (define-key prog-mode-map (kbd "C-q") 'kill-this-buffer)))

(add-hook 'prog-mode-hook (lambda ()
  (hl-line-mode t)
	(electric-pair-mode 1)
	(company-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company undo-tree gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
