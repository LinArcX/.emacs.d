;; init directories----------------------------------------------------------------------------------
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; pacakge.el ----------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(require 'visible-mark)
(visible-mark-mode)

; lovely packages that i need :)
(require 'cl-lib)
(defvar my-packages
  '(gruvbox-theme mentor
    vertico marginalia consult counsel orderless company undo-tree)) ;centaur-tabs

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


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;centaur-tabs undo-tree
(use-package mentor)

(setq mentor-rtorrent-download-directory "/mnt/E/rtorrent/download")
(setq mentor-rtorrent-keep-session t)
(setq mentor-rtorrent-external-rpc "~/.rtorrent-rpc.socket")
;(setq mentor-rtorrent-external-rpc "scgi://127.0.0.1:5000")

(use-package gruvbox-theme)
(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("C-r" . consult-ripgrep)
     ;;    ("C-c M-x" . consult-mode-command)
     ;;    ("C-c h" . consult-history)
     ;;    ("C-c k" . consult-kmacro)
     ;;    ("C-c m" . consult-man)
     ;;    ("C-c i" . consult-info)
     ;;    ([remap Info-search] . consult-info)
     ;;    ;; C-x bindings in `ctl-x-map'
     ;;    ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
     ;;    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
     ;;    ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
     ;;    ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
     ;;    ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
     ;;    ;; Custom M-# bindings for fast register access
     ;;    ("M-#" . consult-register-load)
     ;;    ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
     ;;    ("C-M-#" . consult-register)
     ;;    ;; Other custom bindings
     ;;    ("M-y" . consult-yank-pop)                ;; orig. yank-pop
     ;;    ;; M-g bindings in `goto-map'
     ;;    ("M-g e" . consult-compile-error)
     ;;    ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck

     ;;    ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
     ;;    ("M-g m" . consult-mark)
     ;;    ("M-g k" . consult-global-mark)
     ;;    ("M-g i" . consult-imenu)
     ;;    ("M-g I" . consult-imenu-multi)
     ;;    ;; M-s bindings in `search-map'
     ;;    ("M-s d" . consult-find)
     ;;    ("M-s D" . consult-locate)
     ;;    ("M-s g" . consult-grep)
     ;;    ("M-s G" . consult-git-grep)
     ;;    ("M-s l" . consult-line)
     ;;    ("M-s L" . consult-line-multi)
     ;;    ("M-s k" . consult-keep-lines)
     ;;    ("M-s u" . consult-focus-lines)
     ;;    ;; Isearch integration
     ;;    ("M-s e" . consult-isearch-history)
     ;;    :map isearch-mode-map
     ;;    ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
     ;;    ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
     ;;    ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
     ;;    ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
     ;;    ;; Minibuffer history
     ;;    :map minibuffer-local-map
     ;;    ("M-s" . consult-history)                 ;; orig. next-matching-history-element
     ;;    ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
     )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package counsel)
(use-package company
  :init
  (setq company-global-modes '(not org-mode)))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)

  ;;; Different scroll margin
  (setq vertico-scroll-margin 4)

  ;;; Show more candidates
  ;(setq vertico-count 20)

  ;;; Grow and shrink the Vertico minibuffer
  ;(setq vertico-resize t)

  ;;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; configure packages----------------------------------------------------------------------------------
;; company
;(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-SPC") 'company-complete))

;; global settings ----------------------------------------------------------------------------------
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(pixel-scroll-precision-mode 1)
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

;; mappings
(defun linarcx-reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (unicode-fonts-setup))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun linarcx-kill-current-window ()
  (interactive)
  (kill-current-buffer)
  (delete-other-windows))

(defun linarcx-open-new-line-switch-to-it ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun linarcx-describe-bindings-fullscreen ()
  (interactive)
  (describe-bindings)
  (delete-other-windows))

(defun eval-init-el ()
  "Open and evaluate ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (eval-buffer))


(defun save-with-line-kmark ()
  "Save the buffer and set a mark at the current line."
  (interactive)
  (save-buffer)
  (set-mark-command))

(defun jump-back-to-latest-mark ()
  (execute-kbd-macro [?\C-u ?\C- ]))
  ;(interactive)
  ;(let ((prefix-arg (universal-argument)))
  ;  (set-mark-command prefix-arg)))


;  (bookmark-set (format "Line %d" (line-number-at-pos))))

;; Since you're using emacs in client/server mode, you shouldn't close it!
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-l"))

;;; help
(global-set-key (kbd "C-h b") 'linarcx-describe-bindings-fullscreen)

;;; linarcx
(global-set-key (kbd "C-l b") 'beginning-of-buffer)
(global-set-key (kbd "C-l e") 'end-of-buffer)
(global-set-key (kbd "C-l d") 'duplicate-dwim)
(global-set-key (kbd "C-l r") 'restart-emacs)
(global-set-key (kbd "C-l l") 'eval-init-el)

;;; others
(global-set-key (kbd "C-o") 'linarcx-open-new-line-switch-to-it)
(global-set-key (kbd "C-q") 'linarcx-kill-current-window)
(global-set-key (kbd "C-w") 'save-with-line-bookmark)
(global-set-key (kbd "M-o") 'jump-back-to-latest-mark)

(global-set-key (kbd "C-w") 'save-buffer)
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-;") 'execute-extended-command)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-<left>") 'previous-buffer)
(global-set-key (kbd "C-<right>") 'next-buffer)

(global-set-key (kbd "C-S-s") 'shell-command)
(global-set-key (kbd "C-S-v") 'eval-expression)
(global-set-key (kbd "C-S-r") 'linarcx-reload-init-file)
(global-set-key (kbd "M-SPC") 'company-complete)
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
(global-set-key (kbd "C-x C-b") (lambda ()
		      (interactive)
		      (ibuffer)
		      (delete-other-windows)))

; dired
(defun linarcx-dired-open()
  (interactive)
  (setq file (dired-get-file-for-visit))
  (setq ext (file-name-extension file))
  (cond ((string= ext "pdf") (start-process "Evince" nil "evince" file))
    ((string= ext "epub") (start-process "Foliate" nil "foliate" file))
    ((string= ext "mp4") (start-process "Mpv" nil "mpv" file))
    ((string= ext "avi") (start-process "Mpv" nil "mpv" file))
    ((string= ext "mkv") (start-process "Mpv" nil "mpv" file))
    ((string= ext "gif") (start-process "Mpv" nil "mpv" file))
    ((string= ext "flv") (start-process "Mpv" nil "mpv" file))
    ((string= ext "ogv") (start-process "Mpv" nil "mpv" file))
    ((string= ext "wmv") (start-process "Mpv" nil "mpv" file))
    ((string= ext "mp3") (start-process "Mpv" nil "mpv" file))
    ((string= ext "3gp") (start-process "Mpv" nil "mpv" file))
    ((string= ext "aac") (start-process "Mpv" nil "mpv" file))
    ((string= ext "ogg") (start-process "Mpv" nil "mpv" file))
    ((string= ext "wav") (start-process "Mpv" nil "mpv" file))
    ((string= ext "ttf") (start-process "FontManager" nil "font-manager" file))
    ((string= ext "TTF") (start-process "FontManager" nil "font-manager" file))
    ((string= ext "otf") (start-process "FontManager" nil "font-manager" file))
    ((string= ext "woff") (start-process "FontManager" nil "font-manager" file))
    ((string= ext "rar") (dired-view-file))
    ((string= ext "zip") (dired-view-file))
    ((string= ext "jpg") (start-process "Nomacs" nil "nomacs" file))
    ((string= ext "png") (start-process "Nomacs" nil "nomacs" file))
    ((string= ext "jpeg") (start-process "Nomacs" nil "nomacs" file))
    ((string= ext "ttf") (start-process "Nomacs" nil "nomacs" file))
  (t (dired-find-file))))

(eval-after-load "dired" '(progn
		;(define-key dired-mode-map (kbd "p") nil)
		;(define-key dired-mode-map (kbd "n") nil)
		(define-key dired-mode-map (kbd "l") 'linarcx-dired-open) ; was dired-advertised-find-file
		(define-key dired-mode-map (kbd "RET") 'linarcx-dired-open)
		;(define-key dired-mode-map (kbd "h") 'dired-up-directory) ;(lambda () (interactive) (find-alternate-file "..")))
		;(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)
		;(define-key dired-mode-map (kbd "j") 'dired-next-line)
		;(define-key dired-mode-map (kbd "k") 'dired-previous-line)
		;(define-key dired-mode-map (kbd "q") 'linarcx-quit)
		;(define-key dired-mode-map (kbd ":") 'linarcx-open-fav-dir)
		;(define-key dired-mode-map (kbd ";") 'linarcx-open-fav-dir-vsplit)
		;(define-key dired-mode-map (kbd "/") 'dired-narrow-regexp)
		;(define-key dired-mode-map (kbd "[") 'dired-hide-details-mode)
		;(define-key dired-mode-map (kbd "SPC") 'dired-mark)
		;(define-key dired-mode-map (kbd "<delete>") 'dired-do-delete)
		;(define-key dired-mode-map (kbd "TAB") (lambda() (interactive)(other-window -1)))
		;(define-key dired-mode-map (kbd "C-x C-d") 'dired-jump)
		;(define-key dired-mode-map (kbd "C-x C-e") 'dired-jump-other-window)
		;(define-key dired-mode-map (kbd "C-t") (lambda ()
		;					 (interactive)
		;					 (split-window-vertically)
		;					 (other-window 1)))
		;(define-key dired-mode-map (kbd "<f6>") (lambda ()
		;					  (interactive)
		;					  (dired-do-rename)
		;					  (other-window 1)
		;					  (revert-buffer)
		;					  (dired-unmark-all-marks)))
		;(define-key dired-mode-map (kbd "<f7>") 'dired-create-empty-file)
		;(define-key dired-mode-map (kbd "<f8>") 'dired-create-directory)
		))

;; functions
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

;(add-hook 'after-init-hook
;          '(lambda ()
;             (require 'bookmark)
;             (bookmark-bmenu-list)
;             (switch-to-buffer "*Bookmark List*")))

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
 ;'(mentor-rtorrent-download-directory nil nil nil "/mnt/E/rtorrent/download")
 '(package-selected-packages '(vertico ## company gruvbox-theme)))

(cond ((eq system-type 'gnu/linux)
  (setq linarcx-shell "/bin/bash")
  (setq-default shell-file-name linarcx-shell)
  (setq-default explicit-shell-file-name linarcx-shell)
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
  (setq bookmark-default-file "~/.emacs.d/bookmarks_linux")
  (setq dired-listing-switches "-lghA --time-style long-iso --group-directories-first")
  (global-set-key (kbd "M-t") (lambda () (interactive) (term linarcx-shell)))
  ))


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

;; centaur-tabs
;(centaur-tabs-mode t)
;(centaur-tabs-headline-match)
;(centaur-tabs-group-by-projectile-project)
;(centaur-tabs-change-fonts "Consolas" 130)
;(setq centaur-tabs-set-icons t)
;(setq centaur-tabs-set-bar 'over)
;(setq centaur-tabs-style "slant")
;(setq centaur-tabs-modified-marker "∙")
;(setq centaur-tabs-set-modified-marker t)
;(setq centaur-tabs-gray-out-icons 'buffer)
;(global-set-key (kbd "C-<left>")  'centaur-tabs-backward)
;(global-set-key (kbd "C-<right>") 'centaur-tabs-forward)

;; undo-tree
