(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; packages
(setq package-selected-packages
      '(ido elpy atom-dark-theme ace-window restart-emacs diff-hl flycheck py-isort git-commit projectile blacken typescript-mode prettier-js))
 

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; custom functions
(defun fold-code (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(defun flycheck-all-file-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer (flycheck-buffer)))
   (cl-remove-if-not 'buffer-file-name (buffer-list)))
  )

(defun set-default-directory ()
  (interactive)
  (setq use-dialog-box nil)
    (let ((path (read-file-name "Enter path to project directory:")))
      (write-region path nil "~/.emacs.d/project-directory"))
    (setq default-directory (get-default-directory))
    (restart-emacs))

(defun get-default-directory ()
  (interactive)
  (unless (file-exists-p "~/.emacs.d/project-directory")
    (set-default-directory))
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/project-directory")
    (buffer-string)))

;; emacs customizations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tooltip-mode -1)
(global-hl-line-mode 1)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar
	      indent-tabs-mode nil)
(setq tooltip-mode nil
      initial-frame-alist '((fullscreen . maximized)) ; maximize screen on startup
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-buffer-choice  nil
      confirm-nonexistent-file-or-buffer nil ; don't ask for confirmation when a new file/buffer is created
      column-number-mode t
      ring-bell-function 'ignore
      read-file-name-completion-ignore-case t
      mouse-wheel-scroll-amount '(0.07)
      mouse-wheel-progressive-speed nil
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      backup-directory-alist `(("." . "~/.emacs.d/emacs_backups"))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
(load-theme 'atom-dark t)
(set-frame-font "Fira Code 10")

;; keybindings
(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(global-set-key (kbd "<C-[>") nil)
(global-set-key (kbd "C-x C-m") 'comment-region)
(global-set-key (kbd "C-x C-u") 'uncomment-region)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-;") 'fold-code)
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

;; ido mode
(ido-mode t)

;; cua-mode
(cua-mode t)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)

;; diff-hl
(global-diff-hl-mode)

;; flycheck
(global-flycheck-mode 1)

(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'before-save-hook 'py-isort-before-save)
(setq blacken-line-length 140)

;; typescript
(require 'prettier-js)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(setq prettier-js-width-mode 2)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(put 'downcase-region 'disabled nil)
