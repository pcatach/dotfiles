(package-initialize)

;; packages
(setq package-list '(ido flycheck-mypy neotree elpy atom-dark-theme guru-mode ace-window))
(setq package-check-signature nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package))
  (require package))
(add-to-list 'load-path "~/.emacs.d/blacken/")
(load "blacken")

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

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

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
	  (write-region path nil "~/.emacs.d/project-directory")))

(defun get-default-directory ()
  (interactive)
  (unless (file-exists-p "~/.emacs.d/project-directory")
    (set-default-directory))
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/project-directory")
    (buffer-string)))

;; emacs customizations
(setq default-directory (get-default-directory))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tooltip-mode -1)
(global-hl-line-mode 1)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar)
(setq tooltip-use-echo-area t
      initial-frame-alist '((fullscreen . maximized))
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      confirm-nonexistent-file-or-buffer nil
      column-number-mode t
      ring-bell-function 'ignore
      read-file-name-completion-ignore-case t
      mouse-wheel-scroll-amount '(0.07)
      mouse-wheel-progressive-speed nil
      backup-directory-alist `(("." . "~/.emacs.d/emacs_backups"))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
(load-theme 'atom-dark t)
;;(set-frame-font "Inconsolata-12")
(set-frame-font "Fira Code 8")

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

;; guru mode
(add-hook 'prog-mode-hook 'guru-mode)
(defun disable-guru ()
  "Disable guru mode globally"
  (interactive)
  (guru-global-mode -1))

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-multiframe-window)

;; neotree
(neotree-show)
(setq neo-window-fixed-size nil)

;; python mode
(setenv "PYTHONPATH" default-directory)
(elpy-enable)
(pyvenv-activate "env")
(remove-hook 'elpy-modules 'elpy-module-flymake)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-mypy-ini "setup.cfg")
(flycheck-add-next-checker 'python-pycompile 'python-mypy t)
(add-hook 'before-save-hook 'flycheck-all-file-buffers)

;; term window
(if (not (boundp 'term-is-on))
    (progn
      (other-window 1)
      (split-window-vertically (floor (* 0.75 (window-height))))
      (other-window 1)
      (ansi-term "/bin/bash")
      (set-window-dedicated-p (selected-window) t)
      (other-window 2)
      (setq term-is-on t)
     ))
