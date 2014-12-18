;; remove the training wheels
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; use MELPA package repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; === Autoinstall Packages: ===
(require 'cl)

(defvar required-packages '(ack-and-a-half
    evil
    magit
    rainbow-mode
    rainbow-delimiters
    smartparens
    undo-tree
    tabbar
    sr-speedbar
    monokai-theme
    solarized-theme)
  "A list of packages to ensure are installed at launch.")

; method to check if all packages are installed
(defun packages-installed-p ()
    (loop for p in required-packages
                  when (not (package-installed-p p)) do (return nil)
                          finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
    ; check for new packages (package versions)
      (message "%s" "Emacs is now refreshing its package database...")
        (package-refresh-contents)
          (message "%s" " done.")
            ; install the missing packages
              (dolist (p required-packages)
                    (when (not (package-installed-p p))
                            (package-install p))))

;; === UI Customization: ===

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


(when window-system (set-frame-size (selected-frame) 120 45))


(require 'evil)
(evil-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-linum-mode t)
(ido-mode t)
(show-paren-mode 1)

(require 'smartparens-config)
(smartparens-global-mode t)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(windmove-default-keybindings)

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-less-bold t)
(setq solarized-use-more-italic t)

(load-theme 'monokai t)
(set-default-font "Inconsolata-12")

;;(require 'powerline)
;;(powerline-evil-vim-theme)

;; === Loading other config files: ===

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "lermex-speedbar.el")
(load-user-file "lermex-tabbar.el")
