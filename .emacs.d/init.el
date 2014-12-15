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

(defvar required-packages
  '(ack-and-a-half
    evil
    magit
    rainbow-mode
    undo-tree
    tabbar
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

(load-theme 'solarized-dark t)

(when window-system (set-frame-size (selected-frame) 120 45))

(require 'tabbar)
(tabbar-mode t)

(require 'evil)
(evil-mode 1)
