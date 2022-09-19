;;==========================================================
;;  _____       _     _       _       _       _ _         _ 
;; /  ___|     | |   (_)     ( )     (_)     (_) |       | |
;; \ `--.  __ _| |__  _ _ __ |/ ___   _ _ __  _| |_   ___| |
;;  `--. \/ _` | '_ \| | '_ \  / __| | | '_ \| | __| / _ \ |
;; /\__/ / (_| | |_) | | | | | \__ \ | | | | | | |_ |  __/ |
;; \____/ \__,_|_.__/|_|_| |_| |___/ |_|_| |_|_|\__(_)___|_|
;;
;;==========================================================                                                         


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; === use-package settings ===
;; ensure that every package is installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (auto-package-update-maybe))


;; === Themes ===
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)) 
 
;; (use-package monokai-theme
;;   :config
;;   (load-theme 'monokai t))

(use-package all-the-icons
  :if (display-graphic-p))

;; UI Config
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(setq show-paren-delay 0)
(show-paren-mode  1)
;; autocomplete paired brackets
(electric-pair-mode 1)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

;; Org Mode
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook
	    (lambda () (org-bullets-mode 1))))

;; indentation
(setq org-startup-indented t
      org-src-tab-acts-natively t)

;; Wrap the long lines
(global-visual-line-mode t)

;; Change the font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-19"))
(set-face-attribute 'default t :font "DejaVu Sans Mono-19")


;; === macOS Stuff ===
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq frame-resize-pixelwise t) ; fix the gap when full screen
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; === Company ===
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; === which-key ===
(use-package which-key
    :config
    (which-key-mode))

;; === VTerm ===
(use-package vterm)

;; === Eglot ===
(use-package eglot
  :config
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
  (setq eldoc-echo-area-use-multiline-p nil))

;; ~~~ fixing jdtls's '-data' argument ~~~
(with-eval-after-load 'eglot
  (let ((cache
         (expand-file-name (md5 (project-root (eglot--current-project)))
                           (locate-user-emacs-file
                            "eglot-eclipse-jdt-cache"))))
    (add-to-list 'eglot-server-programs
                 `(java-mode "jdtls" "-data" ,cache))))

;; === ivy ===
(use-package ivy
  :defer 0.1
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (setq ivy-initial-inputs-alist nil))

;; === counsel ===
(use-package counsel
  :after ivy
  :config
  (counsel-mode)
  (global-set-key (kbd "C-c c") 'counsel-compile)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-c w") 'counsel-wmctrl)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :after ivy
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-color-icon t))

;; === Treesitter ===
(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; === Flycheck ===
(use-package flycheck)

;; <3 <3 <3 Having fun with Alexia <3 <3 <3
(use-package fireplace)
(use-package snow)

;; === LSP Performance ===
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

;; === Projectile ===
(use-package projectile
   :config
   (projectile-mode +1))

;; === Ace-window ===
;; ace-window
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; === Native-comp ===


;; ===== END =====
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(snow fireplace use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
