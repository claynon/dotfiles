;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (neotree winum centered-cursor-mode diff-hl magit fuzzy auto-complete auto-highlight-symbol undo-tree bind-key mwim which-key fill-column-indicator solarized-theme smartparens projectile helm-projectile helm-swoop helm)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)
(require 'bind-key)

(bind-keys* ("M-x"     . helm-M-x)
	    ("C-c h"   . helm-command-prefix)
	    ("C-x C-f" . helm-find-files)
	    ("C-x b"   . helm-buffers-list)
	    ("C-x C-b" . switch-to-buffer))
(global-unset-key (kbd "C-x c"))
(which-key-add-key-based-replacements "C-c h" "Helm")

; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
; make TAB work in terminal
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
; list actions using C-z
(define-key helm-map (kbd "C-z")   'helm-select-action)

; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p           t
; move to end or beginning of source when reaching top or bottom of source.
      helm-move-to-line-cycle-in-source     t
; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t
      helm-M-x-fuzzy-match                  t
      helm-boring-file-regexp-list          '("~$")
      helm-ff-skip-boring-files             t
      helm-move-to-line-cycle-in-source     nil)

(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 30)
(helm-autoresize-mode)

(helm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELM SWOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-swoop)

(bind-keys* ("C-s" . helm-swoop)
	    ("C-r" . helm-swoop))
(define-key helm-command-map (kbd "C-s") 'helm-multi-swoop)
(setq helm-swoop-split-direction    'split-window-horizontally
      helm-swoop-move-to-line-cycle t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELM PROJECTILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-projectile)

(helm-projectile-on)
(which-key-add-key-based-replacements "C-c p" "Projectile")
(bind-key* "C-c p C-t" 'projectile-toggle-between-implementation-and-test)
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SMART PARENS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens)
(require 'smartparens-config)

(add-hook 'prog-mode-hook #'smartparens-mode)
(bind-keys :prefix-map smartparens-mode-map
	   :prefix "C-c l"
	   ("s" . sp-forward-slurp-sexp)
	   ("S" . sp-backward-slurp-sexp)
	   ("b" . sp-forward-barf-sexp)
	   ("B" . sp-backward-barf-sexp))
(which-key-add-key-based-replacements "C-c l" "Lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FILL COLUMN INDICATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fill-column-indicator)

(setq fci-rule-column 80
      fci-rule-width  2)
(add-hook 'prog-mode-hook #'fci-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; WHITESPACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)

(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MWIM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys* ("C-a"    . mwim-beginning-of-code-or-line)
	    ("<home>" . mwim-beginning-of-code-or-line)
	    ("C-e"    . mwim-end-of-code-or-line)
	    ("<end>"  . mwim-end-of-code-or-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; WHICH KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'which-key)

(bind-key* "M-m" 'which-key-show-major-mode)
(setq which-key-side-window-max-height 0.2
      which-key-idle-delay             0.1
      which-key-add-column-padding     0)
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; UNDO TREE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; AUTO HIGHLIGHT SYMBOL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ahs-idle-interval 0.2)
(global-auto-highlight-symbol-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; AUTO COMPLETE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-config-default)
(setq ac-auto-show-menu 0.2)
(setq ac-quick-help-delay 0.2)
(setq ac-use-fuzzy t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; DIFF HIGHLIGHT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CENTERED CURSOR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'centered-cursor-mode)
(global-centered-cursor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; WINUM  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'winum)

(winum-set-keymap-prefix (kbd "C-c w"))
(which-key-add-key-based-replacements "C-c w" "Windows")
(bind-keys* ("M-0" . winum-select-window-0-or-10)
	    ("M-1" . winum-select-window-1)
	    ("M-2" . winum-select-window-2)
	    ("M-3" . winum-select-window-3)
	    ("M-4" . winum-select-window-4)
	    ("M-5" . winum-select-window-5)
	    ("M-6" . winum-select-window-6)
	    ("M-7" . winum-select-window-7)
	    ("M-8" . winum-select-window-8)
	    ("M-9" . winum-select-window-9)
	    ("C-c w /" . split-window-horizontally)
	    ("C-c w -" . split-window-vertically)
	    ("C-c w =" . balance-windows)
	    ("C-c w u" . winner-undo)
	    ("C-c w r" . winner-redo))

(defun winum-assign-0-to-neotree ()
  (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
(add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
(setq winum-ignored-buffers '(" *which-key*"))

(winum-mode)
(winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; NEOTREE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))
(bind-key* "C-c p t" 'neotree-find-project-root)

(setq projectile-switch-project-action 'neotree-projectile-action
      neo-smart-open                   t
      neo-create-file-auto-open        t
      neo-window-width                 32
      ;; neo-theme                        (if (display-graphic-p) 'icons 'arrow)
      neo-banner-message               "Press ? for neotree help"
      neo-show-updir-line              nil
      neo-mode-line-type               'neotree
      neo-show-hidden-files            nil
      neo-auto-indent-point            t
      neo-vc-integration               '(face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; AUTO-SAVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-save-default           t
      auto-save-visited-file-name t
      auto-save-interval          20
      auto-save-timeout           1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MAGIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys :prefix-map magit-mode-map
	   :prefix     "C-c g"
	   ("a" . magit-stage-file)
	   ("c" . magit-commit)
	   ("d" . magit-diff)
	   ("p" . magit-push)
	   ("s" . magit-status)
	   ("u" . magit-unstage-file)
	   ("C-s l" . magit-stash-list)
	   ("C-s p" . magit-stash-popup)
	   ("C-s s" . magit-stash))
(which-key-add-key-based-replacements "C-c g" "Git")
(which-key-add-key-based-replacements "C-c g C-s" "stash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
  "C-c e" "Eval")
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c e b") 'eval-buffer)
	    (local-set-key (kbd "C-c e r") 'eval-buffer)))

;; mode line
(global-linum-mode)
(column-number-mode)
(size-indication-mode)
(display-battery-mode)
(display-time-mode)

;; Misc
(load-theme 'solarized-dark t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(save-place-mode)
(global-hl-line-mode)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)


;; barra inferior -- reddit share your mode-line customization
;; ;; numero da coluna
;; ;; nome do projeto?
;; ;; percentual da p'agina
;; clj
;; scala
;; autosave
;; belomonte thing
;; all-the-items
;; use package instead of require
