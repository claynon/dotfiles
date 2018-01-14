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
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (telephone-line all-the-icons company-quickhelp pos-tip exec-path-from-shell
     cider clj-refactor aggressive-indent rainbow-delimiters clojure-mode
     neotree winum centered-cursor-mode diff-hl magit fuzzy
     auto-highlight-symbol undo-tree bind-key mwim which-key
     fill-column-indicator solarized-theme smartparens markdown projectile
     helm-projectile helm-swoop helm)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)
(require 'bind-key)

(bind-keys* ("M-x"     . helm-M-x)
	    ("C-c h"   . helm-command-prefix)
	    ("C-x C-f" . helm-find-files)
	    ("C-x b"   . helm-buffers-list))
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
      projectile-switch-project-action      'projectile-find-file
      helm-move-to-line-cycle-in-source     nil)

(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 30)
(helm-autoresize-mode t)

(helm-mode t)

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
(bind-keys* :prefix-map helm-projectile-projects-map
	    :prefix "C-c p"
	    ("C-t" . projectile-toggle-between-implementation-and-test)
	    ("s"   . helm-projectile-grep))
(bind-key* "C-x C-b" 'projectile-switch-to-buffer)
(projectile-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SMART PARENS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens)
(require 'smartparens-config)

(add-hook 'prog-mode-hook #'smartparens-mode)
(bind-keys :prefix-map smartparens-mode-map
	   :prefix "C-c l"
	   ("s"   . sp-forward-slurp-sexp)
	   ("S"   . sp-backward-slurp-sexp)
	   ("b"   . sp-forward-barf-sexp)
	   ("B"   . sp-backward-barf-sexp)
	   ("k"   . sp-kill-sexp)
	   ("K"   . sp-backward-kill-sexp)
	   ("u"   . sp-up-sexp)
	   ("U"   . sp-backward-up-sexp)
	   ("C-u" . sp-unwrap-sexp)
	   ("r"   . sp-raise-sexp)
	   ("<right>" . ahs-forward)
	   ("<left>"  . ahs-backward))
(bind-key "C-(" `(lambda (&optional arg)
		   (interactive "P")
		   (sp-wrap-with-pair "(")))
(bind-key "ESC [" `(lambda (&optional arg)
		     (interactive "P")
		     (sp-wrap-with-pair "[")))
(bind-key "C-{" `(lambda (&optional arg)
		   (interactive "P")
		   (sp-wrap-with-pair "{")))
(bind-key "C-\"" `(lambda (&optional arg)
		    (interactive "P")
		    (sp-wrap-with-pair "\"")))
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
(global-whitespace-mode t)

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

(define-key which-key-mode-map (kbd "ESC C-h") 'which-key-C-h-dispatch)
(setq which-key-side-window-max-height 0.2
      which-key-idle-delay             0.1
      which-key-add-column-padding     0
      which-key-sort-order             'which-key-key-order-alpha)
(which-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; UNDO TREE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; AUTO HIGHLIGHT SYMBOL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ahs-idle-interval 0.2)
(global-auto-highlight-symbol-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; COMPANY QUICKHELP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-quickhelp)
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; DIFF HIGHLIGHT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-diff-hl-mode t)
(diff-hl-flydiff-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CENTERED CURSOR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'centered-cursor-mode)
(global-centered-cursor-mode t)

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
	    ("M-9" . winum-select-window-9))
(bind-keys* :prefix-map winum-base-map
	    :prefix "C-c w"
	    ("/" . split-window-horizontally)
	    ("-" . split-window-vertically)
	    ("=" . balance-windows)
	    ("u" . winner-undo)
	    ("r" . winner-redo))

(defun winum-assign-0-to-neotree ()
  (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
(add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
(setq winum-ignored-buffers '(" *which-key*"))

(winum-mode t)
(winner-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; NEOTREE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(require 'all-the-icons)

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))
(bind-key* "C-c p t" 'neotree-find-project-root)

(put 'car 'lisp-indent-function 0)
(put 'last 'lisp-indent-function 0)
(put 'butlast 'lisp-indent-function 0)
(put 'split-string 'lisp-indent-function 0)
(setq neo-smart-open                   t
      neo-create-file-auto-open        t
      neo-window-width                 (max (floor (* 0.1 (window-total-width)))
					    32)
      neo-theme                        (if (display-graphic-p) 'icons 'arrow)
      neo-banner-message               (concat "Project: "
					       (car
						 (last
						   (butlast
						     (split-string
						       (projectile-project-info)
						       "/")))))
      neo-show-updir-line              nil
      neo-mode-line-type               'neotree
      neo-show-hidden-files            nil
      neo-auto-indent-point            t
      neo-vc-integration               '(face))
(eval-after-load "neotree"
  '(defun neo-path--shorten (path len)
     (make-string len ?-)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; AUTO-SAVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default auto-save-default           t
	      auto-save-visited-file-name t
	      auto-save-interval          20
	      auto-save-timeout           1
	      create-lockfiles            nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EXEC PATH FROM SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq exec-path-from-shell-check-startup-files nil)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-envs '("NU_HOME"
                                  "NUCLI_HOME"
                                  "HOME"
				  "TERM"
                                  "AWS_DEFAULT_REGION"
                                  "AWS_ACCESS_KEY"
                                  "AWS_SECRET_ACCESS_KEY"
                                  "AWS_ACCESS_KEY_ID"
                                  "AWS_SECRET_KEY"
                                  "ARTIFACTS_AWS_ACCESS_KEY_ID"
                                  "ARTIFACTS_AWS_SECRET_ACCESS_KEY"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TELEPHONE MODE LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'telephone-line)

(setq telephone-line-lhs
      '((accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
(setq telephone-line-primary-left-separator    'telephone-line-gradient
      telephone-line-secondary-left-separator  'telephone-line-nil
      telephone-line-primary-right-separator   'telephone-line-gradient
      telephone-line-secondary-right-separator 'telephone-line-nil)

(setq display-time-24hr-format t)
(display-battery-mode t)
(display-time-mode    t)

(telephone-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MAGIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-keys :prefix-map magit-mode-map
	   :prefix     "C-c g"
	   ("a" . magit-stage-file)
	   ("c" . magit-commit)
	   ("d" . magit-diff)
	   ("p" . magit-pull)
	   ("P" . magit-push)
	   ("s" . magit-status)
	   ("u" . magit-unstage-file)
	   ("C-s l" . magit-stash-list)
	   ("C-s p" . magit-stash-popup)
	   ("C-s s" . magit-stash))
(which-key-add-key-based-replacements "C-c g" "Git")
(which-key-add-key-based-replacements "C-c g C-s" "stash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MAJOR MODEs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SHELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'term-mode-hook (lambda () (centered-cursor-mode nil)))
(bind-key "C-c s" `(lambda (&optional arg)
		     (interactive)
		     (ansi-term "/bin/bash")))
(which-key-add-key-based-replacements "C-c s" "shell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EMACS LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
  "C-c e" "Eval")
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode t)
	    (local-set-key (kbd "C-M-m b") 'eval-buffer)
	    (local-set-key (kbd "C-M-m r") 'eval-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; CLOJURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; clojure-mode
(require 'clojure-mode)

(define-clojure-indent
  (facts 1)
  (fact 1)
  (defrecord '(2 nil nil (1))))
(setq clojure-indent-style              :align-arguments
      clojure-align-forms-automatically t)
(defun my-clojure-mode-hook ()
  (subword-mode t)
  (smartparens-strict-mode t)
  (rainbow-delimiters-mode t)
  (auto-highlight-symbol-mode t))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;;;;;;;;; clj-refactor
(require 'clj-refactor)

(defun my-clj-refactor-hook ()
  (clj-refactor-mode t)
  (yas-minor-mode t)
  (cljr-add-keybindings-with-prefix "C-M-m r"))
(setq cljr-favor-prefix-notation nil)
(which-key-add-major-mode-key-based-replacements 'clojure-mode
  "C-M-m r" "Refactor")
(add-hook 'clojure-mode-hook #'my-clj-refactor-hook)


;;;;;;;;; cider
(defun set-clojure-keys (mode)
  (which-key-add-major-mode-key-based-replacements mode "C-M-m" "Clojure")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m d" "Docs")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m e" "Eval")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m g" "Go to")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r a" "Add")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r c" "Cycle")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r d" "Destruct")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r e" "Extract")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r h" "Hotload")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r m" "Move")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r r" "Rename")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r t" "Thread")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m r u" "Unwind")
  (which-key-add-major-mode-key-based-replacements mode "C-M-m s" "Repl")
  (add-hook (intern (concat (symbol-name mode) "-hook"))
	    (lambda ()
	      (local-set-key (kbd "C-M-m b") 'cider-load-buffer)
	      (local-set-key (kbd "C-M-m c") 'cider-repl-clear-buffer)

	      (local-set-key (kbd "C-M-m d a") 'cider-apropos)
	      (local-set-key (kbd "C-M-m d d") 'cider-doc)
	      (local-set-key (kbd "C-M-m d j") 'cider-javadoc)
	      (local-set-key (kbd "C-M-m d n") 'cider-browse-ns)

	      (local-set-key (kbd "C-M-m e b") 'cider-load-buffer)
	      (local-set-key (kbd "C-M-m e e") 'cider-eval-sexp-at-point)
	      (local-set-key (kbd "C-M-m e E") 'cider-insert-last-sexp-in-repl)
	      (local-set-key (kbd "C-M-m e n") 'cider-eval-ns-form)
	      (local-set-key (kbd "C-M-m e N") 'cider-insert-ns-form-in-repl)
	      (local-set-key (kbd "C-M-m e p") 'cider-pprint-eval-last-sexp)
	      (local-set-key (kbd "C-M-m e P") 'cider-eval-print-last-sexp)
	      (local-set-key (kbd "C-M-m e r") 'cider-eval-region)
	      (local-set-key (kbd "C-M-m e R") 'cider-insert-region-in-repl)

	      (local-set-key (kbd "C-M-m g b") 'cider-pop-back)
	      (local-set-key (kbd "C-M-m g g") 'cider-find-var)
	      (local-set-key (kbd "C-M-m g n") 'cider-find-ns)

	      (local-set-key (kbd "C-M-m s c") 'cider-connect)
	      (local-set-key (kbd "C-M-m s i") 'cider-jack-in)                  ;; cider-jack-in requires ipv6 to be enabled or change cider-lein-parameters to use localhost instead of ::
	      (local-set-key (kbd "C-M-m s I") 'cider-jack-in-clojurescript)
	      (local-set-key (kbd "C-M-m s s") 'cider-switch-to-repl-buffer)
	      (local-set-key (kbd "C-M-m s S")
			     'cider-switch-to-last-clojure-buffer)
	      (local-set-key (kbd "C-M-m s q") 'cider-quit))))

(setq cider-save-file-on-load nil)
(dolist (mode '(clojure-mode
		clojurec-mode
		clojurescript-mode
		clojurex-mode
		cider-repl-mode
		cider-clojure-interaction-mode))
  (set-clojure-keys mode))

;; Misc
(load-theme 'solarized-dark t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(save-place-mode t)
(global-hl-line-mode t)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)
(defun set-text-height (n)
  (interactive "nHeight (100): ")
  (set-face-attribute 'default nil
		      :height  n))
(bind-key "C-c t" 'set-text-height)
(bind-key* "C-c r" 'rename-buffer)

(bind-key "M-j" 'join-line)
(defalias 'yes-or-no-p 'y-or-n-p)
(desktop-save-mode t)

;; scala
;; Belomonte thing
;; figure out term-mode