(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

(use-package emacs
  :config
  (set-face-attribute 'default nil :height 190)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(undecorated . t))

  (defun enable-fill-column-indicator ()
    "Enable fill column indicator only in non-minibuffer windows."
    (unless (minibufferp)
      (display-fill-column-indicator-mode 1)))
  (add-hook 'prog-mode-hook 'enable-fill-column-indicator)
  (add-hook 'text-mode-hook 'enable-fill-column-indicator)
  (add-hook 'LaTeX-mode-hook 'enable-fill-column-indicator)

  (save-place-mode 1)
  (global-auto-revert-mode t)
  (global-display-line-numbers-mode 1)

  (defun my-conditional-after-save-hook ()
    "Run `my-custom-function` only in specific modes."
    (when (derived-mode-p 'emacs-lisp-mode 'markdown-mode 'LaTeX-mode
			  'fundamental-mode 'sh-mode 'text-mode 'python-mode
			  'sql-mode 'cmake-mode)
      (delete-trailing-whitespace)))
  (add-hook 'before-save-hook 'my-conditional-after-save-hook)

  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
  (add-hook 'emacs-startup-hook (lambda () (recenter)))
  (add-hook 'emacs-startup-hook (lambda () (other-frame 0)))
  ;; (make-variable-buffer-local 'register-alist)
  (set-register ?e '(file . "~/.emacs31.d/.emacs"))
  (set-register ?i '(file . "~/org/ideas.org"))
  (set-register ?t '(file . "~/org/todos.org"))
  (set-register ?z '(file . "~/.zshrc"))
  (set-register ?o '(file . "~/.emacs30.d/.emacs"))
  (set-register ?g '(file . "~/.emacs31.d/.emacs.d/elpa/gruber-darker-theme-20231026.2031/gruber-darker-theme.el"))
  (advice-add 'jump-to-register :after (lambda (&rest _) (recenter)))
  (advice-add 'bookmark-jump :after (lambda (&rest _) (recenter)))
  (delete-selection-mode 0)
  (global-set-key (kbd "C-/") 'undo-only)
  (scroll-bar-mode -1)
  ;; (transient-mark-mode -1)

  (defadvice message (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n)
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))
  (ad-disable-advice 'message 'before 'who-said-that)
  (ad-update 'message)
  (defadvice message (before when-was-that activate)
    "Add timestamps to `message' output."
    (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
                          (ad-get-arg 0)) ))

  (defun save-place-reposition ()
    "Force windows to recenter current line (with saved position)."
    (run-with-timer 0 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
			(dolist (win (get-buffer-window-list buf nil t))
                          (with-selected-window win (recenter)))))
                    (current-buffer)))
  (add-hook 'find-file-hook 'save-place-reposition t)

  (defvar my-isearch-window-start nil
    "Stores the window start position before starting `isearch`.")
  (defun my-isearch-save-position ()
    "Save the current window start position before `isearch`."
    (setq my-isearch-window-start (window-start)))
  (defun my-isearch-restore-position ()
    "Restore the saved window start position if `isearch` is unsuccessful."
    (when (and my-isearch-window-start (not isearch-success))
      (set-window-start (selected-window) my-isearch-window-start))
    (setq my-isearch-window-start nil))
  (add-hook 'isearch-mode-hook 'my-isearch-save-position)
  (add-hook 'isearch-mode-end-hook 'my-isearch-restore-position)

  (defun copy-region-or-lines (n &optional beg end)
    "Copy region or the next N lines into the kill ring.
  When called repeatedly, move to the next line and append it to
  the previous kill."
    (interactive "p")
    (let* ((repeatp (eq last-command 'copy-region-or-lines))
           (kill-command
            (if repeatp
                #'(lambda (b e)
		    (kill-append
		     (concat "\n" (buffer-substring b e)) nil))
              #'(lambda (b e) (kill-ring-save b e (use-region-p)))))
           beg
           end)
      (if repeatp
          (let ((goal-column (current-column)))
            (next-line)))
      (setq beg (or beg
                    (if (use-region-p)
                        (region-beginning)
                      (line-beginning-position))))
      (setq end (or end
                    (if (use-region-p)
                        (region-end)
                      (line-end-position n))))
      (funcall kill-command beg end)
      (if repeatp (message "%s" (car kill-ring)))
      ))
  (global-set-key (kbd "M-w") 'copy-region-or-lines)

  (defun my-isearch-recenter-if-needed ()
    "Recenter the screen if the next `isearch` match is outside the visible
window."
    (when (not (pos-visible-in-window-p (point)))
      (recenter)))
  (add-hook 'isearch-update-post-hook #'my-isearch-recenter-if-needed)

  (defun my-comment-line ()
    "Comment or uncomment the current line without moving to the next line."
    (interactive)
    (save-excursion
      (comment-line 1)))
  (global-set-key (kbd "C-x C-;") 'my-comment-line)

  (add-hook 'isearch-mode-hook
            (lambda ()
              (point-to-register ?r)))

  (defun smart-line-beginning ()
    "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
    (interactive)
    (let ((pt (point)))
      (beginning-of-line-text)
      (when (eq pt (point))
	(beginning-of-line))))
  (global-set-key (kbd "C-a") 'smart-line-beginning)

  (defun push-line-down ()
    (interactive)
    (when (region-active-p)
      (deactivate-mark))
    (beginning-of-line)
    (open-line 1)
    (save-excursion
      (next-line)
      (indent-for-tab-command))
    (indent-for-tab-command))
  (global-set-key (kbd "C-o") 'push-line-down)

  ;; (defun ret ()
  ;;   (interactive)
  ;;   (if (eq ?} (char-after))
  ;; 	(progn
  ;;         (newline-and-indent)
  ;; 	  (push-line-down)
  ;; 	  )
  ;;     (progn
  ;; 	(newline-and-indent)
  ;; 	)
  ;;     ))
  ;; (global-set-key (kbd "RET") 'ret)

  (dolist (command '(yank yank-pop))
    (eval
     `(defadvice ,command (after indent-region activate)
	(and (not current-prefix-arg)
             (member
              major-mode
              '(emacs-lisp-mode
		lisp-mode
		clojure-mode
		scheme-mode
		haskell-mode
		ruby-mode
		rspec-mode
		python-mode
		c-mode
		c++-mode
		objc-mode
		latex-mode
		js2-mode
		go-mode
		html-mode
		text-mode
		LaTeX-mode
		plain-tex-mode))
             (let ((mark-even-if-inactive transient-mark-mode))
               (indent-region (region-beginning) (region-end) nil))))))

  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq display-line-numbers-type 'relative)
  ;; (setq electric-indent-mode t)
  (setq-default fill-column 80)
  (setq column-number-mode t)
  ;; (setq make-backup-files nil)
  (setq backup-directory-alist `(("." . "~/.emacs31.d/backups"))
	backup-by-copying t
	delete-old-versions t
	version-control t)
  (setq ring-bell-function 'ignore)
  ;; (setq visible-bell t)  ;; Makes the screen flash instead of beeping
  (setq case-fold-search nil)
  (setq scroll-preserve-screen-position 'always)
  (setq scroll-conservatively 101)
  (setq next-screen-context-lines 1)
  (setq isearch-allow-scroll t)
  (setq-default auto-fill-function 'do-auto-fill)
  ;; (setq sentence-end-double-space nil)
  (setq compile-command "make run")

  :bind
  (
   ("C-; r" . read-only-mode)
   ("C-c ;" . comment-or-uncomment-region)
   ("C-s-<backspace>" . kill-whole-line)
   )

  )

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package company
  :hook ((prog-mode text-mode LaTeX-mode))
  :config
  ;; (setq company-idle-delay
  ;; 	(lambda () (if (company-in-string-or-comment) nil 0)))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-inhibit-inside-symbols t)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-format-margin-function #'company-text-icons-margin)
  (setq company-text-face-extra-attributes
	'(:weight bold :slant italic))
  (setq company-text-icons-add-background t)
  (setq company-tooltip-offset-display 'lines)
  )

;; (use-package company-box
;;   :hook (company-mode)
;;   :config
;;   )

(use-package python-black
  :hook (python-mode . python-black-on-save-mode)
  ;; :hook (python-mode . (lambda ()
  ;; 			 (setq tab-width 4)
  ;; 			 (setq python-indent-offset 4)))
  )

(use-package lsp-mode
  :hook ((c-mode c++-mode go-mode))
  ;; :hook ((go-mode))
  :hook (lsp-mode . (lambda ()
		      (add-hook #'before-save-hook
				#'lsp-format-buffer t t)))
  :hook (lsp-mode . (lambda ()
		      (add-hook #'before-save-hook
				#'lsp-organize-imports t t)))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  (setf lsp-session-folders-blacklist nil)

  (defun my-recenter-if-outside-visible (orig-fun &rest args)
    "Recenter screen if `xref-find-definitions`
jumps to an out-of-view location."
    (let ((prev-pos (point)))  ; Store the current position
      (apply orig-fun args)    ; Call the original `xref-find-definitions`
      (unless (pos-visible-in-window-p)  ; Check if new position is visible
	(recenter))))           ; Recenter if it’s not visible
  (advice-add 'xref-find-definitions :around #'my-recenter-if-outside-visible)
  (advice-add 'xref-go-back :around #'my-recenter-if-outside-visible)

  (add-hook
   'c-mode-common-hook
   (lambda ()
     (setq tab-width 8)
     (setq c-basic-offset 8)
     (setq indent-tabs-mode t)))
  (add-hook 'c-mode-common-hook 'lsp-mode)

  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'compile)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'compile)))
  )

(use-package yasnippet)

(use-package cmake-mode)

(use-package lsp-ui
  ;; :hook (LaTeX-mode)
  :config
  (setq lsp-ui-peek-enable t)
  ;; (setq lsp-ui-peek-show-directory t)
  (setq lsp-ui-doc-enable t)
  ;; (setq lsp-ui-doc-delay 0)
  (setq lsp-ui-sideline-show-diagnostics t)
  ;; (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-sideline-delay 0)
  ;; (setq lsp-clients-clangd-args '("-Wall" "-Wunused"))
  ;; (custom-set-faces
  ;;  '(lsp-ui-sideline-global ((t (:foreground "orange" :weight bold)))))
  )

(use-package flycheck
  :hook (lsp-mode)
  :config
  (setq flycheck-gcc-warnings '("all" "unused"))
  (setq flycheck-clang-warnings '("all" "unused"))
  )

(use-package helm-lsp
  :config
  (helm-mode)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)
  (setq helm-mode-line-string "")
  ;; (setq helm-describe-variable-string "")
  )

(use-package which-key
  :config
  (which-key-mode)
  )

;; ;; (use-package dap-mode
;; ;;   :hook (lsp-mode)
;; ;;   )

;; (use-package lsp-treemacs
;;   )

;; (use-package lsp-docker
;;   )

(use-package auctex
  :hook (LaTeX-mode . (lambda ()
                        (setq TeX-auto-save t)
                        (setq TeX-parse-self t)))
  :hook (LaTeX-mode . flyspell-mode)
  :config
  (setq LaTeX-fill-column 80)
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
  (setq LaTeX-fill-break-before-code-comments t)
  )

(use-package js2-mode
  :hook (js-mode . js2-minor-mode)
  )

(use-package prettier
  :hook (js2-minor-mode)
  )

(use-package go-mode
  :hook (go-mode . (lambda ()
		     (setq tab-width 4)
		     (setq indent-tabs-mode t)))
  )

(use-package smartparens
  :hook ((prog-mode text-mode LaTeX-mode fundamental-mode markdown-mode))
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-with-modes '(malabar-mode c++-mode java-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))

  (sp-with-modes '(js2-mode typescript-mode java-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               ("* ||\n[i]" "RET"))))
  ;; :bind ("<backspace>" . sp-backward-delete-char)
  ;; :bind ("C-d" . sp-delete-char)
  )

;; (use-package rainbow-mode
;;   :hook ((prog-mode text-mode)))

(use-package minions
  :config
  (minions-mode 1)
  )

;; (use-package activities
;;   :init
;;   (activities-mode 1)
;;   ;; (activities-tabs-mode)
;;   ;; Prevent `edebug' default bindings from interfering.
;;   (setq edebug-inhibit-emacs-lisp-mode-bindings t)

;;   :bind
;;   (
;;   ;; (("C-c C-a C-n" . activities-new)
;;    ("C-; C-d" . activities-define)
;;    ("C-; C-a" . activities-resume)
;;    ("C-; C-s" . activities-suspend)
;;    ("C-; C-k" . activities-discard)
;;    ("C-; RET" . activities-switch)
;;    ("C-; b" . activities-switch-buffer)
;;    ("C-; g" . activities-revert)
;;    ("C-; l" . activities-list)
;;    )
;;   )

(use-package hungry-delete
  ;; :hook ((prog-mode text-mode))
  :config
  (setq hungry-delete-join-reluctantly t)
  :bind ("s-<backspace>" . hungry-delete-backward)
  )

(use-package syntax-subword
  :bind
  (("M-f" . syntax-subword-forward)
   ("M-b" . syntax-subword-backward)
   ("C-<backspace>" . sp-backward-kill-word)
   ("M-d" . syntax-subword-kill)
   )
  )

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  )

;; ;; (use-package highlight-symbol
;; ;;   :hook ((prog-mode text-mode LaTeX-mode)
;; ;; 	 . highlight-symbol-mode)
;; ;;   :hook ((prog-mode text-mode LaTeX-mode)
;; ;; 	 . highlight-symbol-nav-mode)
;; ;;   :config
;; ;;   (setq highlight-symbol-idle-delay 0)
;; ;;   (defun my-highlight-recenter-if-needed ()
;; ;;     "Recenter the screen if the next `highlight` match is outside the visible
;; ;; window."
;; ;;     (when (not (pos-visible-in-window-p (point)))
;; ;;       (recenter)))
;; ;;   (advice-add 'highlight-symbol-next :after 'my-highlight-recenter-if-needed)
;; ;;   (advice-add 'highlight-symbol-prev :after 'my-highlight-recenter-if-needed)
;; ;;   (custom-set-faces
;; ;;    '(highlight-symbol-face ((t (:background "#c73c3f")))))
;; ;;   )

(use-package disable-mouse
  :config
  (global-disable-mouse-mode))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
