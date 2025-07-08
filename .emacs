;;; ~/.emacs            -*- lexical-binding: t; -*-

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

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
  (add-hook 'emacs-startup-hook (lambda () (recenter)))
  (add-hook 'emacs-startup-hook (lambda () (other-frame 0)))
  ;; (make-variable-buffer-local 'register-alist)
  (set-register ?e '(file . "~/dotemacs/.emacs"))
  (set-register ?i '(file . "~/org/ideas.org"))
  (set-register ?t '(file . "~/org/todos.org"))
  (set-register ?z '(file . "~/.zshrc"))
  (set-register ?g '(file . "~/.emacs.d/elpa/gruber-darker-theme-20231026.2031/gruber-darker-theme.el"))
  (advice-add 'jump-to-register :after (lambda (&rest _) (recenter)))
  (advice-add 'bookmark-jump :after (lambda (&rest _) (recenter)))
  (delete-selection-mode 0)
  (global-set-key (kbd "C-/") 'undo-only)
  (scroll-bar-mode -1)
  ;; (transient-mark-mode -1)

  ;; (defadvice message (before who-said-that activate)
  ;;   "Find out who said that thing. and say so."
  ;;   (let ((trace nil) (n 1) (frame nil))
  ;;     (while (setq frame (backtrace-frame n))
  ;;       (setq n     (1+ n)
  ;;             trace (cons (cadr frame) trace)) )
  ;;     (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
  ;;     (ad-set-args 1 (cons trace (ad-get-args 1))) ))
  ;; (ad-disable-advice 'message 'before 'who-said-that)
  ;; (ad-update 'message)
  ;; (defadvice message (before when-was-that activate)
  ;;   "Add timestamps to `message' output."
  ;;   (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
  ;;                         (ad-get-arg 0)) ))

  (defun who-said-that--filter (args)
    (let ((trace nil) (n 1) frame)
      (while (setq frame (backtrace-frame n))
	(setq n (1+ n)
              trace (cons (cadr frame) trace)))
      (setcar args (concat "<<%S>>:\n" (car args)))
      (setf (nth 1 args) (cons trace (nth 1 args)))
      args))
  (advice-add 'message :filter-args #'who-said-that--filter)
  (advice-remove 'message #'who-said-that--filter)

  (defun when-was-that--filter (args)
    (setcar args (concat (format-time-string "[%Y-%m-%d %T %Z] ") (car args)))
    args)
  (advice-add 'message :filter-args #'when-was-that--filter)

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
  (setq visible-bell t)  ;; Makes the screen flash instead of beeping
  (setq-default case-fold-search nil)

  (setq scroll-preserve-screen-position 'always)
  (setq scroll-conservatively 101)
  ;; (setq next-screen-context-lines 1)

  (setq isearch-allow-scroll t)
  (setq-default auto-fill-function 'do-auto-fill)
  ;; (setq sentence-end-double-space nil)
  ;; (setq compile-command "make run")

  (defun disable-auto-fill-mode-in-specific-modes ()
    (if (derived-mode-p 'sh-mode)  ; Replace with modes where you want to disable auto-fill
	(auto-fill-mode -1)))
  (add-hook 'text-mode-hook 'disable-auto-fill-mode-in-specific-modes)
  (add-hook 'prog-mode-hook 'disable-auto-fill-mode-in-specific-modes)

  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  ;; (setq user-emacs-directory (expand-file-name "~/emacs-config/"))

  ;;; scroll up and down buffer by one line
  (global-set-key (kbd "M-n") 'scroll-up-line)
  (global-set-key (kbd "M-p") 'scroll-down-line)

  ;; (set-frame-font "-*-Space Mono-regular-normal-normal-*-19-*-*-*-m-0-iso10646-1" t t)
  (set-frame-font "-*-Space Mono for Powerline-regular-normal-normal-*-19-*-*-*-m-0-iso10646-1" t t)
  (add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (defun my/dired-jump-and-recenter (&optional arg)
    "Run `dired-jump` (C-x C-j) then recenter the window (C-l behavior)."
    (interactive "P")
    (dired-jump arg)
    (recenter-top-bottom))
  ;; Override the default C-x C-j
  (global-set-key (kbd "C-x C-j") #'my/dired-jump-and-recenter)

  :bind
  (
   ("C-c ;" . comment-or-uncomment-region)
   ("C-s-<backspace>" . kill-whole-line)
   )
  )

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))        ;; Use Orderless for completion filtering [oai_citation_attribution:4‡kristofferbalintona.me](https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/#:~:text=%E2%80%A6%20%2A%20any%20built,company)
  (completion-category-defaults nil)            ;; No default filtering per category (allow Orderless in all contexts)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  )

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling through candidates (wrap-around)
  (corfu-auto t)                 ;; Enable automatic completion
  (corfu-auto-delay 0)           ;; No delay for auto-completion (show suggestions immediately)
  (corfu-auto-prefix 2)          ;; Pop up after typing 2 characters (for responsiveness)
  (corfu-scroll-margin 5)        ;; Use scroll margin when navigating candidates
  ;; Orderless integration
  (corfu-separator ?\s)          ;; Use space as separator for Orderless components [oai_citation_attribution:7‡github.com](https://github.com/minad/corfu#:~:text=,packages%20via%20margin%20formatter%20functions)
  (corfu-quit-at-boundary t)   ;; Don't quit at word boundary, allow space to continue completion
  (corfu-quit-no-match 'separator) ;; Don't quit if input is non-matching *but* has a separator (allow multiple terms)
  ;; UI behavior
  (corfu-preview-current nil)    ;; Disable inline preview of current candidate
  (corfu-preselect 'first)       ;; Preselect first candidate (or use 'prompt for none)
  (corfu-echo-documentation 0.25) ;; Briefly show documentation in echo area after 0.25s
  :bind (:map corfu-map
              ("TAB" . corfu-next)    ;; Use TAB to go to next candidate
              ("S-TAB" . corfu-previous))  ;; Use Shift+TAB to go to previous candidate
  :init
  (global-corfu-mode 1)          ;; Enable Corfu globally in all buffers [oai_citation_attribution:8‡github.com](https://github.com/minad/corfu#:~:text=%3B%3B%20Recommended%3A%20Enable%20Corfu%20globally,mode)
  :config
  (corfu-history-mode 1)         ;; Enable history for M-x corfu-history (save selection order)
  (corfu-popupinfo-mode 1);; Enable documentation popup (like corfu-doc, built-in)
  (with-eval-after-load 'corfu
    (add-hook 'lisp-interaction-mode-hook (lambda () (corfu-mode -1)))
    (add-hook 'LaTeX-mode-hook          (lambda () (corfu-mode -1))))
  )

(use-package cape
  :ensure t
  :init
  ;; Add helpful defaults to the global completion-at-point-functions list
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

;; (use-package company
;;   :hook ((prog-mode text-mode LaTeX-mode))
;;   :config
;;   ;; (setq company-idle-delay
;;   ;; 	(lambda () (if (company-in-string-or-comment) nil 0)))
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 3)
;;   (setq company-inhibit-inside-symbols t)
;;   (setq company-selection-wrap-around t)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-format-margin-function #'company-text-icons-margin)
;;   (setq company-text-face-extra-attributes
;; 	'(:weight bold :slant italic))
;;   (setq company-text-icons-add-background t)
;;   (setq company-tooltip-offset-display 'lines)
;;   )

;; (use-package company-box
;;   :hook (company-mode)
;;   :config
;;   )

;; (use-package python-black
;;   :hook (python-mode . python-black-on-save-mode)
;;   ;; :hook (python-mode . (lambda ()
;;   ;; 			 (setq tab-width 4)
;;   ;; 			 (setq python-indent-offset 4)))
;;   )

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
	 (c-mode-common . eglot-ensure)
	 (c-mode-common . (lambda ()
			    (setq tab-width 8)
			    (setq c-basic-offset 8)
			    (setq indent-tabs-mode t))))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  ;; (add-hook 'before-save-hook 'eglot-format-buffer)


  (defun my-eglot-managed-mode-hook ()
    "Add or remove eglot-format-buffer from the local before-save-hook based on eglot-managed-mode."
    (if eglot--managed-mode
	;; When eglot is enabled, add eglot-format-buffer locally.
	(add-hook 'before-save-hook 'eglot-format-buffer nil t)
      ;; Otherwise, remove it if present.
      (remove-hook 'before-save-hook 'eglot-format-buffer t)))
  (add-hook 'eglot-managed-mode-hook 'my-eglot-managed-mode-hook)

  (defun my-recenter-if-outside-visible (orig-fun &rest args)
    "Recenter screen if `xref-find-definitions`
  jumps to an out-of-view location."
    (let ((prev-pos (point)))  ; Store the current position
      (apply orig-fun args)    ; Call the original `xref-find-definitions`
      (unless (pos-visible-in-window-p)  ; Check if new position is visible
	(recenter))))           ; Recenter if it’s not visible
  (advice-add 'xref-find-definitions :around #'my-recenter-if-outside-visible)
  (advice-add 'xref-go-back :around #'my-recenter-if-outside-visible)
  )

;; (use-package lsp-mode
;;   :hook ((c-mode c++-mode go-mode))
;;   ;; :hook ((go-mode))
;;   :hook (lsp-mode . (lambda ()
;; 		      (add-hook #'before-save-hook
;; 				#'lsp-format-buffer t t)))
;;   :hook (lsp-mode . (lambda ()
;; 		      (add-hook #'before-save-hook
;; 				#'lsp-organize-imports t t)))
;;   :config
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-enable-snippet nil)
;;   (setf lsp-session-folders-blacklist nil)

;;   (defun my-recenter-if-outside-visible (orig-fun &rest args)
;;     "Recenter screen if `xref-find-definitions`
;; jumps to an out-of-view location."
;;     (let ((prev-pos (point)))  ; Store the current position
;;       (apply orig-fun args)    ; Call the original `xref-find-definitions`
;;       (unless (pos-visible-in-window-p)  ; Check if new position is visible
;; 	(recenter))))           ; Recenter if it’s not visible
;;   (advice-add 'xref-find-definitions :around #'my-recenter-if-outside-visible)
;;   (advice-add 'xref-go-back :around #'my-recenter-if-outside-visible)

;;   (add-hook
;;    'c-mode-common-hook
;;    (lambda ()
;;      (setq tab-width 8)
;;      (setq c-basic-offset 8)
;;      (setq indent-tabs-mode t)))
;;   (add-hook 'c-mode-common-hook 'lsp-mode)

;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c C-c") 'compile)))
;;   (add-hook 'c++-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c C-c") 'compile)))
;;   )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package cmake-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package ivy
  :ensure t
  :diminish
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode 1))

(use-package smex
  :ensure t
  :after ivy
  :bind ("M-x" . smex)
  :init (smex-initialize))

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package auctex
  :defer t
  :hook (LaTeX-mode . (lambda ()
                        (setq TeX-auto-save t)
                        (setq TeX-parse-self t)))
  :hook (LaTeX-mode . flyspell-mode)
  :config
  (setq LaTeX-fill-column 80)
  ;; (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
  (setq LaTeX-fill-break-before-code-comments t)
  ;; Add "-shell-escape" to pdflatex command in AUCTeX
  (with-eval-after-load "tex"
    (add-to-list 'TeX-command-list
                 '("LaTeX with shell escape"
                   "pdflatex -shell-escape %t"
                   TeX-run-command nil t)))
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
   ("C-<backspace>" . syntax-subword-backward-kill)
   ("M-d" . syntax-subword-kill)
   )
  )

;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize)
;;   )

;; (use-package highlight-symbol
;;   :hook ((prog-mode text-mode LaTeX-mode)
;; 	 . highlight-symbol-mode)
;;   :hook ((prog-mode text-mode LaTeX-mode)
;; 	 . highlight-symbol-nav-mode)
;;   :config
;;   (setq highlight-symbol-idle-delay 0)
;;   (defun my-highlight-recenter-if-needed ()
;;     "Recenter the screen if the next `highlight` match is outside the visible
;; window."
;;     (when (not (pos-visible-in-window-p (point)))
;;       (recenter)))
;;   (advice-add 'highlight-symbol-next :after 'my-highlight-recenter-if-needed)
;;   (advice-add 'highlight-symbol-prev :after 'my-highlight-recenter-if-needed)
;;   (custom-set-faces
;;    '(highlight-symbol-face ((t (:background "#c73c3f")))))
;;   )

(use-package disable-mouse
  :config
  (global-disable-mouse-mode)
  )

(use-package markdown-mode)

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex auto-package-update cape cmake-mode corfu counsel disable-mouse
	    docker-compose-mode dockerfile-mode go-mode gruber-darker-theme
	    hungry-delete js2-mode markdown-mode minions orderless prettier
	    smartparens smex syntax-subword yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
