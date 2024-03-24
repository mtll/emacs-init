;;; -*- lexical-binding: t; eval: (outline-minor-mode 1); -*-

;;; Elpaca

;; (setq elpaca-core-date
;;       (list (string-to-number (format-time-string "%Y%m%d" emacs-build-time))))
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;; Built-in

;;;; emacs

(setq scroll-preserve-screen-position t
      fill-column 72
      use-short-answers t
      y-or-n-p-use-read-key t
      xref-search-program 'ripgrep
      read-process-output-max (* 1024 1024)
      scroll-error-top-bottom t
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      delete-by-moving-to-trash t
      pulse-iterations 16
      xref-history-storage 'xref-window-local-history
      xref-prompt-for-identifier nil
      set-mark-command-repeat-pop t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      translate-upper-case-key-bindings nil
      show-paren-context-when-offscreen 'child-frame
      sentence-end-double-space t
      tab-always-indent 'complete
      read-minibuffer-restore-windows nil
      dired-listing-switches "-alFh --dired --group-directories-first"
      enable-recursive-minibuffers t
      ediff-split-window-function #'ediff-split-fn
      uniquify-buffer-name-style 'post-forward
      uniquify-separator " | "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      global-mark-ring-max 32
      mark-ring-max 32
      undo-limit 8000000
      undo-strong-limit 16000000
      undo-outer-limit 32000000
      read-process-output-max (ash 1 18)
      mouse-wheel-scroll-amount '(0.33 ((shift) . hscroll)
                                       ((meta))
                                       ((control meta) . global-text-scale)
                                       ((control) . text-scale))
      mouse-wheel-progressive-speed nil
      register-separator ?+
      history-delete-duplicates t
      disabled-command-function nil
      switch-to-buffer-obey-display-actions t
      minibuffer-prompt-properties '(read-only t
                                               cursor-intangible t
                                               face minibuffer-prompt)
      exec-path (cons (expand-file-name "scripts/" user-emacs-directory) exec-path)
      edebug-inhibit-emacs-lisp-mode-bindings t
      bidi-inhibit-bpa t
      cycle-spacing-actions '(just-one-space
                              delete-all-space
                              delete-space-after
                              delete-space-before
                              restore))

(setq-default indent-tabs-mode nil)

(minibuffer-depth-indicate-mode 1)
(global-goto-address-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(electric-pair-mode 1)
(undelete-frame-mode 1)
(context-menu-mode 1)

(define-key global-map [remap yank] #'yank-in-context)

(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(keymap-global-set "C-z"             #'exit-recursive-edit)
(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "C-M-<return>"    #'default-indent-new-line)
(keymap-global-set "S-<backspace>"   #'cycle-spacing)
(keymap-global-set "M-N"             #'tab-bar-switch-to-next-tab)
(keymap-global-set "M-P"             #'tab-bar-switch-to-prev-tab)
(keymap-global-set "C-:"             #'read-only-mode)
(keymap-global-set "C-c e"           #'eshell)
(keymap-global-set "C-x C-b"         #'ibuffer)
(keymap-global-set "M-;"             #'comment-line)
(keymap-global-set "C-c c"           #'compile)
(keymap-global-set "C-S-w"           #'delete-region)
(keymap-global-set "C-S-o"           #'other-window)
(keymap-global-set "<f2>"            #'other-window)

(put 'other-window 'repeat-map nil)

(keymap-set help-map "M-k" #'describe-keymap)

(define-keymap
  :keymap goto-map
  "," #'xref-go-back
  "." #'xref-go-forward)

(defvar-keymap xref-go-back-repeat-map
  :repeat t
  "," #'xref-go-back
  "." #'xref-go-forward)

(define-keymap
  :keymap ctl-x-map
  "s"   #'save-buffer
  "C-s" #'save-some-buffers
  "f"   #'find-file
  "C-f" #'set-fill-column)

(define-keymap
  :keymap window-prefix-map
  "t" #'tab-detach
  "f" #'tear-off-window)

(define-keymap
  :keymap emacs-lisp-mode-map
  "C-c C-z" #'eval-buffer
  "C-c C-m" #'emacs-lisp-macroexpand
  "C-c C-e" #'eval-print-last-sexp
  "C-c C-f" #'find-function
  "C-c C-l" #'pp-eval-last-sexp
  "C-c C-r" #'eval-region)

(defun my/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode 'my/show-trailing-whitespace)

(defun my/quit-other-window-for-scrolling ()
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))
(keymap-global-set "C-M-S-u" 'my/quit-other-window-for-scrolling)
(keymap-global-set "C-M-S-i" #'scroll-other-window-down)
(keymap-global-set "C-M-S-k" #'scroll-other-window)
(keymap-global-set "C-M-S-." #'end-of-buffer-other-window)
(keymap-global-set "C-M-S-," #'beginning-of-buffer-other-window)

(defun my/select-other-window-for-scrolling ()
  (interactive)
  (when-let ((win (other-window-for-scrolling)))
    (select-window win)))
(keymap-global-set "C-M-S-o" #'my/select-other-window-for-scrolling)

(defun kill-frame-and-buffer ()
  (interactive)
  (when-let ((buf (window-buffer (frame-root-window))))
    (kill-buffer buf))
  (delete-frame))
(keymap-set ctl-x-5-map "k" #'kill-frame-and-buffer)

(defun disable-minibuffer-max-height (fn &rest args)
  (let ((max-mini-window-height 1.0))
    (apply fn args)))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun ediff-split-fn ()
  (if (> (frame-width) 150)
      'split-window-horizontally
    'split-window-vertically))

(find-function-setup-keys)

(defun my/forward-page ()
  (interactive)
  (when (looking-at page-delimiter)
    (forward-char))
  (forward-page)
  (beginning-of-line))

(defun my/backward-page ()
  (interactive)
  (when (looking-at page-delimiter)
    (forward-char))
  (backward-page)
  (beginning-of-line))


;;;; kmacro

(require 'kmacro)
(keymap-global-set "C-x m" 'kmacro-keymap)

(define-keymap
  :keymap kmacro-keymap
  "a" 'kmacro-add-counter
  "s" 'kmacro-set-counter
  "S" 'kmacro-start-macro)


;;;; view-mode

(setq view-read-only t)


;;;; outline-minor-mode

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

(with-eval-after-load 'outline
  (keymap-set outline-minor-mode-map "C-c u"
              (make-composed-keymap (list outline-editing-repeat-map
                                          outline-navigation-repeat-map)))

  (pcase-dolist (`(,_ . ,def) (cdr outline-navigation-repeat-map))
    (put def 'repeat-map nil))

  (defun narrow-to-heading ()
    (interactive)
    (save-mark-and-excursion
      (outline-mark-subtree)
      ;; Must be called interactively if we want zones.el to add it.
      (funcall-interactively #'narrow-to-region (region-beginning) (region-end))))
  (keymap-set outline-minor-mode-map "C-x n h" #'narrow-to-heading))


;;;; line numbers

(require 'display-line-numbers)

(face-spec-set 'line-number-current-line '((t :background "#ccdee3")))

(setq-default display-line-numbers-width 2)
(setq display-line-numbers-width-start nil
      display-line-numbers-grow-only nil
      display-line-numbers-type 'visual
      display-line-numbers-current-absolute nil
      display-line-numbers-major-tick 0)


;;;; dictionary

(require 'dictionary)
(setq dictionary-server "localhost")
(keymap-global-set "C-c t d" #'dictionary-lookup-definition)


;;;; isearch

(setq isearch-lazy-count t)

(defun isearch-kill-region (&optional arg)
  (interactive "P")
  (isearch-done)
  (if arg
      (delete-region (region-beginning) (region-end))
    (kill-region (region-beginning) (region-end))))
(keymap-set isearch-mode-map "C-w" 'isearch-kill-region)

(defun isearch-escapable-split-on-char (string char)
  "Split STRING on CHAR, which can be escaped with backslash."
  (let ((quoted (concat "\\" char)))
    (mapcar
     (lambda (piece) (replace-regexp-in-string (string 0) char piece))
     (split-string (replace-regexp-in-string
                    (concat "\\\\\\" (substring quoted 0 (1- (length quoted)))
                            "\\|\\\\" quoted)
                    (lambda (x) (if (equal x quoted) (string 0) x))
                    string 'fixedcase 'literal)
                   (concat quoted "+")))))

(defun isearch-wildcards-compile (string &optional lax)
  (string-join
   (mapcar (lambda (string)
             (string-join
              (mapcar (lambda (string)
                        (string-join
                         (mapcar (lambda (string)
                                   (regexp-quote string))
                                 (isearch-escapable-split-on-char string ","))
                         ".+?"))
                      (isearch-escapable-split-on-char string "&"))
              "\\(?:\\s_\\|\\w\\)*?"))
           (isearch-escapable-split-on-char string " "))
   search-whitespace-regexp))

(isearch-define-mode-toggle wildcards "*" isearch-wildcards-compile "\
Turning on wildcards turns off regexp mode.")
(put 'isearch-wildcards-compile 'isearch-message-prefix
     (propertize "Wildcard " 'face 'minibuffer-prompt))

(with-eval-after-load 'isearch+
  (defun isearch-forward-wildcard (&optional arg no-recursive-edit)
    "Do incremental search forward.
See command `isearch-forward' for more information."
    (interactive "P\np")
    (let ((numarg  (prefix-numeric-value arg)))
      (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
             (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
            ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
             (call-interactively #'multi-isearch-buffers))
            (t (isearch-mode t (not (null arg)) nil (not no-recursive-edit)
                             #'isearch-wildcards-compile)))))
  (define-key global-map [remap isearch-forward] 'isearch-forward-wildcard)

  (defun isearch-backward-wildcard (&optional arg no-recursive-edit)
    "do incremental search backward.
see command `isearch-forward' for more information."
    (interactive "p\np")
    (let ((numarg  (prefix-numeric-value arg)))
      (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
             (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
            ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
             (call-interactively #'multi-isearch-buffers))
            (t (isearch-mode nil (not (null arg)) nil (not no-recursive-edit)
                             #'isearch-wildcards-compile)))))
  (define-key global-map [remap isearch-backward] 'isearch-backward-wildcard))

(defun isearch-repeat-direction ()
  (interactive)
  (if isearch-forward
      (isearch-repeat-forward)
    (isearch-repeat-backward)))
(keymap-set isearch-mode-map "TAB" 'isearch-repeat-direction)
(keymap-set isearch-mode-map "<tab>" 'isearch-repeat-direction)

(defun isearch-change-direction ()
  (interactive)
  (if isearch-forward
      (isearch-repeat-backward)
    (isearch-repeat-forward)))
(keymap-set isearch-mode-map "M-TAB" 'isearch-change-direction)
(keymap-set isearch-mode-map "M-<tab>" 'isearch-change-direction)


;;;; tab-bar-mode

(setq tab-bar-show nil
      tab-bar-tab-name-function 'tab-bar-tab-name-all)

(tab-bar-mode 1)
(tab-bar-history-mode 1)


;;;; diary / calendar

(keymap-global-set "<f5>" #'calendar)
(setq diary-entry-marker 'highlight
      calendar-holiday-marker 'match)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)


;;;; cc-mode

(with-eval-after-load 'cc-mode
  (define-keymap
    :keymap c-mode-base-map
    "<tab>" 'c-indent-then-complete
    "RET" 'newline-and-indent)

  (defun c-indent-then-complete ()
    (interactive)
    (let ((tick (buffer-modified-tick)))
      (c-indent-line-or-region)
      (when (= tick (buffer-modified-tick))
        (completion-at-point))))

  (setq c-hanging-semi&comma-criteria 'set-from-style))


;;;; doc-view

(setq doc-view-resolution 196)


;;;; ispell

(if (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
  (setq ispell-program-name "aspell"))

(setq ispell-dictionary "american")


;;;; savehist

(with-eval-after-load 'no-littering
  (require 'savehist)

  (setq savehist-additional-variables '(projectile-project-command-history
                                        search-ring
                                        regexp-search-ring
                                        register-alist)
        savehist-file (expand-file-name "var/savehist/hist" user-emacs-directory))

  (savehist-mode 1))


;;;; repeat

(setq repeat-exit-timeout nil
      repeat-keep-prefix nil
      repeat-on-final-keystroke t)

(repeat-mode 1)

(keymap-global-set "C-x c" 'repeat)


;;;; autorevert

(setq auto-revert-interval .01)
(global-auto-revert-mode 1)
(with-eval-after-load 'diminish
  (diminish 'auto-revert-mode))


;;;; face-remap

(with-eval-after-load 'face-remap
  (with-eval-after-load 'diminish
    (diminish 'buffer-face-mode)))


;;;; recentf

(with-eval-after-load 'no-littering
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15)

  (recentf-mode 1)

  (defvar my/recentf-autosave
    (run-with-idle-timer
     4 t (lambda ()
           (when recentf-mode
             ;; inhibit-message t didn't seem to stop
             ;; isearch messages from getting clobered
             ;; so we do this instead.
             (let ((message-log-max nil))
               (with-temp-message (or (current-message) "")
                 (recentf-save-list)))))))

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


;;;; outline

(keymap-global-set "C-c L" 'outline-minor-mode)
(with-eval-after-load 'outline
  (with-eval-after-load 'diminish
    (diminish 'outline-minor-mode " OL")))


;;;; dired

(with-eval-after-load 'dired
  (setq dired-omit-files (rx (or (seq string-start (1+ ".") (1+ (not ".")))
                                 (seq string-start (1+ "#"))))
        dired-dwim-target 'dired-dwim-target-recent)

  (define-keymap
    :keymap dired-mode-map
    "/" 'other-window-prefix
    "?" 'other-frame-prefix))


;;;; eldoc

(with-eval-after-load 'diminish
  (diminish 'eldoc-mode))

(setq eldoc-echo-area-prefer-doc-buffer t)


;;; Packages

;;;; treesit-auto

(elpaca treesit-auto)


;;;; persist

(elpaca (persist :host github :repo "emacs-straight/persist"))


;;;; Compat

(elpaca compat)


;;;; Diminish

(elpaca diminish
  (diminish 'visual-line-mode))


;;;; benchmark-init

;; (elpaca benchmark-init
;;   (require 'benchmark-init)
;;   (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

;; (profiler-start 'cpu+mem)
;; (add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))


;;;; narrow-indirect

(elpaca (narrow-indirect :host github :repo "emacsmirror/narrow-indirect")
  (with-eval-after-load 'narrow-indirect
    (with-eval-after-load 'modus-themes
      (face-spec-set 'ni-mode-line-buffer-id
                     '((t :background "#ffddff"
                          :box (:line-width 1 :color "#675967"))))))

  (define-keymap
    :keymap ctl-x-4-map
    "n d" 'ni-narrow-to-defun-indirect-other-window
    "n n" 'ni-narrow-to-region-indirect-other-window
    "n p" 'ni-narrow-to-page-indirect-other-window)

  (with-eval-after-load 'conn-mode
    (keymap-set conn-region-map "N" 'ni-narrow-to-region-indirect-other-window)))


;;;; paredit

(elpaca (paredit :host github :repo "emacsmirror/paredit")
  (dolist (mode '(lisp-data-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook
                  eshell-mode-hook
                  slime-repl-mode-hook))
    (add-hook mode #'enable-paredit-mode))

  (add-hook 'paredit-mode-hook #'paredit-disable-electric-pair)

  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)

  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

  (with-eval-after-load 'paredit
    (with-eval-after-load 'diminish
      (diminish 'paredit-mode))

    (keymap-unset paredit-mode-map "RET")
    (keymap-unset paredit-mode-map "M-s")
    (keymap-unset paredit-mode-map "M-;")

    (define-keymap
      :keymap paredit-mode-map
      "<remap> <backward-kill-word>" 'paredit-backward-kill-word
      "<remap> <backward-delete>" 'paredit-backward-delete
      "<remap> <forward-sexp>" 'paredit-forward
      "<remap> <backward-sexp>" 'paredit-backward
      "<remap> <forward-sentence>" 'paredit-forward-up
      "<remap> <backward-sentence>" 'paredit-backward-up)

    (keymap-set paredit-mode-map "M-l" 'paredit-splice-sexp)
    (keymap-set paredit-mode-map "C-w" 'paredit-kill-region)
    (keymap-set paredit-mode-map "<remap> <kill-region>" 'paredit-kill-region)
    (keymap-set paredit-mode-map "<remap> <delete-region>" 'paredit-delete-region)

    (defun paredit-space-for-delimiter-predicates-lisp (endp delimiter)
      (or endp
          (cond ((eq (char-syntax delimiter) ?\()
                 (not (or (looking-back ",@" nil t)
                          (looking-back "'" nil t)
                          (looking-back "`" nil t)
                          (looking-back "#." nil t))))
                ((eq (char-syntax delimiter) ?\")
                 (not (or (looking-back "#" nil t)
                          (looking-back "#." nil t))))
                (else t))))

    (add-to-list 'paredit-space-for-delimiter-predicates
                 'paredit-space-for-delimiter-predicates-lisp)

    (defun paredit-kill-rectangle-advice (fn &rest args)
      (if (not rectangle-mark-mode)
          (apply fn args)
        (setq this-command 'kill-rectangle)
        (call-interactively 'kill-rectangle)))
    (advice-add 'paredit-kill-region :around 'paredit-kill-rectangle-advice)

    (defun paredit-disable-electric-pair ()
      (electric-pair-local-mode -1))))


;;;; slime

(elpaca slime
  (require 'slime-autoloads)

  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-autodoc
                         slime-xref-browser
                         slime-repl
                         slime-cl-indent
                         slime-autodoc
                         slime-editing-commands
                         slime-fancy-inspector
                         slime-fancy-trace
                         slime-mdot-fu
                         slime-macrostep
                         slime-presentations
                         slime-scratch
                         slime-references
                         slime-package-fu
                         slime-fontifying-fu
                         slime-quicklisp
                         slime-trace-dialog
                         slime-hyperdoc
                         slime-quicklisp
                         slime-asdf
                         slime-sbcl-exts
                         slime-banner))

  ;; TODO: use local embark-keymap-alist instead
  (defun slime-setup-embark ()
    (require 'embark)
    (setq-local embark-expression-map (define-keymap
                                        :parent embark-expression-map
                                        "RET" 'slime-interactive-eval)
                embark-defun-map (define-keymap
                                   :parent embark-defun-map
                                   "RET" 'slime-eval-defun)
                embark-identifier-map (define-keymap
                                        :parent embark-identifier-map
                                        "RET" 'slime-edit-definition
                                        "M-RET" 'slime-hyperdoc-lookup)))
  (add-hook 'slime-mode-hook #'slime-setup-embark)
  (add-hook 'slime-repl-mode-hook #'slime-setup-embark)

  (with-eval-after-load 'slime
    (slime-setup)

    (keymap-unset slime-repl-mode-map "M-r")

    (defun slime-repl-skip-eval-when-reading (slime-eval &rest args)
      (if (slime-reading-p)
          (user-error "Synchronous Lisp Evaluation suppressed while reading input")
        (apply slime-eval args)))
    (advice-add 'slime-eval :around #'slime-repl-skip-eval-when-reading)))


;;;; bqn-mode

(elpaca (bqn-mode :host github :repo "museoa/bqn-mode")
  (with-eval-after-load 'bqn-mode
    (require 'bqn-keymap-mode)
    (require 'bqn-glyph-mode)))


;;;; cider

(elpaca cider)


;;;; lua-mode

(elpaca lua-mode)


;;;; go-mode

(elpaca go-mode)


;;;; rustic

(elpaca rustic
  (setq rustic-lsp-client 'lsp-mode))


;;;; erlang

(elpaca erlang)


;;;; elixir-mode

(elpaca elixir-mode)

;;;;; inf-elixir

(elpaca (inf-elixir :host github :repo "J3RN/inf-elixir")
  (with-eval-after-load 'elixir-mode
    (require 'inf-elixir)))


;;;; lsp-mode

(elpaca lsp-mode
  (add-hook 'lsp-mode-hook 'lsp-ui-peek-mode)

  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all nil
        lsp-enable-on-type-formatting nil
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-header t
        lsp-ui-doc-border "black"
        lsp-ui-doc-background '((t (:background "#dfd9cf")))
        ;; lsp-flycheck-warning-unnecessary-face '((t (:inherit modus-themes-lang-warning)))
        lsp-inlay-hint-face '((t (:inherit shadow :height 0.8))))

  (setq lsp-clients-clangd-args '("-j=4"
                                  "--log=error"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--cross-file-rename"
                                  "--header-insertion=never")
        lsp-zig-zls-executable "~/build/zls/zig-out/bin/zls")

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

  (with-eval-after-load 'lsp-mode
    (define-keymap
      :keymap lsp-mode-map
      "C-c I" 'lsp-inlay-hints-mode))

  ;; emacs-lsp-booster
  (define-advice json-parse-buffer (:around (old-fn &rest args) lsp-booster-parse-bytecode)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (define-advice lsp-resolve-final-command (:around (old-fn cmd &optional test?) add-lsp-server-booster)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result))))

;;;;; lsp-ui

(elpaca lsp-ui)


;;;; dap-mode

(elpaca dap-mode)


;;;; j-mode

(elpaca j-mode
  (setq j-console-cmd
        (locate-file "j9.4/jconsole.sh" exec-path
                     nil #'file-executable-p)))


;;;; cmake-mode

(elpaca cmake-mode)


;;;; zig-mode

(elpaca zig-mode
  (setq zig-format-on-save nil))


;;;; ess

(elpaca ess)


;;;; pdf-tools

(elpaca pdf-tools
  (with-eval-after-load 'pdf-tools
    (keymap-set pdf-view-mode-map "s a" #'pdf-view-auto-slice-minor-mode))

  (defun my-bmk-pdf-handler-advice (bookmark)
    (bookmark-default-handler (bookmark-get-bookmark bookmark)))

  (advice-add 'pdf-view-bookmark-jump-handler
              :after 'my-bmk-pdf-handler-advice)

  (pdf-loader-install))

;;;;; org-pdf-tools

(elpaca org-pdftools
  (with-eval-after-load 'pdf-tools
    (require 'org)
    (require 'org-pdftools)
    (org-pdftools-setup-link)))

;;;;; hide-mode-line-mode

(elpaca hide-mode-line)


;;;; tex

(elpaca (auctex :pre-build (("./autogen.sh")
                            ("./configure"
                             "--with-texmf-dir=$(kpsewhich -var-value TEXMFHOME)")
                            ("make"))))


;;;; cdlatex

(elpaca cdlatex
  (add-hook 'latex-mode-hook #'cdlatex-mode)
  (add-hook 'org-mode-hook #'org-cdlatex-mode)
  (setf cdlatex-math-symbol-alist
        '(( ?a  ("\\alpha"))
          ( ?A  ("\\forall"         "\\aleph"))
          ( ?b  ("\\beta"))
          ( ?B  (""))
          ( ?c  (""                 ""                "\\cos"))
          ( ?C  (""                 ""                "\\arccos"))
          ( ?d  ("\\delta"          "\\partial"))
          ( ?D  ("\\Delta"          "\\nabla"))
          ( ?e  ("\\epsilon"        "\\varepsilon"    "\\exp"))
          ( ?E  ("\\exists"         ""                "\\ln"))
          ( ?f  ("\\phi"            "\\varphi"))
          ( ?F  (""))
          ( ?g  ("\\gamma"          ""                "\\lg"))
          ( ?G  ("\\Gamma"          ""                "10^{?}"))
          ( ?h  ("\\eta"            "\\hbar"))
          ( ?H  (""))
          ( ?i  ("\\in"             "\\imath"))
          ( ?I  (""                 "\\Im"))
          ( ?j  ("\\left"          "\\right"        "\\jmath"))
          ( ?J  (""))
          ( ?k  ("\\kappa"))
          ( ?K  (""))
          ( ?l  ("\\lambda"         "\\ell"           "\\log"))
          ( ?L  ("\\Lambda"))
          ( ?m  ("\\mu"))
          ( ?M  (""))
          ( ?n  ("\\nu"             ""                "\\ln"))
          ( ?N  ("\\nabla"          ""                "\\exp"))
          ( ?o  ("\\omega"))
          ( ?O  ("\\Omega"          "\\mho"))
          ( ?p  ("\\pi"             "\\varpi"))
          ( ?P  ("\\Pi"))
          ( ?q  ("\\theta"          "\\vartheta"))
          ( ?Q  ("\\Theta"))
          ( ?r  ("\\rho"            "\\varrho"))
          ( ?R  ("\\Rho"                 "\\Re"))
          ( ?s  ("\\sigma"          "\\varsigma"      "\\sin"))
          ( ?S  ("\\Sigma"          ""                "\\arcsin"))
          ( ?t  ("\\tau"            ""                "\\tan"))
          ( ?T  ("\\Tau"            ""                "\\arctan"))
          ( ?u  ("\\upsilon"))
          ( ?U  ("\\Upsilon"))
          ( ?v  ("\\vee"))
          ( ?V  ("\\Phi"))
          ( ?w  ("\\xi"))
          ( ?W  ("\\Xi"))
          ( ?x  ("\\chi"))
          ( ?X  ("\\Chi"))
          ( ?y  ("\\psi"))
          ( ?Y  ("\\Psi"))
          ( ?z  ("\\zeta"))
          ( ?Z  ("\\Zeta"))
          ( ?0  ("\\emptyset"))
          ( ?1  ("\\sum" "\\prod"))
          ( ?2  ("\\dots"))
          ( ?3  ("\\ldots" "\\cdots"))
          ( ?4  ("\\quad" "\\qquad"))
          ( ?5  (""))
          ( ?6  ("\\vdots" "\\ddots"))
          ( ?7  (""))
          ( ?8  ("\\infty"))
          ( ?9  (""))
          ( ?!  ("\\neg"))
          ( ?@  ("\\circ"))
          ( ?#  (""))
          ( ?$  (""))
          ( ?%  (""))
          ( ?^  ("\\uparrow"))
          ( ?&  ("\\wedge"))
          ( ?\? (""))
          ( ?~  ("\\approx"         "\\simeq"))
          ( ?_  ("\\downarrow"))
          ( ?+  ("\\cup"   "\\cap"))
          ( ?-  ("\\leftrightarrow" "\\longleftrightarrow"))
          ( ?*  ("\\times"          ))
          ( ?/  ("\\not"))
          ( ?|  ("\\mapsto"         "\\longmapsto"))
          ( ?\\ ("\\setminus"))
          ( ?\" (""))
          ( ?=  ("\\Leftrightarrow" "\\Longleftrightarrow"))
          ( ?\( ("\\langle" "\\left"))
          ( ?\) ("\\rangle" "\\right"))
          ( ?\[ ("\\Leftarrow"      "\\Longleftarrow"))
          ( ?\] ("\\Rightarrow"     "\\Longrightarrow"))
          ( ?\{  ("\\subset"))
          ( ?\}  ("\\supset"))
          ( ?<  ("\\leftarrow"      "\\longleftarrow"     "\\min"))
          ( ?>  ("\\rightarrow"     "\\longrightarrow"    "\\max"))
          ( ?'  ("\\prime"))
          ( ?.  ("\\cdot")))))


;;;; math-delimiters

(elpaca (math-delimiters :host github :repo "oantolin/math-delimiters")
  (with-eval-after-load 'org
    (keymap-set org-mode-map "S-SPC" 'math-delimiters-insert)))


;;;; org

(elpaca org
  ;; (run-with-idle-timer 1.5 nil (lambda () (require 'org)))

  ;; (setopt org-use-speed-commands t)

  (setq org-agenda-include-diary t
        org-src-window-setup 'plain
        org-startup-truncated nil
        org-insert-mode-line-in-empty-file t
        org-confirm-babel-evaluate nil
        org-fold-core-style 'overlays
        org-startup-indented nil)

  (keymap-global-set "C-c c" 'org-capture)
  (keymap-global-set "C-c o" 'org-store-link)
  (keymap-global-set "C-c l" 'org-insert-link-global)

  (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)
  (add-hook 'org-mode-hook 'abbrev-mode)

  (with-eval-after-load 'org
    (keymap-unset org-mode-map "C-'")
    (keymap-unset org-mode-map "C-,")

    (setf (alist-get "\\*Org Src.*" display-buffer-alist nil nil #'equal)
          '((display-buffer-same-window)))

    (setf (plist-get org-format-latex-options :scale) 1.5)

    (keymap-set org-mode-map "C-c v" 'latex-math-mode)
    (autoload 'latex-math-mode "latex")

    (defun mathematica-nb-link ()
      "Insert an org link to a Mathematica notebook."
      (interactive)
      (let* ((notebook (read-file-name
                        "Notebook: " nil nil t nil
                        (lambda (filename)
                          (or (directory-name-p filename)
                              (member (file-name-extension filename)
                                      '("nb" "ma" "m" "ws" "wls" "cdf" "nbp"))))))
             (link (format "mathematica:%s" notebook)))
        (insert (org-link-make-string link (read-string "Description: ")))))

    (defun mathematica-nb-jump (notebook)
      (call-process "Mathematica" nil 0 nil notebook))

    (org-link-set-parameters "mathematica" :follow #'mathematica-nb-jump)))


;;;; dtrt-indent

(elpaca dtrt-indent
  (with-eval-after-load 'dtrt-indent-mode
    (with-eval-after-load 'diminish
      (diminish 'dtrt-indent-mode))))


;;;; exec-path-from-shell

;; (elpaca exec-path-from-shell
;;   (when (memq window-system '(mac ns x))
;;     (require 'exec-path-from-shell)
;;     (exec-path-from-shell-initialize)))


;;;; modus-themes

(when (< emacs-major-version 30)
  (elpaca modus-themes
    (run-with-timer 0.33 nil (lambda () (require 'modus-themes)))

    (with-eval-after-load 'modus-themes
      (setq modus-themes-common-palette-overrides
            (seq-concatenate
             'list
             `((bg-main "#f8f8f8")
               (cursor "#000000")
               (bg-region "#e1e1e1")
               (fg-region unspecified)
               (fg-completion-match-0 "#323c32")
               (bg-completion-match-0 "#caf1c9")
               (fg-completion-match-1 "#38333c")
               (bg-completion-match-1 "#e3cff1")
               (fg-completion-match-2 "#3c3333")
               (bg-completion-match-2 "#f1cccc")
               (fg-completion-match-3 "#343b3c")
               (bg-completion-match-3 "#d1eff1")
               (bg-search-lazy bg-magenta-subtle)
               (bg-search-current bg-yellow-intense))
             modus-themes-preset-overrides-warmer))

      (load-theme 'modus-operandi-tinted t))))

(setq hi-lock-face-defaults '("modus-themes-subtle-cyan"
                              "modus-themes-subtle-red"
                              "modus-themes-subtle-green"
                              "modus-themes-subtle-blue"
                              "modus-themes-subtle-yellow"))


;;;; no-littering

(elpaca no-littering
  (require 'no-littering)

  (setq backup-by-copying t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (no-littering-theme-backups))


;;;; crux

(elpaca crux
  ;; (keymap-global-set "C-<return>"   'crux-smart-open-line)
  (keymap-global-set "S-<return>"   'crux-smart-open-line)
  (keymap-global-set "C-x F"        'crux-sudo-edit)
  (keymap-global-set "C-x W"        'crux-open-with)
  (define-key global-map [remap kill-whole-line] 'crux-kill-whole-line)
  (define-key global-map [remap kill-line] 'crux-smart-kill-line)
  (define-key global-map [remap open-line] 'crux-smart-open-line)

  (with-eval-after-load 'conn-mode
    (keymap-set conn-state-map "S" 'crux-visit-shell-buffer)
    (keymap-set conn-state-map "D" 'crux-kill-whole-line)
    (keymap-set ctl-x-x-map    "b" 'crux-rename-file-and-buffer)

    (define-keymap
      :keymap conn-misc-edit-map
      "D"   'crux-duplicate-and-comment-current-line-or-region
      "RET" 'crux-cleanup-buffer-or-region
      "@"   'crux-insert-date)))


;;;; transpose-frame

(elpaca transpose-frame
  (with-eval-after-load 'conn-mode
    (define-keymap
      :keymap ctl-x-4-map
      "t" 'transpose-frame
      "C-t" 'transpose-frame
      ">" 'rotate-frame-clockwise
      "C-." 'rotate-frame-clockwise
      "<" 'rotate-frame-anticlockwise
      "C-," 'rotate-frame-anticlockwise
      "@" 'rotate-frame
      "C-o" 'rotate-frame
      "_" 'flip-frame
      "C--" 'flip-frame
      "|" 'flop-frame
      "C-\\" 'flop-frame)

    (defvar-keymap transpose-frame-repeat-map
      :repeat t
      "t" 'transpose-frame
      "C-t" 'transpose-frame
      ">" 'rotate-frame-clockwise
      "C-." 'rotate-frame-clockwise
      "<" 'rotate-frame-anticlockwise
      "C-," 'rotate-frame-anticlockwise
      "@" 'rotate-frame
      "C-o" 'rotate-frame
      "_" 'flip-frame
      "C--" 'flip-frame
      "|" 'flop-frame
      "C-\\" 'flop-frame)))


;;;; popper

(elpaca popper
  (setq popper-display-function 'display-buffer-reuse-window
        popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis))
        popper-reference-buffers '("\\*Messages\\*"
                                   "\\*Warnings\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "\\*sly-macroexpansion"
                                   "\\*sly-description\\*"
                                   "\\*projectile-files-errors\\*"
                                   help-mode
                                   helpful-mode
                                   compilation-mode))

  (define-keymap
    :keymap global-map
    "C-`"   'popper-toggle
    "M-`"   'popper-cycle
    "C-M-`" 'popper-toggle-type)

  (popper-mode 1)
  (popper-echo-mode 1))


;;;; posframe

(when window-system
  (elpaca posframe))


;;;; ace-window

(elpaca ace-window
  (setq aw-keys '(?f ?d ?r ?s ?g ?t ?q ?w)
        aw-dispatch-always t)

  (face-spec-set
   'aw-leading-char-face
   '((t (:inherit ace-jump-face-foreground :height 5.0))))

  (keymap-global-set "C-;" 'ace-window)

  (with-eval-after-load 'ace-window
    (when window-system
      (ace-window-posframe-mode 1)))

  (with-eval-after-load 'conn-mode
    (advice-add 'aw-show-dispatch-help :around 'disable-minibuffer-max-height)))


;;;; zones

(elpaca zones
  (cl-defstruct (izone-register)
    (buffer nil :read-only t)
    (izones nil :read-only t))

  (cl-defmethod register-val-jump-to ((val izone-register) _arg)
    (if (eq (izone-register-buffer val) (current-buffer))
        (set zz-izones-var (izone-register-izones val))
      (user-error "Izones not for this buffer.")))

  (cl-defmethod register-val-describe ((val izone-register) _arg)
    (princ (format "%s izones in %s\n"
                   (length (izone-register-izones val))
                   (izone-register-buffer val))))

  (defun izones-to-register (register)
    (interactive (list (register-read-with-preview "izones to register: ")))
    (require 'zones)
    (set-register register
                  (make-izone-register
                   :buffer (current-buffer)
                   :izones (symbol-value zz-izones-var))))

  (define-keymap
    :keymap narrow-map
    "q" #'zz-delete-zone
    "*" #'zz-replace-regexp-zones
    "/" #'zz-replace-string-zones
    "%" #'zz-map-query-replace-regexp-zones
    "j" #'izones-to-register
    "1" #'zz-coalesce-zones)

  (with-eval-after-load 'isearch+
    (defun isearch-in-zone-p (beg end)
      (catch 'res
        (pcase-dolist (`(,_ ,zbeg ,zend) (symbol-value zz-izones-var))
          (when (and (<= zbeg beg) (<= end zend)) (throw 'res t)))))
    (cl-pushnew '("[zone]" isearch-in-zone-p "[ZZ]")
                isearchp-current-filter-preds-alist
                :test #'equal))

  (with-eval-after-load 'isearch+
    (define-keymap
      :keymap narrow-map
      "b" #'isearchp-zones-forward
      "B" #'isearchp-zones-backward))

  (with-eval-after-load 'bookmark+
    (keymap-global-set "C-x r z" 'bmkp-set-izones-bookmark))

  (with-eval-after-load 'embark
    (keymap-set embark-region-map "z" #'zz-add-zone))

  (with-eval-after-load 'conn-mode
    (keymap-set conn-state-map "X" 'zz-narrow)
    (keymap-set conn-misc-edit-map "w" #'zz-narrow-repeat)))


;;;; isearch+

(elpaca (isearch+ :host github
                  :repo "emacsmirror/isearch-plus"
                  :main "isearch+.el")
  (run-with-timer 0.33 nil (lambda () (require 'isearch+)))

  (setq isearchp-dimming-color "#cddfcc"
        isearchp-lazy-dim-filter-failures-flag nil
        isearchp-restrict-to-region-flag nil
        isearchp-deactivate-region-flag nil
        isearchp-initiate-edit-commands nil
        isearchp-movement-unit-alist '((?w . forward-word)
                                       (?s . forward-sexp)
                                       (?i . forward-list)
                                       (?s . forward-sentence)
                                       (?c . forward-char)
                                       (?l . forward-line)))

  (with-eval-after-load 'isearch+
    (keymap-unset isearch-mode-map "C-t")
    (keymap-set isearch-mode-map "C-;" 'isearchp-property-forward)
    (keymap-set isearch-mode-map "C-y m" 'isearchp-yank-sexp-symbol-or-char)
    (keymap-set isearch-mode-map "C-y o" 'isearchp-yank-word-or-char-forward)
    (keymap-set isearch-mode-map "C-y u" 'isearchp-yank-word-or-char-backward)
    (keymap-set isearch-mode-map "C-y i" 'isearchp-yank-line-backward)
    (keymap-set isearch-mode-map "C-y k" 'isearchp-yank-line-forward)
    (keymap-set isearch-mode-map "C-y l" 'isearchp-yank-char)
    (keymap-set isearch-mode-map "C-M-o" 'isearchp-open-recursive-edit)
    (keymap-set isearchp-filter-map "f" 'isearchp-add-filter-predicate)
    (keymap-set isearchp-filter-map "r" 'isearchp-add-regexp-filter-predicate)))

;;;;; isearch-prop

(elpaca (isearch-prop :host github :repo "emacsmirror/isearch-prop")
  (with-eval-after-load 'isearch+
    (require 'isearch-prop)))


;;;; undo-hl

;; (elpaca (undo-hl :host github :repo "casouri/undo-hl")
;;   (undo-hl-mode 1))


;;;; conn-mode

(elpaca (conn-mode :host github :repo "mtll/conn-mode")
  (setq conn-state-buffer-colors t
        conn-lighter ""
        conn-delete-region-keys "C-S-w"
        dot-state-cursor-type 'box
        conn-state-cursor-type 'box
        emacs-state-cursor-type 'box
        conn-expreg-leave-region-active nil)

  (setopt conn-mark-idle-timer 0.05
          conn-aux-map-update-delay 0.05)

  (with-eval-after-load 'modus-themes
    (face-spec-set 'conn-mark-face '((default :inherit modus-themes-intense-magenta
                                              :background unspecified))))

  (add-hook 'view-mode-hook #'emacs-state)

  (conn-mode 1)
  (conn-mode-line-indicator-mode 1)

  (conn-hide-mark-cursor 'view-state)

  (set-default-conn-state '("COMMIT_EDITMSG.*" "^\\*Echo.*") 'emacs-state)

  (define-keymap
    :keymap global-map
    "C-c v" 'conn-toggle-mark-command
    "M-n" 'conn-embark-region)

  (keymap-global-set "C-x ," 'global-subword-mode)

  (keymap-set conn-mode-map "S-<return>" 'conn-open-line-and-indent)

  (define-keymap
    :keymap conn-state-map
    ;; "<remap> <forward-char>" 'conn-goto-char-forward
    ;; "<remap> <backward-char>" 'conn-goto-char-backward
    "M-TAB" #'indent-for-tab-command
    "M-<tab>" #'indent-for-tab-command
    "TAB" #'embark-act
    "<tab>" #'embark-act)

  (keymap-set conn-common-map "T" 'tab-switch)

  (with-eval-after-load 'vertico
    (keymap-set vertico-map "<f1>" 'conn-toggle-minibuffer-focus)))

;;;;; Conn Extensions

(elpaca (conn-consult :host github
                      :repo "mtll/conn-mode"
                      :files ("extensions/conn-consult.el"))  
  (keymap-set emacs-state-map "M-TAB" 'embark-act)
  (conn-embark-dwim-keys 1)
  (conn-complete-keys-prefix-help-command 1)

  (with-eval-after-load 'embark
    (define-keymap
      :keymap embark-general-map
      "R" 'conn-embark-replace-region
      "~" 'conn-dot-region)

    (keymap-set embark-kill-ring-map "r" 'conn-embark-replace-region)
    (keymap-unset embark-expression-map "D")
    (keymap-unset embark-defun-map "D")))

(elpaca (conn-embark :host github
                     :repo "mtll/conn-mode"
                     :files ("extensions/conn-embark.el")))

(elpaca (conn-avy :host github
                  :repo "mtll/conn-mode"
                  :files ("extensions/conn-avy.el"))
  (keymap-set goto-map "C-," 'conn-avy-goto-dot))

(elpaca (conn-expreg :host github
                     :repo "mtll/conn-mode"
                     :files ("extensions/conn-expreg.el"))
  (conn-expreg-always-use-region 1))

(elpaca (conn-isearch+ :host github
                       :repo "mtll/conn-mode"
                       :files ("extensions/conn-isearch+.el"))
  (keymap-set isearch-mode-map "C-M-." 'conn-isearch-in-dot-toggle))

(elpaca (conn-calc :host github
                   :repo "mtll/conn-mode"
                   :files ("extensions/conn-calc.el"))
  (with-eval-after-load 'calc
    (conn-calc-shim 1)))


;;;; evil text objects

(elpaca evil-textobj-tree-sitter)


;;;; expreg

(elpaca expreg
  (keymap-global-set "C-t" 'expreg-expand)

  (with-eval-after-load 'conn-mode
    (define-keymap
     :keymap conn-common-map
     "b" 'expreg-expand)))


;;;; ialign

(elpaca ialign
  (with-eval-after-load 'conn-mode
    (keymap-set conn-region-map "a" 'ialign))

  (with-eval-after-load 'embark
    (defun embark-ialign (_reg)
      (ialign (region-beginning) (region-end)))

    (keymap-set embark-region-map "a" 'embark-ialign)))


;;;; bookmark+

(elpaca (bookmark+ :host github
                   :repo "emacsmirror/bookmark-plus"
                   :main "bookmark+.el")
  (run-with-timer 0.5 nil (lambda () (require 'bookmark+)))

  (setq bmkp-bookmark-map-prefix-keys '("x")
        bmkp-last-as-first-bookmark-file nil
        bmkp-prompt-for-tags-flag t
        bookmark-version-control t
        delete-old-versions t
        bookmark-save-flag 1)

  (defun bmkp-org-bookmark-store-link-1 ()
    (when (eq major-mode #'bookmark-bmenu-mode)
      (bmkp-org-bookmark-store-link)))

  (defun bmkp-org-bookmark-store-link ()
    (interactive)
    (require 'org)
    (let* ((bmk  (bmkp-completing-read-lax (format "Org link for bookmark")))
           (link (format "bmk:%s" bmk))
           (desc (read-string "Description: ")))
      (org-link-store-props :type "bmk"
                            :link link
                            :description desc)
      link))

  (defun bmkp-org-bookmark-link (bmk)
    (interactive (list (bmkp-completing-read-lax (format "Org link for bookmark"))))
    (require 'org)
    (insert (org-link-make-string (format "bmk:%s" bmk)
                                  (let ((desc (read-string "Description: ")))
                                    (unless (string= desc "")
                                      desc)))))

  (keymap-set bookmark-map "k" #'bmkp-org-bookmark-link)
  (keymap-set bookmark-map "K" #'bmkp-org-bookmark-store-link)

  (with-eval-after-load 'org
    (org-link-set-parameters "bmk"
                             :follow #'bookmark-jump
                             :store #'bmkp-org-bookmark-store-link-1)))


;;;; dired+

(elpaca (dired+ :host github
                :repo "emacsmirror/dired-plus"
                :main "dired+.el"))


;;;; visual-regexp

;; (elpaca visual-regexp
;;   (keymap-global-set "<remap> <query-replace-regexp>" #'vr/query-replace)
;;   (keymap-global-set "C-M-!" #'vr/replace)

;;   (with-eval-after-load 'conn-mode
;;     (define-keymap
;;       :keymap conn-misc-edit-map
;;       "r" 'vr/query-replace
;;       "R" 'vr/replace)))


;;;; avy

(elpaca avy
  (setq avy-single-candidate-jump nil
        avy-timeout-seconds 0.45
        avy-keys '(?a ?b ?f ?g ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?u ?v ?x)
        avy-line-insert-style 'below)

  (setq avy-dispatch-alist '((?w  .  avy-action-kill-move)
                             (?d  .  avy-action-kill-stay)
                             (?t  .  avy-action-teleport)
                             (?c  .  avy-action-copy)
                             (?y  .  avy-action-yank)
                             (?Y  .  avy-action-yank-line)
                             (?\\ .  avy-action-zap-to-char)))

  (with-eval-after-load 'ace-window
    ;; stops ace-window from breaking when minibuffer indicator is enabled
    (defun avy-process-disable-aw-update (&rest app)
      (cl-letf (((symbol-function 'aw-update) #'ignore))
        (apply app)))
    (advice-add #'avy-process :around 'avy-process-disable-aw-update))

  (keymap-set isearch-mode-map "M-," 'avy-isearch)

  (define-keymap
    :keymap goto-map
    "j" 'avy-goto-char-timer
    "o" 'avy-goto-word-or-subword-1
    "m" 'avy-goto-symbol-1
    "k" 'avy-goto-line
    "l" 'avy-goto-end-of-line
    "z" 'avy-resume
    "Y" 'my/avy-toggle-insertion-style
    "C-y" 'my/avy-toggle-insertion-style
    "i" 'avy-goto-char-in-line)

  (with-eval-after-load 'avy
    (dolist (cmd '(avy-goto-char
                   avy-goto-char
                   avy-goto-char-2
                   avy-isearch
                   avy-goto-line
                   avy-goto-subword-0
                   avy-goto-subword-1
                   avy-goto-word-0
                   avy-goto-word-1
                   avy-copy-line
                   avy-copy-region
                   avy-move-line
                   avy-move-region
                   avy-kill-whole-line
                   avy-kill-region
                   avy-kill-ring-save-whole-line
                   avy-kill-ring-save-region))
      (setf (alist-get cmd avy-orders-alist) #'avy-order-closest))

    (defun avy-enable-single-candidate-jump (fn &rest args)
      (let ((avy-single-candidate-jump t))
        (apply fn args)))
    (advice-add 'avy-goto-char-in-line :around #'avy-enable-single-candidate-jump)
    (advice-add 'avy-goto-char-timer :around #'avy-enable-single-candidate-jump)

    (defun avy-isearch ()
      "Jump to one of the current isearch candidates."
      (interactive)
      (avy-with avy-isearch
        (let ((avy-background nil)
              (avy-case-fold-search case-fold-search))
          (prog1
              (avy-process
               (avy--regex-candidates
                (cond
                 ((functionp isearch-regexp-function)
                  (funcall isearch-regexp-function isearch-string))
                 (isearch-regexp-function (word-search-regexp isearch-string))
                 (isearch-regexp isearch-string)
                 (t (regexp-quote isearch-string)))))
            (isearch-done)))))

    (with-eval-after-load 'hyperbole
      (defun avy-action-action-key (pt)
        (unwind-protect
            (let ((newpt (save-excursion
                           (goto-char pt)
                           (action-key)
                           (point))))
              (unless (eq newpt pt)
                (goto-char newpt)))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?e avy-dispatch-alist) #'avy-action-action-key)

      (defun avy-action-assist-key (pt)
        (unwind-protect
            (let ((newpt (save-excursion
                           (goto-char pt)
                           (assist-key)
                           (point))))
              (unless (eq newpt pt)
                (goto-char newpt)))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?h avy-dispatch-alist) #'avy-action-assist-key))

    (with-eval-after-load 'embark
      (defun avy-action-embark (pt)
        (unwind-protect
            (save-excursion
              (goto-char pt)
              (embark-act))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?\C-i avy-dispatch-alist) #'avy-action-embark))

    (defun my/avy-toggle-insertion-style ()
      (interactive)
      (if (eq avy-line-insert-style 'above)
          (setq avy-line-insert-style 'below)
        (setq avy-line-insert-style 'above))
      (message "Avy line insertion style set to: %s" avy-line-insert-style))
    (put 'my/avy-toggle-insertion-style 'repeat-map goto-map)))


;;;; helpful

(elpaca helpful
  (keymap-global-set "C-h v" 'helpful-variable)
  (keymap-global-set "C-h k" 'helpful-key)
  (keymap-global-set "C-h ," 'display-local-help)
  (keymap-global-set "C-h ." 'helpful-at-point)
  (keymap-global-set "<remap> <describe-function>" 'helpful-callable)
  (keymap-global-set "<remap> <describe-variable>" 'helpful-variable)

  (push '(help-mode . helpful-mode) major-mode-remap-alist)

  (with-eval-after-load 'embark
    (keymap-set embark-symbol-map "M-RET" 'helpful-symbol)))


;;;; all-the-icons

(elpaca all-the-icons)

;;;;; all-the-icons-dired

(elpaca all-the-icons-dired
  (with-eval-after-load 'diminish
    (diminish 'all-the-icons-dired-mode))
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))


;;;; magit

(elpaca magit
  (keymap-global-set "C-c s" 'magit-status))


;;;; flycheck

;; (elpaca flycheck
;;   (setq lsp-diagnostics-flycheck-default-level 'warning))


;;;; cape

(elpaca cape
  (keymap-global-set "M-L" #'cape-line)
  (keymap-global-set "M-K" #'cape-dict)

  (add-to-list 'completion-at-point-functions #'cape-file)

  (defun dictionary-doc-lookup (cand)
    (let* ((buffer)
           (dictionary-display-definition-function
            (lambda (word dictionary definition)
              (let ((help-buffer-under-preparation t))
                (help-setup-xref (list #'dictionary-search word dictionary)
                                 (called-interactively-p 'interactive))
                (with-current-buffer (help-buffer)
                  (insert definition)
                  (goto-char (point-min))
                  (setq buffer (current-buffer)))))))
      (dictionary-search cand)
      buffer))

  (add-hook 'text-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions
                           (cape-capf-properties
                            #'cape-dict
                            :company-doc-buffer #'dictionary-doc-lookup)))))


;;;; embark

(elpaca embark
  (setq embark-quit-after-action t
        embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator)
        embark-prompter 'embark-keymap-prompter
        embark-cycle-key "<tab>"
        embark-help-key "?"
        embark-confirm-act-all nil)

  (keymap-global-set "M-." 'embark-act)
  (keymap-set minibuffer-mode-map "C-M-," 'embark-export)

  (defun embark-act-persist ()
    (interactive)
    (require 'embark)
    (let (embark-quit-after-action)
      (embark-act)))

  (defun embark-act-marked ()
    (interactive)
    (require 'embark)
    (if (embark-selected-candidates)
        (embark-act-all)
      (embark-act)))

  (defvar-keymap embark-consult-location-map)
  (defvar-keymap embark-consult-grep-map)

  (defvar-keymap embark-page-map
    "RET" 'my/forward-page
    "M-RET" 'my/backward-page
    "n" 'forward-page
    "p" 'backward-page
    "u" 'narrow-to-page
    "m" 'mark-page)

  (with-eval-after-load 'narrow-indirect
    (keymap-set embark-region-map "N" 'ni-narrow-to-region-indirect-other-window)
    (keymap-set embark-defun-map  "N" 'ni-narrow-to-defun-indirect-other-window)
    (keymap-set embark-page-map "o" 'ni-narrow-to-page-indirect-other-window))

  (defvar-keymap my/embark-tab-map
    "d" 'embark-tab-delete
    "r" 'embark-tab-rename
    "t" 'embark-tab-detach)

  (with-eval-after-load 'embark
    (cl-pushnew 'my/embark-tab-map (alist-get 'tab embark-keymap-alist))
    (setf (alist-get 'page embark-keymap-alist) (list 'embark-page-map))

    (set-keymap-parent embark-consult-location-map embark-general-map)
    (set-keymap-parent embark-consult-grep-map embark-general-map)
    (add-to-list 'embark-keymap-alist '(consult-location embark-consult-location-map))
    (add-to-list 'embark-keymap-alist '(consult-grep embark-consult-grep-map))

    (keymap-set embark-defun-map "n" 'narrow-to-defun)
    (keymap-set embark-symbol-map "h" 'helpful-symbol)
    (keymap-set embark-collect-mode-map "C-j" 'consult-preview-at-point)
    ;; (keymap-set embark-defun-map "M-RET" 'comment-region)
    ;; (keymap-set embark-identifier-map "M-RET" 'xref-find-references)
    (keymap-set embark-heading-map "RET" #'outline-cycle)
    ;; (keymap-set embark-heading-map "M-RET" #'outline-up-heading)
    (keymap-set embark-symbol-map "RET" #'xref-find-definitions)

    (defun embark-tab-delete (name)
      (tab-bar-close-tab
       (1+ (tab-bar--tab-index-by-name name))))

    (defun embark-tab-rename (tab-name)
      (tab-bar-rename-tab
       (read-from-minibuffer
        "New name for tab (leave blank for automatic naming): "
        nil nil nil nil tab-name)))

    (defun embark-tab-detach (tab-name)
      (let* ((tabs (funcall tab-bar-tabs-function))
             (tab-index (tab-bar--tab-index-by-name tab-name))
             (from-frame (selected-frame))
             (new-frame (make-frame `((name . ,tab-name)))))
        (tab-bar-move-tab-to-frame
         nil from-frame from-number new-frame nil)
        (with-selected-frame new-frame
          (tab-bar-close-tab))))

    (defun embark-looking-at-page-target-finder ()
      (when (and (or (save-excursion
                       (beginning-of-line)
                       (looking-at page-delimiter))
                     (eobp)
                     (bobp))
                 (not (window-minibuffer-p (selected-window))))
        (let ((bounds (bounds-of-thing-at-point 'page)))
          (cons 'page (cons
                       (buffer-substring (car bounds) (cdr bounds))
                       bounds)))))
    (add-hook 'embark-target-finders #'embark-looking-at-page-target-finder -80)

    (defun embark-page-target-finder ()
      (when-let ((bounds (bounds-of-thing-at-point 'page)))
        (cons 'page (cons
                     (buffer-substring (car bounds) (cdr bounds))
                     bounds))))
    (add-hook 'embark-target-finders #'embark-page-target-finder 90)

    (defun embark-consult-kill-lines (cands)
      (let (strs)
        (pcase-dolist (`(,marker . ,line) (mapcar #'consult--get-location cands))
          (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (let ((bol (line-beginning-position))
                  (eol (line-end-position)))
              (push (filter-buffer-substring bol eol) strs)
              (delete-region bol (min (1+ eol) (point-max))))
            ;; Does consult already do this?
            (set-marker marker nil)))
        (kill-new (string-join strs "\n"))))
    (keymap-set embark-consult-location-map "C-k" 'embark-consult-kill-lines)
    (cl-pushnew 'embark-consult-kill-lines embark-multitarget-actions)

    (with-eval-after-load 'org
      (defun embark-bookmark-link (cand)
        (when cand
          (let* ((desc (read-string "Description: "))
                 (fmt (if (string= desc "")
                          "[[bmk:%s]]"
                        "[[bmk:%s][%s]]")))
            (insert (format fmt cand desc)))))

      (define-keymap
        :keymap embark-bookmark-map
        "M-RET" 'embark-bookmark-link)

      (add-hook 'embark-target-finders 'embark-org-target-link -85)))

  (with-eval-after-load 'vertico
    (define-keymap
      :keymap vertico-map
      "C-TAB" 'embark-act-all
      "C-<tab>" 'embark-act-all
      "M-TAB" 'embark-act-persist
      "M-<tab>" 'embark-act-persist
      "C-SPC" 'embark-select
      "TAB" 'embark-act-marked
      "<tab>" 'embark-act-marked)))

;;;;; embark-consult

(elpaca embark-consult
  (with-eval-after-load 'embark
    (define-keymap
      :keymap embark-region-map
      "l" 'consult-line
      "r" 'consult-ripgrep
      "h" nil
      "h o" 'consult-line
      "h f" 'consult-find
      "h g" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h r" 'consult-ripgrep)

    (define-keymap
      :keymap embark-general-map
      "h l" 'consult-line
      "h f" 'consult-find
      "h g" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h r" 'consult-ripgrep)))


;;;; corfu

(elpaca corfu
  (run-with-timer
   0.33 nil (lambda ()
              (global-corfu-mode 1)
              (corfu-echo-mode 1)))

  (setq corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-auto t
        corfu-auto-delay 0.05
        corfu-auto-prefix 3
        corfu-map (define-keymap
                    "<remap> <forward-sentence>" 'corfu-prompt-end
                    "<remap> <backward-sentence>" 'corfu-prompt-beginning
                    "<remap> <scroll-down-command>" #'corfu-scroll-down
                    "<remap> <scroll-up-command>" #'corfu-scroll-up
                    "<tab>" #'corfu-complete
                    "C-h" #'corfu-info-documentation
                    "M-h" #'corfu-info-location
                    "M-n" #'corfu-next
                    "M-p" #'corfu-previous
                    "C-g" #'corfu-quit
                    "TAB" #'corfu-complete
                    "M-SPC" #'corfu-insert-separator))

  (with-eval-after-load 'corfu
    (defun corfu-sep-and-start ()
      (interactive)
      (completion-at-point)
      (corfu-insert-separator))

    (keymap-set corfu-mode-map "M-SPC" #'corfu-sep-and-start))

  (with-eval-after-load 'lsp-mode
    (defun wrap-lsp-capf ()
      (setq-local completion-at-point-functions
                  (cl-nsubst
                   (cape-capf-noninterruptible
                    (cape-capf-buster #'lsp-completion-at-point))
                   #'lsp-completion-at-point completion-at-point-functions)))
    (add-hook 'lsp-managed-mode-hook #'wrap-lsp-capf))

  (with-eval-after-load 'eglot
    (defun wrap-eglot-capf ()
      (setq-local completion-at-point-functions
                  (cl-nsubst
                   (cape-capf-noninterruptible
                    (cape-capf-buster #'eglot-completion-at-point))
                   #'eglot-completion-at-point completion-at-point-functions)))
    (add-hook 'eglot-managed-mode-hook #'wrap-eglot-capf)))

;;;;; kind-icons

(elpaca kind-icon
  (setq kind-icon-use-icons nil
        kind-icon-extra-space t)

  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))


;;;; bicycle

(elpaca bicycle
  (with-eval-after-load 'outline
    (define-keymap
      :keymap outline-mode-map
      "C-<tab>" 'bicycle-cycle)

    (define-keymap
      :keymap outline-minor-mode-map
      ;; [C-tab] 'bicycle-cycle
      "C-<tab>" 'bicycle-cycle
      "<backtab>" 'bicycle-cycle-global)))


;;;; outline-minor-faces

(elpaca outline-minor-faces
  (with-eval-after-load 'outline
    (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode)))


;;;; keycast

(elpaca keycast)


;;;; gif-screencase

(elpaca gif-screencast
  (with-eval-after-load 'conn-mode
    (keymap-global-set "M-<f2>" 'gif-screencast-start-or-stop)))


;;;; wgrep

(elpaca wgrep)


;;;; lazytab

(elpaca (lazytab :host github :repo "karthink/lazytab"))


;;;; separedit

(elpaca separedit
  (with-eval-after-load 'helpful
    (keymap-set helpful-mode-map "C-c '" 'separedit))
  (keymap-set prog-mode-map "C-c '" 'separedit)
  (keymap-set minibuffer-local-map "C-c '" 'separedit)
  (keymap-set help-mode-map "C-c '" 'separedit))


;;;; package-link-flymake

(elpaca package-lint)


;;;; orderless

(elpaca orderless
  (require 'orderless)
  (setq orderless-affix-dispatch-alist '((?\= . orderless-literal)
                                         (?! . orderless-without-literal)
                                         (?, . orderless-initialism))
        completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-literal
                                    orderless-regexp)
        orderless-style-dispatchers '(orderless-kwd-dispatch
                                      orderless-affix-dispatch
                                      flex-first-if-completing)
        completion-category-overrides '((file (styles basic
                                                      partial-completion
                                                      orderless+flex))
                                        (lsp-capf (styles orderless+flex)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-smart-case t
        read-file-name-completion-ignore-case nil
        read-buffer-completion-ignore-case nil)

  (defun flex-first-if-completing (pattern index _total)
    (when (and (= index 0) completion-in-region-mode)
      `(orderless-flex . ,pattern)))

  (defun orderless-toggle-smart-case ()
    (interactive)
    (cond (orderless-smart-case
           (setq-local orderless-smart-case (not orderless-smart-case)))
          ((null completion-ignore-case)
           (setq-local completion-ignore-case t))
          (t
           (setq-local orderless-smart-case t
                       completion-ignore-case nil)))
    (message "ignore-case: %s" (if orderless-smart-case
                                   "smart"
                                 completion-ignore-case)))
  (keymap-set minibuffer-local-map "M-C" 'orderless-toggle-smart-case)

  (defun flex-first (pattern index _total)
    (when (= index 0) `(orderless-flex . ,pattern)))

  (orderless-define-completion-style orderless+flex
    (orderless-matching-styles '(orderless-literal
                                 orderless-initialism
                                 orderless-regexp))
    (orderless-style-dispatchers '(orderless-kwd-dispatch
                                   orderless-affix-dispatch
                                   flex-first))))

;;;;; orderless-set-operations

(elpaca (orderless-set-operations :host github
                                  :repo "mtll/orderless-set-operations")
  (with-eval-after-load 'orderless
    (oso-mode 1)))


;;;; consult

(elpaca consult
  (setq consult-async-min-input 3
        consult-yank-rotate t
        consult-narrow-key "M-N"
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0.3
        register-preview-function #'consult-register-format
        completion-in-region-function #'consult-completion-in-region
        consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-modified-buffer
                                 consult--source-buffer
                                 consult--source-bookmark
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-project-buffer-hidden
                                 consult--source-project-recent-file-hidden))

  (keymap-global-set "<remap> <Info-search>" #'consult-info)
  (keymap-global-set "<remap> <bookmark-jump>" #'consult-bookmark)
  (keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
  (keymap-global-set "<remap> <yank-from-kill-ring>" #'consult-yank-from-kill-ring)
  (keymap-global-set "<remap> <jump-to-register>" #'consult-register-load)
  (keymap-global-set "<remap> <switch-to-buffer>" 'consult-buffer)
  (keymap-global-set "C-x k" 'kill-this-buffer)
  (keymap-global-set "M-X" 'consult-mode-command)
  (keymap-global-set "C-x M-:" 'consult-complex-command)
  (keymap-global-set "C-h i" 'consult-info)
  (keymap-global-set "C-h TAB" 'info)

  (keymap-set minibuffer-local-map "M-r" 'consult-history)

  (define-keymap
    :keymap search-map
    "p" 'consult-page
    "K" 'consult-kmacro
    "N" 'consult-ripgrep-n
    "w" 'consult-man
    "e" 'consult-isearch-history
    "t" 'consult-outline
    "o" 'consult-line
    "O" 'consult-line-multi
    "r" 'consult-ripgrep
    "G" 'consult-grep
    "g" 'consult-git-grep
    "f" 'consult-find
    "L" 'consult-locate
    "v" 'consult-focus-lines
    "k" 'consult-keep-lines
    "i" 'consult-imenu
    "I" 'consult-imenu-multi)

  (keymap-set goto-map "g" 'consult-goto-line)
  (keymap-global-set "<remap> <project-switch-to-buffer>" 'consult-project-buffer)

  (define-keymap
    :keymap isearch-mode-map
    "M-s j" 'consult-line
    "M-s J" 'consult-line-multi)

  (with-eval-after-load 'consult
    (setq consult-ripgrep-args (concat consult-ripgrep-args " --multiline"))

    (consult-customize consult-completion-in-region :preview-key nil)
    (consult-customize consult--source-bookmark :preview-key "C-j")
    (consult-customize consult-bookmark :preview-key "C-j")
    (consult-customize consult-buffer :preview-key "C-j")
    (consult-customize consult-project-buffer :preview-key "C-j"))

  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  (defun consult-async-pause (&optional arg)
    (interactive "P")
    (require 'consult)
    (setq consult-async-min-input
          (if (eq consult-async-min-input most-positive-fixnum)
              (or (and arg (prefix-numeric-value arg)) 3)
            most-positive-fixnum)))

  (defun consult--ripgrep-n-make-builder (paths)
    (let* ((cmd (consult--build-args consult-ripgrep-args))
           (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
      (lambda (input)
        (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                     (flags (append cmd opts))
                     (ignore-case
                      (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                           (or (member "-i" flags) (member "--ignore-case" flags)
                               (and (or (member "-S" flags) (member "--smart-case" flags))
                                    (let (case-fold-search)
                                      ;; Case insensitive if there are no uppercase letters
                                      (not (string-match-p "[[:upper:]]" arg))))))))
          (if (or (member "-F" flags) (member "--fixed-strings" flags))
              (let ((res (consult--split-escaped arg)))
                (cons (append `("rgn" ,(car flags)
                                ,(string-join (append (cdr flags) '("-m" "1")) " "))
                              (cdr res) '("--") (list (car res)) paths)
                      (apply-partially #'consult--highlight-regexps
                                       (list (regexp-quote arg)) ignore-case)))
            (pcase-let ((`(,res . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
              (when res
                (cons (append `("rgn" ,(car flags)
                                ,(string-join (append (cdr flags)
                                                      (and (eq type 'pcre) '("-P"))
                                                      '("-m" "1"))
                                              " "))
                              (cdr res) '("--") (list (car res)) paths)
                      hl))))))))

  (defun consult-ripgrep-n (&optional dir initial)
    (interactive "P")
    (consult--grep "Ripgrep N" #'consult--ripgrep-n-make-builder dir initial))

  (defun consult--page-candidates ()
    "Return alist of outline headings and positions."
    (consult--forbid-minibuffer)
    (let* ((line (line-number-at-pos (point-min) consult-line-numbers-widen))
           (page-regexp (concat "^\\(?:" page-delimiter "\\)"))
           (buffer (current-buffer))
           candidates)
      (save-excursion
        (goto-char (point-min))
        (while (save-excursion
                 (if-let (fun (bound-and-true-p outline-search-function))
                     (funcall fun)
                   (re-search-forward page-regexp nil t)))
          (cl-incf line (consult--count-lines (match-beginning 0)))
          (push (consult--location-candidate
                 (save-excursion
                   (forward-line)
                   (consult--buffer-substring (pos-bol) (pos-eol) 'fontify))
                 (cons buffer (point)) (1- line) (1- line))
                candidates)
          (goto-char (1+ (pos-eol)))))
      (unless candidates
        (user-error "No pages"))
      (nreverse candidates)))

  (defun consult-page ()
    "Jump to a page."
    (interactive)
    (let* ((candidates (consult--slow-operation
                           "Collecting headings..."
                         (consult--page-candidates))))
      (consult--read
       candidates
       :prompt "Go to page: "
       :annotate (consult--line-prefix)
       :category 'consult-location
       :sort nil
       :require-match t
       :lookup #'consult--line-match
       :history '(:input consult--line-history)
       :add-history (thing-at-point 'symbol)
       :state (consult--location-state candidates))))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (advice-add #'register-preview :override #'consult-register-window)

  (defun consult-goto-edit ()
    (interactive)
    (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
           (candidates (consult--slow-operation "Collecting lines..."
                         (let ((candidates nil))
                           (save-excursion
                             (dolist (undo buffer-undo-list)
                               (pcase undo
                                 ((and (pred integerp) pos)
                                  (goto-char pos)
                                  (push (line-number-at-pos) candidates))
                                 ((and `(,beg . ,_end)
                                       (guard (integerp beg)))
                                  (goto-char beg)
                                  (push (line-number-at-pos) candidates))
                                 ((and `(,string . ,pos)
                                       (guard (stringp string)))
                                  (goto-char (abs pos))
                                  (push (line-number-at-pos) candidates)))))
                           (mapcar (lambda (line)
                                     (goto-line line)
                                     (let ((beg (line-beginning-position))
                                           (end (line-end-position)))
                                       (consult--location-candidate
                                        (consult--buffer-substring beg end)
                                        (cons (current-buffer) beg) line line)))
                                   (nreverse (seq-uniq candidates
                                                       (lambda (l1 l2)
                                                         (< (abs (- l1 l2)) 5)))))))))
      (consult--read
       candidates
       :prompt "Go to edit: "
       :annotate (consult--line-prefix curr-line)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last `isearch-string' to future history
       :add-history (list (thing-at-point 'symbol) isearch-string)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       :default (car candidates)
       ;; Add `isearch-string' as initial input if starting from Isearch
       :state (consult--location-state candidates))))
  (keymap-set search-map "/" 'consult-goto-edit)

  (with-eval-after-load 'ibuffer
    (defun conn-consult-line-multi-ibuffer-marked ()
      (interactive)
      (consult-line-multi
       `(:include
         ,(mapcar (lambda (buf)
                    (regexp-quote (buffer-name buf)))
                  (ibuffer-get-marked-buffers)))))
    (keymap-set ibuffer-mode-map "M-s j" 'conn-consult-line-multi-ibuffer-marked))

  (with-eval-after-load 'dired
    (defun consult-ripgrep-dired-marked-files ()
      (interactive)
      (consult-ripgrep (dired-get-marked-files)))
    (keymap-set dired-mode-map "M-s r" 'consult-ripgrep-dired-marked-files))

  (with-eval-after-load 'projectile
    (setq consult-project-function (lambda (_) (projectile-project-root))))

  (with-eval-after-load 'embark
    (with-eval-after-load 'org
      (defun embark-consult-grep-link (cand)
        (when cand
          (let* ((file-end (next-single-property-change 0 'face cand))
                 (line-end (next-single-property-change (1+ file-end) 'face cand))
                 (file (expand-file-name (substring-no-properties cand 0 file-end)))
                 (line (substring-no-properties cand (1+ line-end)))
                 (line (if (and (string-match-p ".*\\.org" file)
                                (equal ?\* (aref line 0)))
                           (substring line (1- (seq-position line ?\ )))
                         line))
                 (desc (read-string "Description: " line))
                 (fmt (if (string= desc "")
                          "[[file:%s::%s]]"
                        "[[file:%s::%s][%s]]")))
            (insert (format fmt file line desc)))))

      (define-keymap
        :keymap embark-consult-grep-map
        "M-RET" 'embark-consult-grep-link)

      (defun consult-org-link-location (cand)
        (let* ((loc (car-safe (consult--get-location cand)))
               (link (save-excursion
                       (goto-char loc)
                       (org-store-link nil))))
          (insert link)))

      (define-keymap
        :keymap embark-consult-location-map
        "M-RET" 'consult-org-link-location)))

  (with-eval-after-load 'conn-mode
    ;; (keymap-set conn-common-map "," 'consult-line)
    (keymap-set conn-misc-edit-map "e" 'consult-keep-lines)))

;;;;; consult-dir

(elpaca consult-dir
  (keymap-global-set "C-x C-d" 'consult-dir)

  (with-eval-after-load 'conn-mode
    (keymap-set conn-state-map "F" 'consult-dir))

  (with-eval-after-load 'vertico
    (define-keymap
      :keymap vertico-map
      "C-x y" 'consult-dir
      "C-x C-j" 'consult-dir-jump-file)))

;;;;; consult-lsp

(elpaca consult-lsp
  (with-eval-after-load 'lsp-mode
    (keymap-set lsp-mode-map "M-s ," #'consult-lsp-symbols)
    (keymap-set lsp-mode-map "M-s >" #'consult-lsp-diagnostics)
    (keymap-set lsp-mode-map "M-s <" #'consult-lsp-file-symbols)))

;;;;; consult-extras

(elpaca (consult-extras :host codeberg :repo "crcs/consult-extras")
  (keymap-global-set "C-h o" 'consult-symbol)
  (keymap-set goto-map "y" 'consult-all-marks)
  (with-eval-after-load 'consult
    (require 'consult-extras)))

;;;;; consult-project-extras

(elpaca (consult-project-extras :host github
                                :repo "Qkessler/consult-project-extra"
                                :files (:defaults "consult-project-extra.el")
                                :main "consult-project-extra.el")
  (keymap-global-set "C-c j" 'consult-project-extra-find))


;;;; vertico

(elpaca (vertico :files (:defaults "extensions/*"))
  (setq resize-mini-windows t
        vertico-preselect 'first
        vertico-buffer-hide-prompt nil
        vertico-cycle t
        vertico-buffer-display-action '(display-buffer-reuse-mode-window
                                        (mode . minibuffer-mode))
        vertico-multiform-categories '((t buffer)))

  (face-spec-set 'vertico-current
                 '((t :inherit region)))
  (face-spec-set 'vertico-group-title
                 '((t :inherit modus-themes-heading-0 :italic t :bold t)))

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)

  (defun vertico-buffer--redisplay-ad (win)
    (let ((mbwin (active-minibuffer-window)))
      (when (and mbwin vertico-buffer-mode
                 (eq (window-buffer mbwin) (current-buffer))
                 (not (eq win mbwin))
                 ;; Without this check we would be running this
                 ;; in any vertico-posframe windows every time.
                 (not (equal "posframe" (frame-parameter (window-frame win) 'title))))
        (setq-local vertico-count (- (/ (window-pixel-height win)
                                        (default-line-height))
                                     2)))))
  (advice-add 'vertico-buffer--redisplay :after 'vertico-buffer--redisplay-ad)

  (defun vertico--display-count-ad ()
    (when vertico-flat-mode
      (overlay-put vertico--count-ov 'before-string "")))
  (advice-add 'vertico--display-count :before-until #'vertico--display-count-ad)

  ;; I prefer it if the vertico buffer mode-line face
  ;; is not remapped to always appear active.
  (defun vertico-buffer--setup-ad ()
    "Setup buffer display."
    (let* ((action vertico-buffer-display-action)
           (old-wins (mapcar (lambda (w) (cons w (window-buffer w))) (window-list)))
           win old-buf tmp-buf
           (_ (unwind-protect
                  (progn
                    (with-current-buffer
                        (setq tmp-buf (generate-new-buffer "*vertico-buffer*"))
                      ;; Set a fake major mode such that
                      ;; `display-buffer-reuse-mode-window' does not take over!
                      (setq major-mode 'vertico-buffer-mode))
                    ;; Temporarily select the original window such that
                    ;; `display-buffer-same-window' works.
                    (setq win (with-minibuffer-selected-window
                                (display-buffer tmp-buf action))
                          old-buf (alist-get win old-wins))
                    (set-window-buffer win (current-buffer)))
                (kill-buffer tmp-buf)))
           (old-no-other (window-parameter win 'no-other-window))
           (old-no-delete (window-parameter win 'no-delete-other-windows))
           (old-state (buffer-local-set-state
                       cursor-in-non-selected-windows cursor-in-non-selected-windows
                       show-trailing-whitespace nil
                       truncate-lines t
                       ;; face-remapping-alist (copy-tree `((mode-line-inactive mode-line)
                       ;;					 ,@face-remapping-alist))
                       mode-line-format
                       (list (format  #(" %s%s " 1 3 (face mode-line-buffer-id))
                                      (replace-regexp-in-string ":? *\\'" ""
                                                                (minibuffer-prompt))
                                      (let ((depth (recursion-depth)))
                                        (if (< depth 2) "" (format " [%s]" depth)))))
                       vertico-count (- (/ (window-pixel-height win)
                                           (default-line-height)) 2))))
      (set-window-parameter win 'no-other-window t)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-dedicated-p win t)
      (overlay-put vertico--candidates-ov 'window win)
      (when (and vertico-buffer-hide-prompt vertico--count-ov)
        (overlay-put vertico--count-ov 'window win))
      (setq-local vertico-buffer--restore (make-symbol "vertico-buffer--restore"))
      (fset vertico-buffer--restore
            (lambda ()
              (with-selected-window (active-minibuffer-window)
                (when vertico-buffer--restore
                  (when transient-mark-mode
                    (with-silent-modifications
                      (vertico--remove-face (point-min) (point-max) 'region)))
                  (remove-hook 'pre-redisplay-functions #'vertico-buffer--redisplay 'local)
                  (remove-hook 'minibuffer-exit-hook vertico-buffer--restore)
                  (fset vertico-buffer--restore nil)
                  (kill-local-variable 'vertico-buffer--restore)
                  (buffer-local-restore-state old-state)
                  (overlay-put vertico--candidates-ov 'window nil)
                  (when vertico--count-ov (overlay-put vertico--count-ov 'window nil))
                  (cond
                   ((and (window-live-p win) (buffer-live-p old-buf))
                    (set-window-parameter win 'no-other-window old-no-other)
                    (set-window-parameter win 'no-delete-other-windows old-no-delete)
                    (set-window-dedicated-p win nil)
                    (set-window-buffer win old-buf))
                   ((window-live-p win)
                    (delete-window win)))
                  (when vertico-buffer-hide-prompt
                    (set-window-vscroll nil 0))))))
      ;; We cannot use a buffer-local minibuffer-exit-hook here.  The hook will
      ;; not be called when abnormally exiting the minibuffer from another buffer
      ;; via `keyboard-escape-quit'.
      (add-hook 'minibuffer-exit-hook vertico-buffer--restore)
      (add-hook 'pre-redisplay-functions #'vertico-buffer--redisplay nil 'local)))
  (advice-add 'vertico-buffer--setup :override #'vertico-buffer--setup-ad)

  (defun vertico--display-count-ad ()
    (when vertico-flat-mode
      (overlay-put vertico--count-ov 'before-string "")))
  (advice-add 'vertico--display-count :before-until #'vertico--display-count-ad)

  ;; Refocus the minibuffer if vertico-repeat is called with a minibuffer open.
  (defun vertico-repeat-ad (&rest _)
    (when (> (minibuffer-depth) 0)
      (select-window
       (if (and (equal (selected-window) (minibuffer-window))
                (not (with-current-buffer
                         (window-buffer (minibuffer-selected-window))
                       (eq major-mode 'minibuffer-mode))))
           (minibuffer-selected-window)
         (minibuffer-window)))
      (message "Switched to *MINIBUFFER*")
      t))
  (advice-add 'vertico-repeat :before-until #'vertico-repeat-ad)

  (defun vertico-focus-selected-window ()
    (interactive)
    (select-window (minibuffer-selected-window))
    (message "Focused other window"))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (keymap-global-set "<f1>" 'vertico-repeat)

  (keymap-unset vertico-map "C-j")

  (define-keymap
    :keymap vertico-map
    "M-i" #'vertico-insert
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    "C-<backspace>" #'vertico-directory-delete-word
    "C-DEL" #'vertico-directory-delete-word
    "M-<backspace>" #'vertico-directory-up
    "M-DEL" #'vertico-directory-up
    "M-RET" #'vertico-exit-input
    "C-M-j" #'vertico-exit-input
    "C-M-<return>" #'vertico-exit-input
    "C-j" #'vertico-quick-jump
    "C-S-j" #'vertico-quick-exit
    "C-w" #'my/vertico-copy-or-kill)

  (defun my/vertico-copy-or-kill (beg end)
    (interactive (list (region-beginning) (region-end)))
    (if (or (use-region-p) (not transient-mark-mode))
        (call-interactively #'kill-region)
      (kill-new (let ((cand (vertico--candidate)))
                  (if (consult--tofu-p (aref cand (1- (length cand))))
                      (substring cand 0 -1)
                    cand))))))


;;;; marginalia

(elpaca marginalia
  (marginalia-mode 1)

  (setq marginalia-align 'column)

  (keymap-global-set "M-A" 'marginalia-cycle)
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)

  (defun marginalia-annotate-alias (cand)
    "Annotate CAND with the function it aliases."
    (when-let ((sym (intern-soft cand))
               (alias (car (last (function-alias-p sym t))))
               (name (and (symbolp alias) (symbol-name alias))))
      (format #(" (%s)" 1 5 (face marginalia-function)) name)))

  (defun my/marginalia-annotate-binding (cand)
    "Annotate command CAND with keybinding."
    (when-let ((sym (intern-soft cand))
               (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
      (format #(" {%s}" 1 5 (face marginalia-key)) (key-description key))))

  (defun marginalia-annotate-command-with-alias (cand)
    "Annotate command CAND with its documentation string.
    Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let (sym (intern-soft cand))
      (concat
       (my/marginalia-annotate-binding cand)
       (marginalia-annotate-alias cand)
       (marginalia--documentation (marginalia--function-doc sym)))))
  (cl-pushnew #'marginalia-annotate-command-with-alias
              (alist-get 'command marginalia-annotator-registry))

  (defvar marginalia-align-column 40)

  ;; Try to align to a specific column and if candidate is too long align
  ;; on multiple of marginalia--cand-width-step. This prevents very long
  ;; candidates from pushing all annotations almost entirely out of view.
  (defun marginalia--align-column (cands)
    "Align annotations of CANDS according to `marginalia-align'."
    (cl-loop
     for (cand . ann) in cands do
     (when-let (align (text-property-any 0 (length ann) 'marginalia--align t ann))
       (setq marginalia--cand-width-max
             (max marginalia--cand-width-max
                  (* (ceiling (+ (string-width cand)
                                 (compat-call string-width ann 0 align))
                              marginalia--cand-width-step)
                     marginalia--cand-width-step)))))
    (cl-loop
     for (cand . ann) in cands collect
     (progn
       (when-let (align (text-property-any 0 (length ann) 'marginalia--align t ann))
         (put-text-property
          align (1+ align) 'display
          `(space :align-to
                  ,(pcase-exhaustive marginalia-align
                     ('center `(+ center ,marginalia-align-offset))
                     ('left `(+ left ,(+ marginalia-align-offset marginalia--cand-width-max)))
                     ('right `(+ right ,(+ marginalia-align-offset 1
                                           (- (compat-call string-width ann 0 align)
                                              (string-width ann)))))
                     ('column `(+ left ,(+ marginalia-align-offset
                                           (max marginalia-align-column
                                                (* (ceiling (+ (string-width cand)
                                                               (compat-call string-width ann 0 align))
                                                            marginalia--cand-width-step)
                                                   marginalia--cand-width-step)))))))
          ann))
       (list cand "" ann))))
  (advice-add 'marginalia--align :override 'marginalia--align-column))


;;;; tempel

(elpaca tempel
  (keymap-global-set "M-i" 'tempel-complete)
  (keymap-global-set "M-I" 'tempel-insert-region)

  (defun tempel-insert-region ()
    (interactive)
    (require 'tempel)
    (activate-mark t)
    (unwind-protect
        (call-interactively 'tempel-insert)
      (deactivate-mark)))

  (with-eval-after-load 'tempel
    (keymap-set tempel-map "M-i" 'tempel-next)
    (keymap-set tempel-map "M-I" 'tempel-previous)

    (setq tempel-path "/home/dave/.emacs.d/templates/*.eld")

    (defun conn-tempel-insert-ad (fn &rest args)
      (apply fn args)
      (when tempel--active (emacs-state)))
    (advice-add 'tempel-insert :around 'conn-tempel-insert-ad)))

;;;;; tempel-collection

(elpaca tempel-collection)

;; 
;; ;;;; repeat-help

;; (elpaca repeat-help
;;   (setq repeat-help-auto t)
;;   (add-hook 'repeat-mode-hook 'repeat-help-mode)
;;   (repeat-help-mode 1))


;;;; vundo

(elpaca vundo
  (keymap-global-set "C-x u" 'vundo)
  (with-eval-after-load 'vundo
    (setq vundo-glyph-alist vundo-unicode-symbols)))


;;;; htmlize

(elpaca htmlize)


;;;; page-break-lines

(elpaca page-break-lines
  (setq page-break-lines-max-width 72)
  (global-page-break-lines-mode)
  (with-eval-after-load 'diminish
    (diminish 'page-break-lines-mode)))


;;;; sage-shell-mode

(elpaca sage-shell-mode
  (with-eval-after-load 'sage-shell-mode
    (setq sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history")
    (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)))


;;;; tuareg

(elpaca tuareg)


;;;; rfc-mode

(elpaca rfc-mode)


;;;; heex-ts-mode

(elpaca heex-ts-mode)


;;;; polymode

(elpaca polymode)


;;;; visual-fill-column

;; (elpaca visual-fill-column
;;   (add-hook 'text-mode-hook 'visual-fill-column-mode))


;;;; denote

(elpaca (denote :files (:defaults "denote-org-extras.el"))
  (with-eval-after-load 'denote
    (require 'denote-org-extras)
    (denote-rename-buffer-mode 1))

  (keymap-global-set "C-c n e" #'denote-org-extras-extract-org-subtree)
  (keymap-global-set "C-c n d" #'denote-dired-directory)
  (keymap-global-set "C-c n n" #'denote)
  (keymap-global-set "C-c n s" #'denote-signature)
  (keymap-global-set "C-c n a" #'denote-keywords-add)
  (keymap-global-set "C-c n r" #'denote-keywords-remove)
  (keymap-global-set "C-c n w" #'denote-rename-file)
  (keymap-global-set "C-c n b" #'denote-backlinks)
  (keymap-global-set "C-c n B" #'denote-find-backlink)
  (keymap-global-set "C-c n l" #'denote-find-link)

  (defun denote-dired-directory ()
    (interactive)
    (dired denote-directory))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  (defun denote-backlinks-file (file)
    (when (denote-file-is-writable-and-supported-p file)
      (let* ((id (denote-retrieve-filename-identifier-with-error file))
             (xref-show-xrefs-function #'denote-link--prepare-backlinks)
             (project-find-functions #'denote-project-find))
        (xref--show-xrefs
         (apply-partially #'xref-matches-in-files id
                          (denote-directory-files nil :omit-current :text-only))
         nil))))

  (with-eval-after-load 'denote
    (require 'denote-silo-extras)
    (setq org-agenda-files (append denote-silo-extras-directories
                                   org-agenda-files))))

;;;;; consult notes

(elpaca consult-notes
  (with-eval-after-load 'denote
    (consult-notes-denote-mode 1)

    ;; Less extra spacing between candidates and annotation
    (setf (plist-get consult-notes-denote--source :items)
          (lambda ()
            (let* ((max-width 0)
                   (cands (mapcar (lambda (f)
                                    (let* ((id (denote-retrieve-filename-identifier f))
                                           (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f))
                                                        (denote-retrieve-filename-title f)))
                                           (title (if consult-notes-denote-display-id
                                                      (concat id " " title-1)
                                                    title-1))
                                           (dir (file-relative-name (file-name-directory f) denote-directory))
                                           (keywords (denote-extract-keywords-from-path f)))
                                      (let ((current-width (string-width title)))
                                        (when (> current-width max-width)
                                          (setq max-width (+ 5 current-width))))
                                      (propertize title 'denote-path f 'denote-keywords keywords)))
                                  (funcall consult-notes-denote-files-function))))
              (mapcar (lambda (c)
                        (let* ((keywords (get-text-property 0 'denote-keywords c))
                               (path (get-text-property 0 'denote-path c))
                               (dirs (thread-first
                                       (file-name-directory path)
                                       (file-relative-name denote-directory)
                                       (directory-file-name))))
                          (concat c
                                  ;; align keywords
                                  (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                  "  "
                                  (format "%18s"
                                          (if keywords
                                              (concat (propertize "#" 'face 'consult-notes-name)
                                                      (propertize (mapconcat 'identity keywords " ")
                                                                  'face 'consult-notes-name))
                                            ""))
                                  (when consult-notes-denote-dir
                                    (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
                      cands)))))

  (defun consult-denote ()
    (interactive)
    (require 'denote)
    (require 'consult-notes)
    (consult--read
     (funcall (plist-get consult-notes-denote--source :items))
     :category 'consult-denote
     :lookup #'consult--lookup-member
     :annotate (plist-get consult-notes-denote--source :annotate)
     :prompt "Denotes: "
     :state (consult-notes-denote--state)
     :preview-key 'any
     :history 'consult-denote-history
     :add-history (seq-some #'thing-at-point '(region symbol))
     :require-match t))
  (keymap-global-set "C-c n f" 'consult-denote)

  (defun consult-denote-headings (files)
    (interactive)
    (let ((builder (consult--ripgrep-make-builder
                    (mapcar #'consult-notes-denote--file files))))
      (consult--read
       (consult--async-command builder
         (consult--grep-format builder))
       :preview-key 'any
       :prompt "Heading: "
       :lookup #'consult--lookup-member
       :state (consult--grep-state)
       :add-history (thing-at-point 'symbol)
       :require-match t
       :initial "#/^[*]+#"
       :category 'consult-denote-heading
       :group #'consult--prefix-group
       :history '(:input consult--note-history)
       :sort nil)))

  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions 'consult-denote-headings)

    (setf (alist-get 'consult-denote embark-keymap-alist)
          '(embark-consult-denote-map))

    (defun embark-export-notes (notes)
      "Create a Dired buffer listing NOTES."
      (embark-export-dired (mapcar #'consult-notes-denote--file notes))
      (denote-dired-mode 1))

    (setf (alist-get 'consult-denote embark-exporters-alist)
          #'embark-export-notes)

    (defvar-keymap embark-consult-denote-map
      :parent embark-general-map
      "M-RET" 'denote-link
      "h" 'consult-denote-headings
      "E" 'embark-export)

    (defun embark-consult-denote-heading-link (cand)
      (when-let (cand
                 (file-end (next-single-property-change 0 'face cand))
                 (line-end (next-single-property-change (1+ file-end) 'face cand))
                 (file (expand-file-name (substring-no-properties cand 0 file-end)))
                 (file-text (denote--link-get-description file))
                 (line (string-to-number (substring-no-properties cand (1+ file-end) line-end)))
                 (heading-data (denote-org-extras--get-heading-and-id-from-line line file))
                 (heading-text (car heading-data))
                 (heading-id (cdr heading-data))
                 (description (denote-link-format-heading-description file-text heading-text)))
        (insert (denote-org-extras-format-link-with-heading file heading-id description))))

    (defvar-keymap embark-consult-denote-heading-map
      :parent embark-consult-grep-map
      "M-RET" 'embark-consult-denote-heading-link)

    (setf (alist-get 'consult-denote-heading embark-keymap-alist)
          (list 'embark-consult-denote-heading-map))))


;;;; tab bookmark

(elpaca (tab-bookmark :host github
                      :repo "minad/tab-bookmark")
  (define-keymap
    :keymap bookmark-map
    "v b" #'tab-bookmark
    "v r" #'tab-bookmark-rename
    "v s" #'tab-bookmark-save
    "v o" #'tab-bookmark-open
    "v d" #'tab-bookmark-delete
    "v k" #'tab-bookmark-push
    "v i" #'tab-bookmark-pop))


;;;; diff-hl

(elpaca diff-hl
  (run-with-timer
   1 nil
   (lambda ()
     (global-diff-hl-mode 1)
     (add-hook 'dired-mode-hook #'diff-hl-dired-mode))))


;;;; teco

;; (elpaca teco)


;;;; dumb-jump

(elpaca dumb-jump
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))


;;;; jinx

(elpaca jinx
  (keymap-global-set "<remap> <ispell-word>" #'jinx-correct))


;;;; hyperbole

(elpaca hyperbole
  (require 'embark)
  ;; (require 'conn-mode)
  (hyperbole-mode 1)

  (remove-hook 'temp-buffer-show-hook #'hkey-help-show)

  (defvar hyperbole-embark-target-finders)

  (setq hyperbole-mode-lighter " Hy"
        hyrolo-file-list (list (expand-file-name "var/hyperbole/rolo.org" user-emacs-directory))
        action-key-default-function #'action-key-error
        assist-key-default-function #'assist-key-error
        smart-scroll-proportional nil
        hpath:display-where 'this-window
        hyperbole-embark-target-finders '(embark--vertico-selected
                                          embark-target-top-minibuffer-candidate
                                          embark-target-active-region
                                          embark-org-target-link
                                          embark-org-target-element-context
                                          embark-org-target-agenda-item
                                          embark-target-collect-candidate
                                          embark-target-completion-list-candidate
                                          embark-target-flymake-at-point
                                          embark-target-smerge-at-point
                                          embark-target-package-at-point
                                          embark-target-email-at-point
                                          embark-target-url-at-point
                                          embark-target-file-at-point
                                          embark-target-defun-looking-at
                                          embark-target-custom-variable-at-point
                                          embark-target-identifier-at-point
                                          embark-target-guess-file-at-point
                                          embark-target-expression-at-point
                                          embark-looking-at-page-target-finder)
        action-key-eol-function #'ignore
        assist-key-eol-function #'ignore
        ;; Remove items I want embark to handle instead.
        hkey-alist '(;; Company completion mode
                     ((and (boundp 'company-active-map)
                           (memq company-active-map (current-minor-mode-maps)))
                      . ((smart-company-to-definition) . (smart-company-help)))
                     ;;
                     ;; Treemacs hierarchical file manager
                     ((eq major-mode 'treemacs-mode)
                      . ((smart-treemacs) . (smart-treemacs)))
                     ;;
                     ;; dired-sidebar-mode
                     ((eq major-mode 'dired-sidebar-mode)
                      . ((smart-dired-sidebar) . (smart-dired-sidebar)))
                     ;;
                     ((and (eq major-mode 'ert-results-mode)
                           (featurep 'ert-results)
                           (setq hkey-value (ert-results-filter-status-p)))
                      . ((smart-ert-results hkey-value) . (smart-ert-results-assist hkey-value)))
                     ;;
                     ;;
                     ;; Handle Emacs push buttons in buffers
                     ((and (fboundp 'button-at) (button-at (point)))
                      . ((smart-push-button nil (mouse-event-p last-command-event))
                         . (smart-push-button-help nil (mouse-event-p last-command-event))))
                     ;;
                     ;; Smart end of line
                     ;; ((smart-eolp)
                     ;;  . ((funcall action-key-eol-function) . (funcall assist-key-eol-function)))
                     ;;
                     ;; Handle any Org mode-specific contexts but give priority to Hyperbole
                     ;; buttons prior to cycling Org headlines
                     ((and (not (hyperb:stack-frame '(smart-org)))
                           (let ((hrule:action #'actype:identity))
                             (smart-org)))
                      . ((smart-org) . (smart-org)))
                     ;;
                     ;; If in an xref buffer on a listing of matching identifier lines, go to
                     ;; the source line referenced by the current entry.
                     ((and (fboundp 'xref--item-at-point) (xref--item-at-point))
                      . ((xref-goto-xref) . (xref-show-location-at-point)))
                     ;;
                     ;; The Smart Menu system is an attractive in-buffer menu system
                     ;; that works on any display system that supports Emacs.  It
                     ;; predates Emacs' menu systems; it is a part of InfoDock.
                     ;; It is not included with Hyperbole but is compatible with the
                     ;; Smart Keys.
                     ;;
                     ;; This selects or gives help for a menu item.
                     ((eq major-mode 'smart-menu-mode)
                      . ((smart-menu-select) . (smart-menu-help)))
                     ;;
                     ((derived-mode-p 'dired-mode)
                      . ((smart-dired) . (smart-dired-assist)))
                     ;;
                     ((string-prefix-p "magit-" (symbol-name major-mode))
                      . ((smart-magit) . (smart-magit-assist)))
                     ;;
                     ;; If on a Hyperbole button, perform action or give help.
                     ((hbut:at-p)
                      . ((hui:hbut-act 'hbut:current) . (hui:hbut-help 'hbut:current)))
                     ;;
                     ;; View minor mode
                     ((if (boundp 'view-minor-mode) view-minor-mode)
                      . ((cond ((last-line-p)
                                (view-quit))
                               ((pos-visible-in-window-p (point-max))
                                (goto-char (point-max)))
                               (t (scroll-up)))
                         . (scroll-down)))
                     ;;
                     ;; View major mode
                     ((eq major-mode 'view-mode) .
                      ((View-scroll-lines-forward) . (View-scroll-lines-backward)))
                     ;;
                     ((eq major-mode 'occur-mode)
                      . ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
                     ;;
                     ((eq major-mode 'moccur-mode)
                      . ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
                     ((eq major-mode 'amoccur-mode)
                      . ((amoccur-mode-goto-occurrence) . (amoccur-mode-goto-occurrence)))
                     ;;
                     ((eq major-mode 'kotl-mode)
                      . ((kotl-mode:action-key) . (kotl-mode:assist-key)))
                     ;;
                     ;; If in the flymake linter list of issues buffer, jump to or show issue at point
                     ((eq major-mode 'flymake-diagnostics-buffer-mode)
                      . ((flymake-goto-diagnostic (point)) . (flymake-show-diagnostic (point) t)))
                     ;;
                     ;; Handle widgets in Custom-mode
                     ((eq major-mode 'Custom-mode)
                      . ((smart-custom) . (smart-custom-assist)))
                     ;;
                     ;; Emacs bookmarks menu (bookmark.el)
                     ((eq major-mode 'bookmark-bmenu-mode)
                      . ((bookmark-jump (bookmark-bmenu-bookmark) (hpath:display-buffer-function))
                         .
                         ;; Below we want the Assist Key to show what the Action Key does.
                         (hkey-help)))
                     ;;
                     ;; Pages directory listing mode (page-ext.el)
                     ((eq major-mode 'pages-directory-mode)
                      . ((pages-directory-goto) . (pages-directory-goto)))
                     ;;
                     ;; Imenu listing in GNU Emacs
                     ((smart-imenu-item-at-p)
                      . ((smart-imenu-display-item-where (car hkey-value) (cdr hkey-value)) .
                         (imenu-choose-buffer-index)))
                     ;;
                     ((eq major-mode 'calendar-mode)
                      . ((smart-calendar) . (smart-calendar-assist)))
                     ;;
                     ;; Part of InfoDock
                     ((eq major-mode 'unix-apropos-mode)
                      . ((smart-apropos) . (smart-apropos-assist)))
                     ;;
                     ((eq major-mode 'outline-mode)
                      . ((smart-outline) . (smart-outline-assist)))
                     ;;
                     ((eq major-mode 'Info-mode)
                      . ((smart-info) .  (smart-info-assist)))
                     ;;
                     ((if (boundp 'hmail:reader)
                          (or (eq major-mode hmail:reader)
                              (eq major-mode hmail:lister)))
                      . ((smart-hmail) . (smart-hmail-assist)))
                     ;;
                     ((eq major-mode 'gnus-group-mode)
                      (smart-gnus-group) . (smart-gnus-group-assist))
                     ;;
                     ((eq major-mode 'gnus-summary-mode)
                      . ((smart-gnus-summary) . (smart-gnus-summary-assist)))
                     ;;
                     ((eq major-mode 'gnus-article-mode)
                      . ((smart-gnus-article) . (smart-gnus-article-assist)))
                     ;;
                     ((eq major-mode 'Buffer-menu-mode)
                      . ((smart-buffer-menu) . (smart-buffer-menu-assist)))
                     ;;
                     ((eq major-mode 'ibuffer-mode)
                      . ((smart-ibuffer-menu) . (smart-ibuffer-menu-assist)))
                     ;;
                     ((eq major-mode 'tar-mode)
                      . ((smart-tar) . (smart-tar-assist)))
                     ;;
                     ;; Follow references in man pages.
                     ((setq hkey-value (smart-man-entry-ref))
                      . ((smart-man-display hkey-value) . (smart-man-display hkey-value)))
                     ;;
                     ((eq major-mode 'w3-mode)
                      . ((w3-follow-link) . (w3-goto-last-buffer)))
                     ;;
                     ((eq major-mode 'hyrolo-mode)
                      . ((smart-hyrolo) . (smart-hyrolo-assist)))
                     ;;
                     ((eq major-mode 'image-dired-thumbnail-mode)
                      . ((smart-image-dired-thumbnail) . (smart-image-dired-thumbnail-assist)))
                     ;;
                     ;; Gomoku game
                     ((eq major-mode 'gomoku-mode)
                      . ((gomoku-human-plays) . (gomoku-human-takes-back)))
                     ;;
                     ;; Embark-dwim
                     ((setq hkey-value
                            (let* ((embark-target-finders hyperbole-embark-target-finders))
                              (car (embark--targets))))
                      . ((embark-smart-action) . (embark-smart-assist)))
                     ;;
                     ;; Todotxt
                     ((eq major-mode 'todotxt-mode)
                      . ((smart-todotxt) . (smart-todotxt-assist)))
                     ;;
                     ;; Outline minor mode
                     ((and (boundp 'outline-minor-mode) outline-minor-mode)
                      . ((smart-outline) . (smart-outline-assist)))))

  (defun embark-target-defun-looking-at ()
    (pcase (embark-target-defun-at-point)
      ((and target `(,_ ,_ ,beg . ,end)
            (guard (= beg (point))))
       target)))

  (defun embark-smart-action ()
    (interactive)
    (let* ((target hkey-value)
           (type (plist-get target :type))
           (default-action (embark--default-action type))
           (action (or (command-remapping default-action) default-action)))
      (unless action
        (user-error "No default action for %s targets" type))
      (when (and current-prefix-arg (minibufferp))
        (setq embark--toggle-quit t))
      (embark--act action
                   (if (and (eq default-action embark--command)
                            (not (memq default-action
                                       embark-multitarget-actions)))
                       (embark--orig-target target)
                     target)
                   (embark--quit-p action)))
    (setq hkey-value nil))

  (defun embark-smart-assist ()
    (interactive)
    (let* ((target hkey-value)
           (type (plist-get target :type))
           (default-action (conn-embark-alt--default-action type))
           (action (or (command-remapping default-action) default-action)))
      (unless action
        (user-error "No default action for %s targets" type))
      (when (and current-prefix-arg (minibufferp))
        (setq embark--toggle-quit t))
      (embark--act action
                   (if (and (eq default-action embark--command)
                            (not (memq default-action
                                       embark-multitarget-actions)))
                       (embark--orig-target target)
                     target)
                   (embark--quit-p action)))
    (setq hkey-value nil))

  (defib gh-issue-link ()
    "Link to a github issue."
    (when (org-in-regexp "gh:\\([a-zA-Z-]*/[a-zA-Z-]*\\)#\\([0-9]*\\)")
      (ibut:label-set (match-string 0))
      (hact 'www-url (concat "https://github.com/"
                             (match-string 1)
                             "/issues/"
                             (match-string 2)))))

  (defib gnu-bug-link ()
    "Link to a gnu bug."
    (when (org-in-regexp "bug#\\([0-9]*\\)")
      (ibut:label-set (match-string 0))
      (hact 'www-url (concat "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                             (match-string 1)))))

  (keymap-global-set "C-," 'action-key)
  (keymap-global-set "C-." 'assist-key)
  (keymap-set hyperbole-mode-map "C-c C-\\" #'hycontrol-windows-mode)
  (keymap-unset hyperbole-mode-map "C-c RET" t)
  (keymap-unset hyperbole-mode-map "M-RET" t)
  (keymap-unset hyperbole-mode-map "M-<return>" t)

  (keymap-set hycontrol-windows-mode-map ":" 'hycontrol-enable-frames-mode)
  (keymap-set hycontrol-frames-mode-map ":" 'hycontrol-enable-windows-mode)

  (dolist (state '(conn-state dot-state view-state org-tree-edit-state))
    (keymap-set (conn-get-mode-map state 'hyperbole-mode)
                ":" 'hycontrol-enable-windows-mode))

  (dolist (state '(conn-state org-tree-edit-state))
    (define-keymap
      :keymap (conn-get-mode-map state 'hyperbole-mode)
      "e" #'action-key
      "h" #'assist-key
      "H" #'hyperbole))

  (keymap-set (conn-get-mode-map 'view-state 'hyperbole-mode) "H" #'hyperbole))
