;;; -*- lexical-binding: t; eval: (outline-minor-mode 1); -*-

;;; Elpaca

(setq elpaca-core-date
      (list (string-to-number (format-time-string "%Y%m%d" emacs-build-time))))
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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

;;;; persist

(elpaca (persist :host github :repo "emacs-straight/persist"))

;;; Compat

(elpaca compat (require 'compat))

;;; Diminish

(elpaca diminish
  (require 'diminish)
  (diminish 'visual-line-mode))

;;; Wait

(elpaca-wait)

;;; Built-in

;;;; emacs

(setq fill-column 72
      minibuffer-default-prompt-format ""
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
      view-read-only t
      set-mark-command-repeat-pop t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      translate-upper-case-key-bindings nil
      show-paren-context-when-offscreen 'child-frame
      sentence-end-double-space nil
      tab-always-indent 'complete
      read-minibuffer-restore-windows nil
      dired-listing-switches "-alFh --dired --group-directories-first"
      isearch-lazy-count t
      isearch-yank-on-move t
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
      resize-mini-windows 'grow-only
      minibuffer-prompt-properties '(read-only t
                                               cursor-intangible t
                                               face minibuffer-prompt)
      yank-from-kill-ring-rotate nil
      exec-path (cons (expand-file-name "scripts/" user-emacs-directory) exec-path)
      edebug-inhibit-emacs-lisp-mode-bindings t
      bidi-inhibit-bpa t)

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

(define-key global-map [remap yank] 'yank-in-context)

(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(keymap-global-set "S-<backspace>" 'cycle-spacing)
(keymap-global-set "C-|"           'indent-relative)
(keymap-global-set "M-N"           'tab-bar-switch-to-next-tab)
(keymap-global-set "M-P"           'tab-bar-switch-to-prev-tab)
(keymap-global-set "C-:"           'read-only-mode)
(keymap-global-set "C-c e"         'eshell)
(keymap-global-set "C-x C-b"       'ibuffer)
(keymap-global-set "C-o"           goto-map)
(keymap-global-set "M-;"           'comment-line)
(keymap-global-set "C-c c"         'compile)
(keymap-global-set "M-W"           'other-window-prefix)
(keymap-global-set "M-F"           'other-frame-prefix)

(keymap-set text-mode-map "M-TAB" #'completion-at-point)
(keymap-set help-map "M-k" #'describe-keymap)

(define-keymap
  :keymap ctl-x-map
  "s"   'save-buffer
  "C-s" 'save-some-buffers
  "f"   'find-file
  "C-f" 'set-fill-column)

(define-keymap
  :keymap window-prefix-map
  "t" 'tab-detach
  "f" 'tear-off-window)

(define-keymap
  :keymap emacs-lisp-mode-map
  "C-c C-z" 'eval-buffer
  "C-c C-m" 'emacs-lisp-macroexpand
  "C-c C-e" 'eval-print-last-sexp
  "C-c C-f" 'find-function
  "C-c C-l" 'pp-eval-last-sexp
  "C-c C-r" 'eval-region)

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

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded %d packages in %s with %d garbage collections."
                     (cdar elpaca--status-counts)
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(find-function-setup-keys)

;;;; tab-bar-mode

(progn
  (setq tab-bar-show nil
        tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode 1)
  (tab-bar-history-mode 1)

  (with-eval-after-load 'conn-mode
    (defvar-keymap tab-bar-history-mode-repeat-map
      :repeat t
      "/" 'tab-bar-history-forward
      "?" 'tab-bar-history-back)

    (define-keymap
      :keymap tab-bar-history-mode-map
      "C-x 4 /" 'tab-bar-history-forward
      "C-x 4 ?" 'tab-bar-history-back)))

;;;; diary / calendar

(progn
  (require 'calendar)
  (keymap-global-set "<f5>" #'calendar)
  (setq diary-entry-marker 'highlight
        calendar-holiday-marker 'match)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t)

  (appt-activate 1))

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

(setq ispell-program-name "aspell"
      ispell-dictionary "american")

;;;; savehist

(progn
  (with-eval-after-load 'no-littering
    (require 'savehist)

    (setq savehist-additional-variables '(projectile-project-command-history
                                          ;; file-name-history
                                          ;; recentf-list
                                          search-ring
                                          regexp-search-ring
                                          register-alist)
          savehist-file (expand-file-name "var/savehist/hist" user-emacs-directory))

    (savehist-mode 1)))

;;;; repeat

(progn
  (setq repeat-check-key t
        repeat-exit-timeout nil
        repeat-echo-function 'repeat-echo-message
        repeat-keep-prefix nil
        repeat-on-final-keystroke t)

  (repeat-mode 1)

  (keymap-global-set "C-x c" 'repeat))

;;;; autorevert

(progn
  (setq auto-revert-interval .01)
  (global-auto-revert-mode 1)
  (diminish 'auto-revert-mode))

;;;; face-remap

(with-eval-after-load 'face-remap
  (diminish 'buffer-face-mode))

;;;; recentf

(with-eval-after-load 'no-littering
  (require 'recentf)

  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15)

  (recentf-mode 1)

  (defvar david-recentf-autosave
    (run-with-idle-timer 4 t (lambda ()
                               (let ((inhibit-message t))
                                 (when recentf-mode
                                   (recentf-save-list))))))

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;;; outline

(progn
  (keymap-global-set "C-c L" 'outline-minor-mode)
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode)))

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

(progn
  (diminish 'eldoc-mode)

  (setq eldoc-echo-area-prefer-doc-buffer t))

;;; Packages

;;;; benchmark-init

;; (elpaca benchmark-init
;;   (require 'benchmark-init)
;;   (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

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

  (with-eval-after-load 'embark
    (keymap-set embark-region-map "N" 'ni-narrow-to-region-indirect-other-window)
    (keymap-set embark-defun-map  "N" 'ni-narrow-to-defun-indirect-other-window)
    (keymap-set embark-alt-page-map "M-RET" 'ni-narrow-to-page-indirect-other-window)))

;;;; paredit

(elpaca (paredit :host github :repo "emacsmirror/paredit")
  (require 'paredit)

  (dolist (mode '(lisp-data-mode-hook
                  eshell-mode-hook
                  sly-mrepl-mode-hook
                  slime-repl-mode-hook))
    (add-hook mode #'enable-paredit-mode))

  (add-hook 'paredit-mode-hook #'paredit-disable-electric-pair)

  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)

  (diminish 'paredit-mode)

  (keymap-unset paredit-mode-map "RET")
  (keymap-unset paredit-mode-map "M-s")
  (keymap-unset paredit-mode-map "M-;")
  (keymap-set   paredit-mode-map "M-l" 'paredit-splice-sexp)

  (keymap-set paredit-mode-map "C-w" 'paredit-kill-region)

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
    (electric-pair-local-mode -1))

  (with-eval-after-load 'conn-mode
    (define-conn-mode-map
     'conn-state 'paredit-mode
     (define-keymap
       "C-<backspace>" 'paredit-backward-kill-word
       "M-DEL"         'paredit-backward-kill-word
       "DEL"           'paredit-backward-delete))

    (define-conn-mode-map
     '(conn-state dot-state) 'paredit-mode
     (define-keymap
       "m" 'paredit-forward
       "n" 'paredit-backward
       "O" 'paredit-forward-up
       "U" 'paredit-backward-up))

    (conn-add-thing-movement-command 'sexp 'paredit-forward)
    (conn-add-thing-movement-command 'sexp 'paredit-backward)))

;;;; slime

(elpaca slime
  (run-with-idle-timer 3 nil (lambda () (require 'slime)))
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
  (keymap-global-set "C-c s" 'lsp)

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
                                  "--header-insertion=never"))
  (setq lsp-zig-zls-executable "~/build/zls/zig-out/bin/zls")

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
  (run-with-idle-timer 4 nil (lambda () (require 'org)))

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

    (with-eval-after-load 'conn-mode
      (conn-add-thing-movement-command 'org-paragraph 'org-forward-paragraph)
      (conn-add-thing-movement-command 'org-paragraph 'org-backward-paragraph)
      (put 'org-paragraph 'forward-op 'org-forward-paragraph))

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
  ;; (dtrt-indent-global-mode 1)
  (diminish 'dtrt-indent-mode))

;;;; exec-path-from-shell

;; (elpaca exec-path-from-shell
;;   (when (memq window-system '(mac ns x))
;;     (require 'exec-path-from-shell)
;;     (exec-path-from-shell-initialize)))

;;;; modus-themes

(elpaca modus-themes
  (require 'modus-themes)

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

  (load-theme 'modus-operandi-tinted t)

  (with-eval-after-load 'hi-lock
    (setq hi-lock-face-defaults '("modus-themes-subtle-cyan"
                                  "modus-themes-subtle-red"
                                  "modus-themes-subtle-green"
                                  "modus-themes-subtle-blue"
                                  "modus-themes-subtle-yellow"))))

;;;; no-littering

(elpaca no-littering
  (require 'no-littering)
  (no-littering-theme-backups)
  (setq backup-by-copying t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;;; crux

(elpaca crux
  (keymap-global-set "C-<return>"   'crux-smart-open-line)
  (keymap-global-set "<deleteline>" 'crux-smart-kill-line)
  (keymap-global-set "C-k"          'crux-smart-kill-line)
  (keymap-global-set "C-x F"        'crux-sudo-edit)
  (keymap-global-set "C-x W"        'crux-open-with)

  (with-eval-after-load 'conn-mode
    (keymap-set conn-state-map "S" 'crux-visit-shell-buffer)
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
      ">" 'rotate-frame-clockwise
      "<" 'rotate-frame-anticlockwise
      "@" 'rotate-frame
      "_" 'flip-frame
      "|" 'flop-frame)

    (with-eval-after-load 'transpose-frame
      (defvar-keymap rotate-frame-map
        :repeat t
        ">" 'rotate-frame-clockwise
        "<" 'rotate-frame-anticlockwise)

      (mapc #'conn-set-repeat-command
            '(rotate-frame
              flop-frame
              flip-frame
              transpose-frame)))))

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
  (require 'ace-window)

  (when window-system
    (ace-window-posframe-mode 1))

  (setq aw-keys '(?f ?d ?r ?s ?g ?t ?q ?w)
        aw-dispatch-always t)

  (face-spec-set
   'aw-leading-char-face
   '((t (:inherit ace-jump-face-foreground :height 5.0))))

  (keymap-global-set "C-;" 'ace-window)

  (with-eval-after-load 'conn-mode
    (advice-add 'aw-show-dispatch-help :around 'disable-minibuffer-max-height)))

;;;; expand-region

(elpaca expand-region
  (keymap-global-set "C-." 'er/expand-region))

;;;; zones

(elpaca zones
  (run-with-idle-timer 2 nil (lambda () (require 'zones)))

  (with-eval-after-load 'zones
    (defun david-zz-widen ()
      (interactive)
      (zz-narrow '(4)))

    (define-keymap
      :keymap narrow-map
      "w" 'david-zz-widen
      "*" 'zz-replace-regexp-zones
      "/" 'zz-replace-string-zones
      "%" 'zz-map-query-replace-regexp-zones)))

;;;; isearch+

(elpaca (isearch+ :host github
                  :repo "emacsmirror/isearch-plus"
                  :main "isearch+.el")
  (run-with-idle-timer 1.5 nil (lambda () (require 'isearch+)))

  (with-eval-after-load 'isearch+
    (setq isearchp-lazy-dim-filter-failures-flag nil
          isearchp-restrict-to-region-flag nil
          isearchp-deactivate-region-flag nil
          isearchp-movement-unit-alist '((?w . forward-word)
                                         (?s . forward-sexp)
                                         (?i . forward-list)
                                         (?s . forward-sentence)
                                         (?c . forward-char)
                                         (?l . forward-line)))

    (keymap-set isearch-mode-map "C-;" isearchp-filter-map)
    (keymap-set isearchp-filter-map "a" 'isearchp-add-filter-predicate)
    (keymap-set isearchp-filter-map "r" 'isearchp-add-regexp-filter-predicate)))

;;;; isearch-prop

(elpaca (isearch-prop :host github :repo "emacsmirror/isearch-prop")
  (with-eval-after-load 'isearch+
    (require 'isearch-prop)))

;;;; conn-mode

(elpaca (conn-mode :host codeberg
                   :repo "crcs/conn-mode"
                   :files (:defaults "extensions/*"))
  (setq conn-lighter nil
        conn-state-buffer-colors t
        conn-modes '(prog-mode
                     conf-mode
                     diary-mode
                     fundamental-mode
                     slime-repl-mode
                     (not pdf-outline-buffer-mode)
                     text-mode
                     outline-mode
                     eshell-mode
                     minibuffer-mode
                     grep-mode
                     occur-mode)
        dot-state-cursor-type 'box
        conn-state-cursor-type 'box
        emacs-state-cursor-type 'box)

  (conn-mode 1)
  (conn-mode-line-indicator-mode 1)

  (conn-add-mark-trail-command 'forward-whitespace)
  (conn-add-mark-trail-command 'conn-backward-whitespace)

  (keymap-global-set "C-S-j" 'backward-page)
  (keymap-global-set "C-S-l" 'forward-page)
  (keymap-global-set "C-c b" 'conn-buffer-map)

  (define-keymap
    :keymap page-navigation-repeat-map
    "j" 'backward-page
    "l" 'forward-page)

  (define-keymap
    :keymap conn-misc-edit-map
    "d" 'duplicate-dwim
    "," 'subword-mode
    "<" 'global-subword-mode)

  (define-keymap
    :keymap conn-misc-edit-map
    "d" 'duplicate-dwim
    "," 'subword-mode
    "<" 'global-subword-mode)

  (set-default-conn-state '(minibuffer-mode
                            eshell-mode
                            grep-mode
                            occur-mode
                            diary-mode
                            fundamental-mode
                            slime-repl-mode
                            "COMMIT_EDITMSG")
                          'emacs-state)

  (add-hook 'read-only-mode-hook 'emacs-state))

;;;;; conn-expand-region

(with-eval-after-load 'conn-mode
  (keymap-set conn-state-map "C-." 'conn-expand-region)
  (define-keymap
    :keymap dot-state-map
    "C-." 'conn-expand-dots
    "C-M-." 'conn-contract-dots))

;;;;; conn-isearch+

(with-eval-after-load 'isearch+
  (with-eval-after-load 'conn-mode
    (require 'conn-isearch+)
    (keymap-set isearch-mode-map "M-," 'conn-isearch-in-dot-toggle)))

;;;;; conn-avy

(with-eval-after-load 'conn-mode
  (with-eval-after-load 'avy
    (require 'conn-avy)
    (keymap-set goto-map "C-," 'conn-avy-goto-dot)))

;;;;; conn-embark

(with-eval-after-load 'conn-mode
  (with-eval-after-load 'embark
    (require 'conn-embark)

    (define-keymap
      :keymap embark-general-map
      "R" 'conn-embark-replace-region
      "~" 'conn-dot-region)

    (define-keymap
      :keymap embark-region-map
      "," 'indent-rigidly
      "r" 'conn-replace-region-substring
      "D" 'conn-dot-region
      "g" 'conn-duplicate-region
      "G" 'conn-duplicate-and-comment-region)

    (define-keymap
      :keymap conn-common-map
      "r" 'conn-embark-region
      "e" 'embark-dwim
      "h" 'embark-alt-dwim
      "H" 'embark-act)

    (keymap-set embark-kill-ring-map "r" 'conn-embark-replace-region)
    (keymap-unset embark-expression-map "D")
    (keymap-unset embark-defun-map "D")

    (setq conn-complete-keys-prefix-help-command t)

    (conn-complete-keys-mode 1)

    (with-eval-after-load 'conn-consult
      (defvar-keymap embark-consult-location-map
        :parent embark-general-map
        "D" 'conn-dot-consult-location-candidate)
      (add-to-list 'embark-keymap-alist '(consult-location embark-consult-location-map))

      (defvar-keymap embark-consult-grep-map
        :parent embark-general-map
        "D" 'conn-dot-consult-grep-candidate)
      (add-to-list 'embark-keymap-alist '(consult-grep embark-consult-grep-map)))))

;;;;; conn-consult

(with-eval-after-load 'conn-mode
  (with-eval-after-load 'consult
    (require 'conn-consult)
    (keymap-set search-map "t" 'conn-consult-thing)))

;;;; ialign

(elpaca ialign
  (with-eval-after-load 'embark
    (defun embark-ialign (_reg)
      (ialign (region-beginning) (region-end)))

    (keymap-set embark-region-map "a" 'embark-ialign)))

;;;; bookmark+

(elpaca (bookmark+ :host github
                   :repo "emacsmirror/bookmark-plus"
                   :main "bookmark+.el")
  (run-with-idle-timer 1 nil (lambda () (require 'bookmark+)))

  (setq bmkp-bookmark-map-prefix-keys '("x")
        bookmark-default-file (expand-file-name "~/.emacs.d/var/bmkp/current-bookmark.el")
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

(elpaca visual-regexp
  (keymap-global-set "C-M-%" #'vr/query-replace)
  (keymap-global-set "C-M-!" #'vr/replace)

  (with-eval-after-load 'conn-mode
    (define-keymap
      :keymap conn-misc-edit-map
      "R" 'vr/query-replace
      "r" 'vr/replace)))

;;;; avy

(elpaca avy
  (setq avy-single-candidate-jump nil
        avy-timeout-seconds 0.45
        avy-keys '(?a ?b ?f ?g ?i ?j ?k ?l ?m ?o ?p ?q ?r ?s ?u ?v ?x)
        avy-line-insert-style 'below)

  (setf (alist-get 'avy-goto-char-timer avy-orders-alist) #'avy-order-closest)

  (setq avy-dispatch-alist '((?w  .  avy-action-kill-move)
                             (?d  .  avy-action-kill-stay)
                             (?t  .  avy-action-teleport)
                             (?c  .  avy-action-copy)
                             (?y  .  avy-action-yank)
                             (?Y  .  avy-action-yank-line)
                             (?$  .  avy-action-ispell)
                             (?\\ .  avy-action-zap-to-char)))

  (defun avy-process-disable-aw-update (&rest app)
    (cl-letf (((symbol-function 'aw-update) #'ignore))
      (apply app)))

  (advice-add #'avy-process :around 'avy-process-disable-aw-update)

  (keymap-global-set           "C-,"   'avy-goto-char-timer)
  (keymap-set isearch-mode-map "S-SPC" 'avy-isearch)
  (keymap-set isearch-mode-map "TAB" 'avy-isearch)

  (define-keymap
    :keymap goto-map
    "u" 'avy-goto-char-timer
    "L" 'avy-goto-word-or-subword-1
    "C-l" 'avy-goto-word-or-subword-1
    "J" 'avy-goto-word-or-subword-1
    "C-j" 'avy-goto-word-or-subword-1
    "j" 'avy-goto-word-1-above
    "l" 'avy-goto-word-1-below
    "M" 'avy-goto-symbol-1
    "C-m" 'avy-goto-symbol-1
    "N" 'avy-goto-symbol-1
    "C-n" 'avy-goto-symbol-1
    "m" 'avy-goto-symbol-1-below
    "n" 'avy-goto-symbol-1-above
    "K" 'avy-goto-line
    "C-k" 'avy-goto-line
    "o" 'avy-goto-end-of-line
    "k" 'avy-goto-line-below
    "i" 'avy-goto-line-above
    "z" 'avy-resume
    "Y" 'david-avy-toggle-insertion-style
    "C-y" 'david-avy-toggle-insertion-style
    "I" 'avy-goto-char-in-line
    "C-i" 'avy-goto-char-in-line)

  (with-eval-after-load 'conn-mode
    (setf (alist-get ?n avy-dispatch-alist) #'avy-action-transpose))

  (with-eval-after-load 'avy
    (with-eval-after-load 'embark
      (defun avy-action-embark (pt)
        (unwind-protect
            (save-excursion
              (goto-char pt)
              (embark-act))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?e avy-dispatch-alist) #'avy-action-embark)

      (defun avy-action-embark-dwim (pt)
        (unwind-protect
            (save-excursion
              (goto-char pt)
              (embark-dwim))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?h avy-dispatch-alist) #'avy-action-embark-dwim)

      (defun avy-action-embark-alt-dwim (pt)
        (unwind-protect
            (save-excursion
              (goto-char pt)
              (embark-alt-dwim))
          (select-window
           (cdr (ring-ref avy-ring 0))))
        t)
      (setf (alist-get ?H avy-dispatch-alist) #'avy-action-embark-alt-dwim))

    (defun david-avy-toggle-insertion-style ()
      (interactive)
      (if (eq avy-line-insert-style 'above)
          (setq avy-line-insert-style 'below)
        (setq avy-line-insert-style 'above))
      (message "Avy line insertion style set to: %s" avy-line-insert-style))
    (put 'david-avy-toggle-insertion-style 'repeat-map goto-map)))

;;;; helpful

(elpaca helpful
  (keymap-global-set "C-h v" 'helpful-variable)
  (keymap-global-set "C-h k" 'helpful-key)
  (keymap-global-set "C-h ," 'display-local-help)
  (keymap-global-set "C-h ." 'helpful-at-point)
  (define-key global-map [remap describe-function] 'helpful-callable)
  (define-key global-map [remap describe-variable] 'helpful-variable)

  (push '(help-mode . helpful-mode) major-mode-remap-alist)

  (with-eval-after-load 'embark
    (keymap-set embark-symbol-map "M-RET" 'helpful-symbol)))

;;;; all-the-icons

(elpaca all-the-icons)

;;;;; all-the-icons-dired

(elpaca all-the-icons-dired
  (diminish 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;;;; magit

(elpaca magit
  (with-eval-after-load 'conn-mode
    (conn-hide-mark-cursor 'magit-status-mode)))

;;;; flycheck

;; (elpaca flycheck
;;   (setq lsp-diagnostics-flycheck-default-level 'warning))

;;;; cape

(elpaca cape
  (keymap-global-set "M-L" 'cape-line)

  (add-hook 'text-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dict)))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file)

  (with-eval-after-load 'embark
    (keymap-set embark-symbol-map "TAB" 'embark-cape-symbol)))

;;;; embark

(elpaca embark
  (defvar-keymap embark-alt-page-map
    "RET" 'narrow-to-page
    "m" 'mark-page)

  (require 'embark)

  (setq embark-quit-after-action t
        embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator)
        embark-prompter 'embark-keymap-prompter
        embark-cycle-key "."
        embark-help-key "?"
        embark-confirm-act-all nil)

  (keymap-global-set "M-." 'embark-dwim)
  (keymap-global-set "M-," 'embark-alt-dwim)

  (define-keymap
    :keymap minibuffer-mode-map
    "C-M-." 'embark-export)

  (keymap-set embark-symbol-map "h" 'helpful-symbol)
  (keymap-set embark-collect-mode-map "C-j" 'consult-preview-at-point)

  (defun embark-act-persist ()
    (interactive)
    (let (embark-quit-after-action)
      (embark-act)))

  (defun embark-act-marked ()
    (interactive)
    (if (embark-selected-candidates)
        (embark-act-all)
      (embark-act)))

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

  (defvar-keymap embark-tab-bar-map
    "d" 'embark-tab-delete
    "r" 'embark-tab-rename
    "t" 'embark-tab-detach)

  (add-to-list 'embark-keymap-alist '(tab-bar embark-tab-bar-map))

  (defcustom embark-alt-default-action-overrides nil
    "`embark-default-action-overrides' for alternate actions."
    :type '(alist :key-type (choice (symbol :tag "Type")
                                    (cons (symbol :tag "Type")
                                          (symbol :tag "Command")))
                  :value-type (function :tag "Default action")))

  (defun embark-alt--default-action (type)
    "`embark--default-action' for alt actions"
    (or (alist-get (cons type embark--command) embark-alt-default-action-overrides
                   nil nil #'equal)
        (alist-get type embark-alt-default-action-overrides)
        (alist-get t embark-alt-default-action-overrides)
        (keymap-lookup (embark--raw-action-keymap type) "M-RET")))

  (defun embark-alt-dwim (&optional arg)
    "alternate `embark-dwim'."
    (interactive "P")
    (if-let ((targets (embark--targets)))
        (let* ((target
                (or (nth
                     (if (or (null arg) (minibufferp))
                         0
                       (mod (prefix-numeric-value arg) (length targets)))
                     targets)))
               (type (plist-get target :type))
               (default-action (embark-alt--default-action type))
               (action (or (command-remapping default-action) default-action)))
          (unless action
            (user-error "No default action for %s targets" type))
          (when (and arg (minibufferp)) (setq embark--toggle-quit t))
          (embark--act action
                       (if (and (eq default-action embark--command)
                                (not (memq default-action
                                           embark-multitarget-actions)))
                           (embark--orig-target target)
                         target)
                       (embark--quit-p action)))
      (user-error "No target found")))

  (defun embark-alt-line-target-finder ()
    (when (and (not (minibufferp))
               (not (region-active-p))
               (bolp))
      (let ((bounds (bounds-of-thing-at-point 'line)))
        (cons 'line (cons
                     (buffer-substring (car bounds) (cdr bounds))
                     bounds)))))

  (defun embark-alt-page-target-finder ()
    (when-let ((bounds (bounds-of-thing-at-point 'page)))
      (cons 'page (cons
                   (buffer-substring (car bounds) (cdr bounds))
                   bounds))))

  (defun embark-alt-heading-target-finder ()
    (when (and (derived-mode-p 'outline-mode)
               (outline-on-heading-p))
      (let ((bounds (save-excursion
                      (let ((beg))
                        (beginning-of-line)
                        (setq beg (point))
                        (outline-end-of-subtree)
                        (cons beg (point))))))
        (cons 'outline-heading
              (cons
               (buffer-substring (car bounds) (cdr bounds))
               bounds)))))

  (defun embark-alt-scroll-down (&rest _)
    (scroll-down-command)
    (move-beginning-of-line nil))

  (defun embark-alt-scroll-up (&rest _)
    (scroll-up-command)
    (move-beginning-of-line nil))

  (keymap-set embark-identifier-map "M-RET" 'xref-find-references)

  (defvar-keymap embark-alt-line-map
    "RET" 'embark-alt-scroll-up
    "M-RET" 'embark-alt-scroll-down)

  (setf (alist-get 'line embark-keymap-alist)
        (list 'embark-alt-line-map))

  (setf (alist-get 'page embark-keymap-alist)
        (list 'embark-alt-page-map))

  (defvar-keymap xref-go-back-repeat-map
    :repeat t
    "," 'xref-go-back
    "." 'xref-go-forward)

  (keymap-global-set "C-M-." 'embark-alt-dwim)
  (keymap-set embark-heading-map "RET" #'outline-cycle)

  (define-keymap
    :keymap goto-map
    "," 'xref-go-back
    "." 'xref-go-forward)

  (add-to-list 'embark-target-finders #'embark-alt-line-target-finder)
  (add-to-list 'embark-target-finders #'embark-alt-page-target-finder t)

  (with-eval-after-load 'consult
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
    (cl-pushnew 'embark-consult-kill-lines embark-multitarget-actions))

  (with-eval-after-load 'vertico
    (define-keymap
      :keymap vertico-map
      "C-TAB" 'embark-act-all
      "C-<tab>" 'embark-act-all
      "M-TAB" 'embark-act-persist
      "M-<tab>" 'embark-act-persist
      "C-SPC" 'embark-select
      "TAB" 'embark-act-marked
      "<tab>" 'embark-act-marked))

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

    (add-to-list 'embark-target-finders 'embark-org-target-link))

  (with-eval-after-load 'conn-mode
    (keymap-set embark-region-map "RET" 'conn-copy-region)

    (define-keymap
      :keymap embark-region-map
      "l" 'conn-join-lines
      "TAB" 'indent-region
      "u" nil
      "RET" 'eval-region)))

;;;;; embark-consult

(elpaca embark-consult
  (require 'embark-consult)

  (define-keymap
    :keymap embark-region-map
    "o" 'consult-line
    "u o" 'consult-line
    "u f" 'consult-find
    "u g" 'consult-git-grep
    "u l" 'consult-locate
    "u i" 'consult-imenu
    "u I" 'consult-imenu-multi
    "u O" 'consult-line-multi
    "u r" 'consult-ripgrep)

  (define-keymap
    :keymap embark-general-map
    "u o" 'consult-line
    "u f" 'consult-find
    "u g" 'consult-git-grep
    "u l" 'consult-locate
    "u i" 'consult-imenu
    "u I" 'consult-imenu-multi
    "u O" 'consult-line-multi
    "u r" 'consult-ripgrep))

;;;; corfu

(elpaca corfu
  (setq corfu-quit-at-boundary nil
        corfu-quit-no-match nil
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-auto nil)

  (global-corfu-mode 1)

  (defun corfu-start-and-insert-sep ()
    (interactive)
    (completion-at-point)
    (corfu-insert-separator))

  (keymap-set corfu-mode-map "M-SPC" #'corfu-start-and-insert-sep)
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (keymap-set corfu-map "C-j" #'corfu-quick-complete)
  (keymap-set corfu-map "<return>" #'corfu-insert)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties (nth 4 completion-in-region--data))
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))

  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;;;; projectile

(elpaca projectile
  (projectile-mode 1)
  (diminish 'projectile-mode)

  (define-keymap
    :keymap projectile-mode-map
    "C-c p" 'projectile-command-map)

  (define-keymap
    :keymap projectile-command-map
    "I" 'projectile-invalidate-cache
    "i" 'projectile-ibuffer))

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

  (defun orderless-toggle-smart-case ()
    (interactive)
    (setq-local orderless-smart-case (not orderless-smart-case))
    (message "smart-case: %s" orderless-smart-case))
  (keymap-set minibuffer-local-map "M-C" 'orderless-toggle-smart-case)

  (orderless-define-completion-style orderless+mm
    (orderless-affix-dispatch-alist (append '((?* . orderless-major-mode))
                                            orderless-affix-dispatch-alist)))

  (setq orderless-affix-dispatch-alist '((?^ . orderless-not)
                                         (?/ . orderless-regexp)
                                         (?! . orderless-without-literal)
                                         (?@ . orderless-annotation)
                                         (?, . orderless-initialism)
                                         (?~ . orderless-flex))
        completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-literal)
        completion-category-overrides '((file (styles basic partial-completion))
                                        (buffer (styles orderless+mm)))
        orderless-component-separator #'orderless-escapable-split-on-space))

;;;;; orderless-set-operations

(elpaca (orderless-set-operations :host codeberg
                                  :repo "crcs/orderless-set-operations")
  (setq oso--command-affix-overrides
        '((consult-buffer (?* . orderless-major-mode))))

  (orderless-predicate-mode 1))

;;;; consult

(elpaca consult
  (require 'consult)

  (setq consult-async-min-input 3
        consult-yank-rotate nil
        consult-narrow-key "M-N"
        consult-ripgrep-args (concat consult-ripgrep-args " --multiline")
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

  (define-key global-map [remap Info-search] 'consult-info)
  (define-key global-map [remap bookmark-jump] 'consult-bookmark)
  (define-key global-map [remap yank-pop] 'consult-yank-pop)
  (keymap-global-set "C-x k" 'kill-this-buffer)
  (keymap-global-set "M-X" 'consult-mode-command)
  (keymap-global-set "C-x M-:" 'consult-complex-command)
  (keymap-global-set "C-x b" 'consult-buffer)
  (keymap-global-set "C-h i" 'consult-info)
  (keymap-global-set "C-h TAB" 'info)

  (keymap-set minibuffer-local-map "M-r" 'consult-history)

  (define-keymap
    :keymap search-map
    "K" 'consult-kmacro
    "n" 'consult-ripgrep-n
    "w" 'consult-man
    "e" 'consult-isearch-history
    "t" 'consult-outline
    "o" 'consult-line
    "O" 'consult-line-multi
    "r" 'consult-ripgrep
    "G" 'consult-grep
    "g" 'consult-git-grep
    "f" 'consult-find
    "l" 'consult-locate
    "v" 'consult-focus-lines
    "k" 'consult-keep-lines
    "i" 'consult-imenu
    "I" 'consult-imenu-multi)

  (keymap-set goto-map "g" 'consult-goto-line)
  (keymap-global-set "C-x B" 'consult-project-buffer)

  (define-keymap
    :keymap isearch-mode-map
    "M-s j" 'consult-line
    "M-s J" 'consult-line-multi)

  (consult-customize consult-completion-in-region :preview-key nil)
  (consult-customize consult--source-bookmark :preview-key "C-j")
  (consult-customize consult-bookmark :preview-key "C-j")
  (consult-customize consult-buffer :preview-key "C-j")
  (consult-customize consult-project-buffer :preview-key "C-j")

  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  (defun consult-async-pause (&optional arg)
    (interactive "P")
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
    (keymap-set conn-misc-edit-map "e" 'consult-keep-lines)
    (keymap-set conn-buffer-map "u" 'consult-project-buffer)
    (keymap-set conn-buffer-map "b" 'ibuffer)

    (define-keymap
      :keymap conn-state-map
      "Y" 'consult-yank-pop
      "p" 'consult-register-load)

    (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

    (with-eval-after-load 'dired
      (defun conn-consult-ripgrep-dired-marked-files ()
        (interactive)
        (consult-ripgrep (dired-get-marked-files)))
      (keymap-set dired-mode-map "M-s r" 'conn-consult-ripgrep-dired-marked-files))

    (with-eval-after-load 'ibuffer
      (defun conn-consult-line-multi-ibuffer-marked ()
        (interactive)
        (consult-line-multi
         `(:include
           ,(mapcar (lambda (buf)
                      (regexp-quote (buffer-name buf)))
                    (ibuffer-get-marked-buffers)))))
      (keymap-set ibuffer-mode-map "M-s j" 'conn-consult-line-multi-ibuffer-marked))

    (advice-add #'register-preview :override #'consult-register-window)))

;;;;; consult-extras

(elpaca (consult-extras :host codeberg :repo "crcs/consult-extras")
  (with-eval-after-load 'consult
    (require 'consult-extras)
    (keymap-global-set "C-h o" 'consult-symbol)
    (keymap-set goto-map "y" 'consult-all-marks)))

;;;;; consult-projectile

(elpaca consult-projectile
  (keymap-global-set "C-c j" 'consult-projectile))

;;;; vertico

(elpaca (vertico :files (:defaults "extensions/*"))
  (require 'vertico)

  (setq vertico-preselect 'first
        vertico-buffer-hide-prompt nil
        vertico-cycle t
        vertico-buffer-display-action '(display-buffer-reuse-mode-window
                                        (mode . minibuffer-mode))
        vertico-multiform-commands '((completion-at-point
                                      buffer (vertico-buffer-display-action
                                              display-buffer-same-window))
                                     (tempel-insert
                                      buffer (vertico-buffer-display-action
                                              display-buffer-same-window))
                                     (tempel-complete
                                      buffer (vertico-buffer-display-action
                                              display-buffer-same-window)))
        vertico-multiform-categories '((lsp-capf
                                        buffer (vertico-buffer-display-action
                                                display-buffer-same-window))
                                       (t buffer)))

  (face-spec-set 'vertico-current
                 '((t :inherit region)))
  (face-spec-set 'vertico-group-title
                 '((t :inherit modus-themes-heading-0 :italic t :bold t)))

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)

  (defun vertico-buffer--redisplay-ad (win)
    (when-let ((mbwin (active-minibuffer-window))
               ((eq (window-buffer mbwin) (current-buffer))))
      (unless (eq win mbwin)
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

  (defun vertico-repeat-ad (&rest _)
    (when (> (minibuffer-depth) 0)
      (select-window
       (if (and (equal (selected-window) (minibuffer-window))
                (not (with-current-buffer
                         (window-buffer (minibuffer-selected-window))
                       (eq major-mode 'minibuffer-mode))))
           (minibuffer-selected-window)
         (minibuffer-window)))
      t))
  (advice-add 'vertico-repeat :before-until #'vertico-repeat-ad)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (keymap-global-set "<f1>" 'vertico-repeat)

  (keymap-unset vertico-map "C-j")

  (define-keymap
    :keymap vertico-map
    "M-i" 'vertico-insert
    "RET" 'vertico-directory-enter
    "DEL" 'vertico-directory-delete-char
    "C-<backspace>" 'vertico-directory-delete-word
    "C-DEL" 'vertico-directory-delete-word
    "M-<backspace>" 'vertico-directory-up
    "M-DEL" 'vertico-directory-up
    "M-RET" 'vertico-exit-input
    "C-M-j" 'vertico-exit-input
    "C-M-<return>" 'vertico-exit-input
    "C-j" 'vertico-quick-jump
    "C-S-j" 'vertico-quick-exit
    "C-w" 'david-vertico-copy-or-kill)

  (defun david-vertico-copy-or-kill (beg end)
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

  (defun marginalia-annotate-alias (cand)
    "Annotate CAND with the function it aliases."
    (when-let ((sym (intern-soft cand))
               (alias (car (last (function-alias-p sym t))))
               (name (and (symbolp alias) (symbol-name alias))))
      (format #(" (%s)" 1 5 (face marginalia-function)) name)))

  (defun david-marginalia-annotate-binding (cand)
    "Annotate command CAND with keybinding."
    (when-let ((sym (intern-soft cand))
               (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
      (format #(" {%s}" 1 5 (face marginalia-key)) (key-description key))))

  (defun marginalia-annotate-command-with-alias (cand)
    "Annotate command CAND with its documentation string.
    Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let (sym (intern-soft cand))
      (concat
       (david-marginalia-annotate-binding cand)
       (marginalia-annotate-alias cand)
       (marginalia--documentation (marginalia--function-doc sym)))))

  (defvar marginalia-align-column 40)

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
  (advice-add 'marginalia--align :override 'marginalia--align-column)

  (cl-pushnew #'marginalia-annotate-command-with-alias
              (alist-get 'command marginalia-annotator-registry))

  (keymap-global-set "M-A" 'marginalia-cycle)
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle))

;;;; tempel

(elpaca tempel
  (keymap-global-set "M-i" 'tempel-insert)
  (keymap-global-set "M-+" 'tempel-complete)
  (keymap-global-set "M-*" 'tempel-insert)

  (with-eval-after-load 'tempel
    (setq tempel-path "/home/dave/.emacs.d/templates/*.eld")

    (defun conn-tempel-insert-ad (fn &rest args)
      (apply fn args)
      (when tempel--active
        (emacs-state)))
    (advice-add 'tempel-insert :around 'conn-tempel-insert-ad)))

;;;;; tempel-collection

(elpaca tempel-collection)

;;;; vundo

(elpaca vundo
  (keymap-global-set "C-x u" 'vundo)
  (with-eval-after-load 'vundo
    (setq vundo-glyph-alist vundo-unicode-symbols)))

;;;; htmlize

(elpaca htmlize)

;;;; page-break-lines

(elpaca page-break-lines
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode))

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

;;;; adaptive-wrap

(elpaca adaptive-wrap
  (setq adaptive-wrap-extra-indent 2)
  (add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode))

;;;; visual-fill-column

(elpaca visual-fill-column
  (add-hook 'text-mode-hook 'visual-fill-column-mode))

;;;; denote

(elpaca (denote :files (:defaults "denote-org-extras.el"))
  (with-eval-after-load 'denote
    (require 'denote-org-extras)
    (denote-rename-buffer-mode 1))

  (keymap-global-set "C-c n e" 'denote-org-extras-extract-org-subtree)
  (keymap-global-set "C-c n d" 'denote-dired-directory)
  (keymap-global-set "C-c n n" 'denote)
  (keymap-global-set "C-c n s" 'denote-signature)
  (keymap-global-set "C-c n a" 'denote-keywords-add)
  (keymap-global-set "C-c n r" 'denote-keywords-remove)
  (keymap-global-set "C-c n w" 'denote-rename-file)
  (keymap-global-set "C-c n b" 'denote-backlinks)
  (keymap-global-set "C-c n B" 'denote-find-backlink)
  (keymap-global-set "C-c n l" 'denote-find-link)

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

  (with-eval-after-load 'consult
    (defun denote-backlinks-file (file)
      (when (denote-file-is-writable-and-supported-p file)
        (let* ((id (denote-retrieve-filename-identifier-with-error file))
               (xref-show-xrefs-function #'denote-link--prepare-backlinks)
               (project-find-functions #'denote-project-find))
          (xref--show-xrefs
           (apply-partially #'xref-matches-in-files id
                            (denote-directory-files nil :omit-current :text-only))
           nil))))))

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

  (keymap-global-set "C-c n f" 'consult-denote)

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

  (with-eval-after-load 'embark
    (defun grep--process (lines)
      (let ((file "") (file-len 0) result)
        (save-match-data
          (dolist (str lines)
            (when (and (string-match consult--grep-match-regexp str)
                       ;; Filter out empty context lines
                       (or (/= (aref str (match-beginning 3)) ?-)
                           (/= (match-end 0) (length str))))
              ;; We share the file name across candidates to reduce
              ;; the amount of allocated memory.
              (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                           (eq t (compare-strings
                                  file 0 file-len
                                  str (match-beginning 1) (match-end 1) nil)))
                (setq file (match-string 1 str)
                      file-len (length file)))
              (let* ((line (match-string 2 str))
                     (ctx (= (aref str (match-beginning 3)) ?-))
                     (sep (if ctx "-" ":"))
                     (content (substring str (match-end 0)))
                     (line-len (length line)))
                (when (length> content consult-grep-max-columns)
                  (setq content (substring content 0 consult-grep-max-columns)))
                (setq str (concat file sep line sep content))
                ;; Store file name in order to avoid allocations in `consult--prefix-group'
                (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                (when ctx
                  (add-face-text-property (+ 2 file-len line-len) (length str)
                                          'consult-grep-context 'append str))
                (push str result)))))
        (nreverse result)))

    (defun consult-denote-headings (files)
      (interactive)
      (let ((cmd (consult--build-args consult-ripgrep-args)))
        (consult--read
         (grep--process
          (apply #'process-lines
                 (append cmd '("-e" "^[*]+")
                         (mapcar #'consult-notes-denote--file files))))
         :preview-key 'any
         :prompt "Heading: "
         :lookup #'consult--lookup-member
         :state (consult--grep-state)
         :add-history (thing-at-point 'symbol)
         :require-match t
         :category 'consult-denote-heading
         :group #'consult--prefix-group
         :history '(:input consult--note-history)
         :sort nil)))
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

;;;; activities

(elpaca (activities :host github :repo "alphapapa/activities")
  (with-eval-after-load 'activities
    (activities-mode 1)
    (activities-tabs-mode 1))

  (keymap-global-set "C-x C-a C-n" 'activities-new)
  (keymap-global-set "C-x C-a C-r" 'activities-resume)
  (keymap-global-set "C-x C-a C-s" 'activities-suspend)
  (keymap-global-set "C-x C-a C-k" 'activities-kill)
  (keymap-global-set "C-x C-a RET" 'activities-switch)
  (keymap-global-set "C-x C-a g" 'activities-revert)
  (keymap-global-set "C-x C-a l" 'activities-list)

  (with-eval-after-load 'conn-mode
    (keymap-set conn-buffer-map "a n" 'activities-new)
    (keymap-set conn-buffer-map "a r" 'activities-resume)
    (keymap-set conn-buffer-map "a s" 'activities-suspend)
    (keymap-set conn-buffer-map "a k" 'activities-kill)
    (keymap-set conn-buffer-map "a RET" 'activities-switch)
    (keymap-set conn-buffer-map "a t" 'activities-switch)
    (keymap-set conn-buffer-map "a g" 'activities-revert)
    (keymap-set conn-buffer-map "a l" 'activities-list)))
