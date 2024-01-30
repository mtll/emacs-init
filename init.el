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

(setopt minibuffer-default-prompt-format ""
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
        indent-tabs-mode nil
        history-delete-duplicates t
        disabled-command-function nil
        switch-to-buffer-obey-display-actions t
        resize-mini-windows 'grow-only
        tab-bar-show nil
        tab-bar-tab-name-function 'tab-bar-tab-name-all
        minibuffer-prompt-properties '(read-only t
                                                 cursor-intangible t
                                                 face minibuffer-prompt)
        minibuffer-depth-indicate-mode t
        global-goto-address-mode t
        show-paren-mode t
        delete-selection-mode t
        column-number-mode t
        line-number-mode t
        global-subword-mode t
        electric-pair-mode t
        context-menu-mode t
        tab-bar-mode t
        winner-mode t
        undelete-frame-mode t
        yank-from-kill-ring-rotate nil)

(setq exec-path (cons (expand-file-name "scripts/" user-emacs-directory) exec-path))

(define-key global-map [remap yank] 'yank-in-context)

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

(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(keymap-set text-mode-map "M-TAB" #'completion-at-point)
(keymap-set help-map "M-k" #'describe-keymap)

(define-keymap
  :keymap ctl-x-map
  "s"   'save-buffer
  "C-s" 'save-some-buffers)

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

;;;; diary / calendar

(progn
  (require 'calendar)
  (keymap-global-set "<f5>" #'calendar)
  (setq diary-entry-marker 'highlight)
  (setq calendar-holiday-marker 'match)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'list-diary-entries-hook 'sort-diary-entries t))

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

  (setopt c-hanging-semi&comma-criteria 'set-from-style))

;;;; doc-view

(setopt doc-view-resolution 196)

;;;; ispell

(setopt ispell-program-name "aspell"
        ispell-dictionary "american")

;;;; savehist

(progn
  (setopt savehist-additional-variables
          '(projectile-project-command-history
            file-name-history
            search-ring
            regexp-search-ring
            register-alist)
          savehist-file (expand-file-name "var/savehist/hist" user-emacs-directory)
          savehist-mode t))

;;;; repeat

(progn
  (setopt repeat-check-key t
          repeat-exit-timeout nil
          repeat-echo-function 'repeat-echo-message
          repeat-keep-prefix nil
          repeat-on-final-keystroke t)

  (repeat-mode 1)

  (keymap-global-set "C-x c" 'repeat))

;;;; autorevert

(progn
  (setopt auto-revert-interval .01)
  (global-auto-revert-mode 1)
  (diminish 'auto-revert-mode))

;;;; face-remap

(with-eval-after-load 'face-remap
  (diminish 'buffer-face-mode))

;;;; recentf

(progn
  (setopt recentf-save-file "~/.emacs.d/var/recentf"
          recentf-max-saved-items 100
          recentf-max-menu-items 15
          recentf-mode t)

  (with-eval-after-load 'no-littering
    (setopt recentf-save-file "~/.emacs.d/var/recentf")
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

;;;; outline

(progn
  (keymap-global-set "C-c L" 'outline-minor-mode)
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode)))

;;;; dired

(with-eval-after-load 'dired
  (setopt dired-omit-files (rx (or (seq string-start (1+ ".") (1+ (not ".")))
                                   (seq string-start (1+ "#"))))
          dired-dwim-target 'dired-dwim-target-recent)

  (define-keymap
    :keymap dired-mode-map
    "/" 'other-window-prefix
    "?" 'other-frame-prefix))

;;;; eldoc

(progn
  (diminish 'eldoc-mode)

  (setopt eldoc-echo-area-prefer-doc-buffer t))

;;; Packages

;;;; benchmark-init

;; (elpaca benchmark-init
;;   (require 'benchmark-init)
;;   (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

;;;; narrow-indirect

(elpaca (narrow-indirect :host github :repo "emacsmirror/narrow-indirect")
  (define-keymap
    :keymap ctl-x-4-map
    "n d" 'ni-narrow-to-defun-indirect-other-window
    "n n" 'ni-narrow-to-region-indirect-other-window
    "n p" 'ni-narrow-to-page-indirect-other-window)

  (with-eval-after-load 'embark
    (keymap-set embark-region-map "N" 'ni-narrow-to-region-indirect-other-window)
    (keymap-set embark-defun-map  "N" 'ni-narrow-to-defun-indirect-other-window)))

;;;; paredit

(elpaca (paredit :host github :repo "emacsmirror/paredit")
  (require 'paredit)

  (dolist (mode '(lisp-data-mode-hook
                  eshell-mode-hook
                  sly-mrepl-mode-hook))
    (add-hook mode #'enable-paredit-mode))

  (add-hook 'paredit-mode-hook #'paredit-disable-electric-pair)

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
       "n" 'paredit-backward))

    (conn-add-thing-movement-command 'sexp 'paredit-forward)
    (conn-add-thing-movement-command 'sexp 'paredit-backward)))

;;;; sly

(elpaca sly
  (setopt inferior-lisp-program "sbcl --dynamic-space-size 8000"
          sly-symbol-completion-mode nil))

;;;;; sly-quicklisp

(elpaca sly-quicklisp)

;;;;; sly-asdf

(elpaca sly-asdf)

;;;;; sly-stepper

;; (elpaca (sly-stepper :host github :repo "joaotavora/sly-stepper"))

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
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'erlang-mode-hook 'lsp)
  (add-hook 'elixir-mode-hook 'lsp)

  (add-hook 'lsp-mode-hook 'lsp-ui-peek-mode)

  (setopt lsp-keymap-prefix "C-c l"
          lsp-eldoc-render-all nil
          lsp-enable-on-type-formatting nil
          lsp-ui-doc-alignment 'window
          lsp-ui-doc-header t
          lsp-ui-doc-border "black")

  (setopt lsp-ui-doc-background '((t (:background "#dfd9cf")))
          lsp-flycheck-warning-unnecessary-face '((t (:inherit modus-themes-lang-warning)))
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
          '(orderless))) ;; Configure orderless)
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
  (setopt j-console-cmd
          (locate-file "j9.4/jconsole.sh" exec-path
                       nil #'file-executable-p)))

;;;; cmake-mode

(elpaca cmake-mode)

;;;; zig-mode

(elpaca zig-mode
  (setopt zig-format-on-save nil))

;;;; ess

(elpaca ess)

;;;; pdf-tools

(elpaca pdf-tools
  (add-hook 'pdf-view-mode-hook #'hide-mode-line-mode)

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
        '(
          ( ?a  ("\\alpha"))
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
          ( ?j  (""                 "\\jmath"))
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
          ( ?R  (""                 "\\Re"))
          ( ?s  ("\\sigma"          "\\varsigma"      "\\sin"))
          ( ?S  ("\\Sigma"          ""                "\\arcsin"))
          ( ?t  ("\\tau"            ""                "\\tan"))
          ( ?T  (""                 ""                "\\arctan"))
          ( ?u  ("\\upsilon"))
          ( ?U  ("\\Upsilon"))
          ( ?v  ("\\vee"))
          ( ?V  ("\\Phi"))
          ( ?w  ("\\xi"))
          ( ?W  ("\\Xi"))
          ( ?x  ("\\chi"))
          ( ?X  (""))
          ( ?y  ("\\psi"))
          ( ?Y  ("\\Psi"))
          ( ?z  ("\\zeta"))
          ( ?Z  (""))
          ( ?   (""))
          ( ?0  ("\\emptyset"))
          ( ?1  (""))
          ( ?2  (""))
          ( ?3  (""))
          ( ?4  (""))
          ( ?5  (""))
          ( ?6  (""))
          ( ?7  (""))
          ( ?8  ("\\infty"))
          ( ?9  (""))
          ( ?!  ("\\neg"))
          ( ?@  (""))
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
          ( ?*  ("\\times"))
          ( ?/  ("\\not"))
          ( ?|  ("\\mapsto"         "\\longmapsto"))
          ( ?\\ ("\\setminus"))
          ( ?\" (""))
          ( ?=  ("\\Leftrightarrow" "\\Longleftrightarrow"))
          ( ?\( ("\\langle"))
          ( ?\) ("\\rangle"))
          ( ?\[ ("\\Leftarrow"      "\\Longleftarrow"))
          ( ?\] ("\\Rightarrow"     "\\Longrightarrow"))
          ( ?\{  ("\\subset"))
          ( ?\}  ("\\supset"))
          ( ?<  ("\\leftarrow"      "\\longleftarrow"     "\\min"))
          ( ?>  ("\\rightarrow"     "\\longrightarrow"    "\\max"))
          ( ?'  ("\\prime"))
          ( ?.  ("\\cdot"))
          ( ?@  ("\\circ")))))

;;;; math-delimiters

(elpaca (math-delimiters :host github :repo "oantolin/math-delimiters")
  (with-eval-after-load 'embark
    (keymap-set embark-region-map "\\" 'math-delimiters-insert))
  (with-eval-after-load 'org
    (keymap-set org-mode-map "]" 'math-delimiters-insert)))
  ;; (with-eval-after-load 'conn-mode
  ;;   (conn-set-repeat-command 'math-delimiters-insert))


;;;; org

(elpaca org
  (run-with-idle-timer 4 nil (lambda () (require 'org)))

  (setopt org-src-window-setup 'plain
          org-startup-truncated nil
          org-insert-mode-line-in-empty-file t
          org-confirm-babel-evaluate nil
          org-fold-core-style 'overlays
          org-startup-indented nil)

  (keymap-global-set "C-c o" 'org-store-link)
  (keymap-global-set "C-c l" 'org-insert-link-global)

  (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)
  (add-hook 'org-mode-hook 'abbrev-mode)

  (with-eval-after-load 'org
    (keymap-unset org-mode-map "C-'")

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

  (face-spec-set
   'modus-themes-completion-match-0
   '((t :foreground "unspecified" :background "#caf1c9")))
  (face-spec-set
   'modus-themes-completion-match-1
   '((t :foreground "unspecified" :background "#e3cff1")))
  (face-spec-set
   'modus-themes-completion-match-2
   '((t :foreground "unspecified" :background "#d1eff1")))
  (face-spec-set
   'modus-themes-completion-match-3
   '((t :foreground "unspecified" :background "#f1cccc")))

  (setq modus-themes-common-palette-overrides
        (seq-concatenate
         'list
         '((bg-main "#f8f8f8")
           (cursor "#000000"))
         modus-themes-preset-overrides-warmer))

  (load-theme 'modus-operandi-tinted t))

;;;; no-littering

(elpaca no-littering
  (require 'no-littering)
  (no-littering-theme-backups)
  (setopt backup-by-copying t
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
     :keymap conn-window-map
     "t" 'transpose-frame
     ">" 'rotate-frame-clockwise
     "<" 'rotate-frame-anticlockwise
     "r" 'rotate-frame
     "f" 'flip-frame
     "p" 'flop-frame)

    (with-eval-after-load 'transpose-frame
      (defvar-keymap rotate-frame-map
        :repeat t
        ">" 'rotate-frame-clockwise
        "<" 'rotate-frame-anticlockwise)

      (put 'rotate-frame 'repeat-cmd t)
      (put 'flop-frame 'repeat-cmd t)
      (put 'flip-frame 'repeat-cmd t)
      (put 'transpose-frame 'repeat-cmd t))))

;;;; popper

(elpaca popper
  (setopt popper-display-function 'display-buffer-reuse-window
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

  (setopt aw-keys '(?f ?d ?r ?s ?g ?t ?q ?w)
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
    (setopt isearchp-lazy-dim-filter-failures-flag nil
            isearchp-restrict-to-region-flag nil
            isearchp-deactivate-region-flag nil
            isearchp-movement-unit-alist '((?w . forward-word)
                                           (?s . forward-sexp)
                                           (?i . forward-list)
                                           (?s . forward-sentence)
                                           (?c . forward-char)
                                           (?l . forward-line)))))

;;;; isearch-prop

(elpaca (isearch-prop :host github :repo "emacsmirror/isearch-prop")
  (with-eval-after-load 'isearch+
    (require 'isearch-prop)))

;;;; conn-mode

(elpaca (conn-mode :host codeberg
                   :repo "crcs/conn-mode"
                   :files (:defaults "extensions/*"))
  (setopt conn-lighter nil
          conn-state-buffer-colors t
          conn-mode-line-indicator-mode t
          conn-modes '(prog-mode
                       diary-mode
                       fundamental-mode
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

  (defun conn-open-line-emacs-state (&optional arg)
    (interactive "P")
    (crux-smart-open-line arg)
    (emacs-state))

  (defun conn-open-line-above-emacs-state ()
    (interactive)
    (crux-smart-open-line-above)
    (emacs-state))

  (defun emacs-state-eol (&optional N)
    (interactive "P")
    (end-of-line N)
    (emacs-state))

  (defun emacs-state-bol (&optional N)
    (interactive "P")
    (beginning-of-line N)
    (back-to-indentation)
    (emacs-state))

  (set-conn-transition 'conn-state "R" #'conn-open-line-emacs-state)
  (set-conn-transition 'conn-state "E" #'conn-open-line-above-emacs-state)
  (set-conn-transition 'conn-state "F" #'emacs-state-eol)
  (set-conn-transition 'conn-state "D" #'emacs-state-bol)

  (conn-mode 1)

  (keymap-global-set "C-c v" 'conn-buffer-map)
  (keymap-global-set "C-c w" 'conn-window-map)
  (keymap-global-set "C-c W" 'conn-frame-map)
  (keymap-global-set "C-S-j" 'backward-page)
  (keymap-global-set "C-S-l" 'forward-page)

  (keymap-set conn-state-map "#" 'tear-off-window)

  (define-keymap
    :keymap page-navigation-repeat-map
    "j" 'backward-page
    "l" 'forward-page)

  (define-keymap
    :keymap conn-misc-edit-map
    "d" 'duplicate-dwim
    "b" 'subword-mode
    "B" 'global-subword-mode)

  (set-default-conn-state '(minibuffer-mode
                            eshell-mode
                            grep-mode
                            occur-mode
                            diary-mode
                            fundamental-mode
                            "COMMIT_EDITMSG")
                          'emacs-state))

;;;;; conn-expand-region

(with-eval-after-load 'conn-mode
  (keymap-set conn-state-map "." 'conn-expand-region)
  (define-keymap
    :keymap dot-state-map
    "C-." 'conn-expand-dots
    "C-M-." 'conn-contract-dots))

;;;;; conn-isearch+

(with-eval-after-load 'isearch+
  (with-eval-after-load 'conn-mode
    (require 'conn-isearch+)
    (keymap-set isearch-mode-map "C-," 'conn-isearch-in-dot-toggle)))

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
      "e" 'embark-act)

    (keymap-set embark-kill-ring-map "r" 'conn-embark-replace-region)
    (keymap-unset embark-expression-map "D")
    (keymap-unset embark-defun-map "D")

    (setopt conn-complete-keys-prefix-help-command t)

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

  (setopt bmkp-bookmark-map-prefix-keys '("x")
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
  (setopt avy-single-candidate-jump nil
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
  (define-keymap
    :keymap goto-map
    "q" 'avy-goto-char-timer
    "U" 'avy-goto-word-or-subword-1
    "O" 'avy-goto-word-or-subword-1
    "u" 'avy-goto-word-1-above
    "o" 'avy-goto-word-1-below
    "M" 'avy-goto-symbol-1
    "N" 'avy-goto-symbol-1
    "m" 'avy-goto-symbol-1-below
    "n" 'avy-goto-symbol-1-above
    "L" 'avy-goto-char
    "K" 'avy-goto-line
    "l" 'avy-goto-end-of-line
    "k" 'avy-goto-line-below
    "i" 'avy-goto-line-above
    "z" 'avy-resume
    "Y" 'david-avy-toggle-insertion-style
    "I" 'avy-goto-char-in-line)

  (with-eval-after-load 'conn-mode
    (define-keymap
      :keymap conn-common-map
      ","   'avy-goto-char-timer)

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

  (push '(help-mode . helpful-mode) major-mode-remap-alist))

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

(elpaca flycheck
  (setq lsp-diagnostics-flycheck-default-level 'warning))

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
  (with-eval-after-load 'conn-mode
    (require 'embark)
    (setopt embark-mixed-indicator-delay .66
            embark-quit-after-action t
            embark-indicators '(embark-minimal-indicator
                                embark-highlight-indicator
                                embark-isearch-highlight-indicator)
            embark-prompter 'embark-keymap-prompter
            embark-cycle-key "."
            embark-help-key "?"
            embark-confirm-act-all nil)

    (keymap-global-set "M-<return>" 'embark-act)
    (keymap-global-set "M-." 'embark-dwim)
    (keymap-global-set "M-," 'embark-alt-dwim)

    (define-keymap
      :keymap embark-region-map
      "l" 'conn-join-lines
      "TAB" 'indent-region
      "u" nil
      "RET" 'eval-region)

    (define-keymap
      :keymap minibuffer-mode-map
      "M-." 'embark-act
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

    (add-to-list 'embark-keymap-alist '(tab-bar embark-tab-bar-map)))

  (with-eval-after-load 'embark
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
          ;; embark--command
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

    ;; (defun embark-keybind-target-finder ()
    ;;   (when-let ((bounds (org-in-regexp "{.*}"))
    ;;              (keys (buffer-substring-no-properties (car bounds) (cdr bounds))))
    ;;     (when (key-valid-p keys)
    ;;       (cons 'keys (cons keys bounds)))))

    (defun embark-alt-line-target-finder ()
      (when (and (not (minibufferp))
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

    (defvar-keymap embark-alt-page-map
      "RET" 'narrow-to-page
      "M-RET" 'ni-narrow-to-page-indirect-other-window
      "m" 'mark-page)

    (setf (alist-get 'page embark-keymap-alist)
          (list 'embark-alt-page-map))

    (define-keymap
      :keymap embark-symbol-map
      "M-RET" 'helpful-symbol)

    (defvar-keymap xref-go-back-repeat-map
      :repeat t
      "," 'xref-go-back
      "." 'xref-go-forward)

    (keymap-global-set "C-M-." 'embark-alt-dwim)

    (define-keymap
      :keymap goto-map
      "," 'xref-go-back
      "." 'xref-go-forward)

    (define-keymap
      :keymap embark-heading-map
      "RET" #'outline-cycle)

    (with-eval-after-load 'conn-mode
      (define-keymap
        :keymap conn-state-map
        "h" 'embark-dwim
        "H" 'embark-alt-dwim)

      (define-conn-mode-map
       'emacs-state 'view-mode
       (define-keymap
         "h" 'embark-dwim
         "H" 'embark-alt-dwim)))

    (add-to-list 'embark-target-finders #'embark-alt-line-target-finder)
    (add-to-list 'embark-target-finders #'embark-alt-page-target-finder t)))

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
  (setopt corfu-quit-at-boundary nil
          corfu-quit-no-match nil
          corfu-preview-current nil
          corfu-on-exact-match nil
          corfu-auto nil)

  (global-corfu-mode 1)

  ;; (keymap-set corfu-mode-map "S-SPC" #'corfu-start-and-insert-sep)
  ;; (keymap-set corfu-map "S-SPC" #'corfu-insert-sep)
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (keymap-set corfu-map "C-j" #'corfu-quick-complete)

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

;;;; orderless

(elpaca orderless
  (with-eval-after-load 'orderless
    (defun orderless-toggle-smart-case ()
      (interactive)
      (setq-local orderless-smart-case (not orderless-smart-case))
      (message "smart-case: %s" orderless-smart-case))

    (define-keymap
      :keymap minibuffer-local-map
      "M-C" 'orderless-toggle-smart-case)

    (setopt completion-styles '(orderless basic)
            orderless-matching-styles '(orderless-literal
                                        orderless-initialism
                                        orderless-regexp)
            completion-category-overrides '((file (styles basic partial-completion)))
            orderless-component-separator #'orderless-escapable-split-on-space)))

;;;;; orderless-set-operations

(elpaca (orderless-set-operations :host codeberg
                                  :repo "crcs/orderless-set-operations")
  (orderless-predicate-mode 1)

  (setq orderless-predicate-dispatchers
        '(orderless-contents-pred orderless-annotation-pred))

  (define-orderless-predicate-advice
      consult-buffer-advice
    (consult-buffer read-buffer)
    orderless-major-mode-pred orderless-buffer-modified-pred))

;;;; consult

(elpaca consult
  (setopt consult-async-min-input 3
          consult-yank-rotate nil
          consult-narrow-key "M-N"
          consult-ripgrep-args (concat consult-ripgrep-args " --multiline")
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref
          register-preview-delay 0.3
          register-preview-function #'consult-register-format
          consult-project-function (lambda (_) (projectile-project-root))
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
  (keymap-global-set "C-x k" 'kill-this-buffer)
  (keymap-global-set "M-X" 'consult-mode-command)
  (keymap-global-set "C-x M-:" 'consult-complex-command)
  (keymap-global-set "C-x b" 'consult-buffer)

  (keymap-set minibuffer-local-map "M-r" 'consult-history)

  (define-keymap
    :keymap search-map
    "K" 'consult-kmacro
    "n" 'consult-ripgrep-n
    "c" 'consult-info
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
  (consult-customize consult--source-bookmark :preview-key nil)
  (consult-customize consult-buffer :preview-key "C-j")
  (consult-customize consult-project-buffer :preview-key "C-j")

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

  (with-eval-after-load 'conn-mode
    (keymap-set conn-misc-edit-map "e" 'consult-keep-lines)

    (define-keymap
      :keymap conn-state-map
      "Y" 'consult-yank-pop
      "p" 'consult-register-load)

    (with-eval-after-load 'consult
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

      (advice-add #'register-preview :override #'consult-register-window))))

;;;;; consult-notes

;; (elpaca consult-notes
;;   (face-spec-set 'consult-notes-sep
;;                  '((t :inherit vertico-group-title)))
;;   (with-eval-after-load 'denote
;;     (consult-notes-denote-mode 1)
;;     (keymap-set 'denote-map "s" 'consult-notes)))

;;;;; consult-extras

(elpaca (consult-extras :host codeberg :repo "crcs/consult-extras")
  (with-eval-after-load 'consult
    (require 'consult-extras)
    (keymap-global-set "C-h o" 'consult-symbol)
    ;; (keymap-global-set "C-h f" 'consult-symbol)
    (keymap-set goto-map "y" 'consult-all-marks)))

;;;;; consult-projectile

(elpaca consult-projectile
  (keymap-global-set "C-c j" 'consult-projectile))

;;;; vertico

(elpaca (vertico :files (:defaults "extensions/*"))
  (setopt vertico-preselect 'first
          vertico-buffer-hide-prompt t
          vertico-buffer-display-action '(display-buffer-reuse-window)
          vertico-group-format (concat #(" %s " 0 4 (face vertico-group-title))
                                       #(" " 0 1 (face vertico-group-separator
                                                       display (space :align-to right))))
          vertico-count 10
          vertico-cycle t
          vertico-multiform-categories
          '((lsp-capf
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (file
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (consult-grep buffer)
            (consult-line buffer)
            (consult-location buffer)
            (note buffer)
            (imenu buffer)
            (embark-keybinding grid))
          vertico-multiform-commands
          '((consult-symbol buffer)
            (completion-at-point
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (consult-buffer
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (consult-project-buffer
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (corfu-move-to-minibuffer
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (tempel-insert
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (tempel-complete
             buffer
             (vertico-buffer-display-action . (display-buffer-same-window)))
            (denote-ripgrep-notes buffer)))

  (face-spec-set 'vertico-current '((t :background "#e1e1e1")))
  (face-spec-set 'vertico-group-separator
                 '((t :inherit default :background "#b7c9e6")))
  (face-spec-set 'vertico-group-title
                 '((t :inherit default :background "#b7c9e6" :bold t)))

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (keymap-global-set "<f1>" 'vertico-repeat)

  (keymap-unset vertico-map "C-j")

  (define-keymap
    :keymap vertico-map
    "M-TAB" 'vertico-insert
    "M-<tab>" 'vertico-insert
    "RET" 'vertico-directory-enter
    "DEL" 'vertico-directory-delete-char
    "C-<backspace>" 'vertico-directory-delete-word
    "C-DEL" 'vertico-directory-delete-word
    "M-<backspace>" 'vertico-directory-up
    "M-DEL" 'vertico-directory-up
    "C-M-j" 'vertico-exit-input
    "M-j" 'vertico-quick-jump
    "M-J" 'vertico-quick-exit
    "C-w" 'david-vertico-copy-or-kill)

  (defun david-vertico-copy-or-kill (beg end)
    (interactive (list (region-beginning) (region-end)))
    (if (or (use-region-p) (not transient-mark-mode))
        (call-interactively #'kill-region)
      (kill-new (let ((cand (vertico--candidate)))
                  (if (consult--tofu-p (aref cand (1- (length cand))))
                      (substring cand 0 -1)
                    cand)))))

  (with-eval-after-load 'embark
    (keymap-unset vertico-map "TAB")
    (keymap-unset vertico-map "<tab>")

    (define-keymap
      :keymap vertico-map
      "TAB" 'embark-act-marked
      "<tab>" 'embark-act-marked
      "C-t" 'embark-act-persist
      "C-SPC" 'embark-select)))

;;;; marginalia

(elpaca marginalia
  (marginalia-mode 1)

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

;;;; howm

(elpaca howm
  (setq howm-view-title-header "*")

  (defvar howm-previous-link nil
    "Org link to location where note was created.")

  (require 'howm)

  (setq howm-directory "~/Documents/howm"
        howm-template (list (concat "#+DATE: %date\n"
                                    "#+STARTUP: showall\n\n"
                                    howm-view-title-header
                                    " %title%cursor\n"
                                    ":HOWM:\n%link%filen:END:\n\n")
                            (concat "#+DATE: %date\n"
                                    "#+STARTUP: showall\n\n"
                                    howm-view-title-header
                                    " %title%cursor\n"
                                    ":HOWM:\n:END:\n\n"))
        howm-view-title-regexp (rx bol
                                   (or (seq (+ (any "*" "#")) " =") (+ "="))
                                   (+ " ") (group (* nonl)) eol)
        howm-view-title-regexp-pos 1
        howm-view-title-regexp-grep "^(?:[*#]+ =|=+) +"
        howm-template-file-format "%s"
        howm-view-use-grep t
        howm-view-grep-command "rg"
        howm-view-grep-option "-nH --no-heading --color never --smart-case"
        howm-view-grep-extended-option nil
        howm-view-grep-fixed-option "-F"
        howm-view-grep-expr-option "-e"
        howm-view-grep-file-stdin-option "-f -"
        howm-iigrep-preview-items 50
        howm-keyword-file (expand-file-name ".howm-keys" howm-directory)
        howm-history-file (expand-file-name ".howm-history" howm-directory)
        howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org"
        howm-content-from-region 1
        howm-menu-refresh-after-save nil
        howm-ref-regexp "<(\\([^\t\r\n]+?\\))>"
        howm-ref-regexp-pos 1
        howm-keyword-format "<\\[%s\\]>"
        howm-keyword-regexp "<\\[\\([^\t\r\n]+?\\)\\]>"
        howm-keyword-regexp-hilit-pos 1
        howm-keyword-regexp-pos 1
        howm-keyword-regexp-format "%s"
        howm-wiki-format "<([%s])>"
        howm-wiki-regexp "<\\(\\[\\([^\t\r\n]+?\\)\\)\\]>"
        howm-mode-keyword-face 'modus-themes-search-lazy
        howm-view-name-face 'modus-themes-search-lazy
        howm-template-file-format "<(%s)>"
        howm-view-header-format "\n\n#+file: %s\n\n"
        howm-view-header-regexp "^#+file:.*$"
        howm-menu-display-rules (cons
                                 (cons "%hdiary[\n]?" 'howm-menu-diary)
                                 howm-menu-display-rules)
        initial-buffer-choice (lambda ()
                                (howm-menu)
                                (current-buffer))
        howm-menu-schedule-days 14)

  (pcase-let ((`(,pat . ,rest)
               (action-lock-general 'howm-open-diary "^\\(>>d\\) " 1 1)))
    (setf (alist-get pat action-lock-default-rules nil nil 'equal) rest))

  (defun howm-menu-diary ()
    (delete-region (match-beginning 0) (match-end 0))
    (pcase-dolist (`((,m ,d ,y) ,label)
                   (diary-list-entries (calendar-current-date) howm-menu-schedule-days t))
      (insert (format ">>d [%04d-%02d-%02d] %s\n" y m d label))))

  (defun howm-open-diary (&optional dummy)
    (interactive)
    (when-let (str (save-excursion
                     (and (re-search-forward
                           "\\[[-0-9]+\\] \\(.*\\)" (line-end-position) t)
                          (regexp-quote (match-string-no-properties 1)))))
      (find-file diary-file)
      (goto-char (point-min))
      (re-search-forward str nil t)
      (beginning-of-line)))

  (defun riffle-summary-to-contents-org ()
    (org-mode)
    (org-fold-hide-drawer-all))

  (advice-add 'riffle-summary-to-contents :after 'riffle-summary-to-contents-org)

  (defun howm-template-org-link (arg)
    (when howm-previous-link
      (insert howm-previous-link "\n")))

  (setf (alist-get "%link" howm-template-rules nil nil #'equal)
        'howm-template-org-link)

  (defun howm-template-previous-file-n (arg)
    (when-let ((f (alist-get 'file arg))
               (_ (not (equal f ""))))
      (insert f "\n")))

  (setf (alist-get "%filen" howm-template-rules nil nil #'equal)
        'howm-template-previous-file-n)

  (defun howm-create-capture (fn &optional which-template here)
    (let ((howm-previous-link (and (not here)
                                   (org-store-link nil))))
      (funcall fn which-template here)))

  (advice-add 'howm-create :around #'howm-create-capture)

  (keymap-global-set "C-c n" 'howm-create)

  ;; Default recent to sorting by mtime
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  ;; Default all to sorting by creation, newest first
  (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)

  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)

  (with-eval-after-load 'conn-mode
    (add-hook 'howm-create-hook 'emacs-state))

  (with-eval-after-load 'consult
    (defun consult--ripgrep-note-make-builder (paths)
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
                (cons (append `("rgn" ,(car flags) ,(string-join (cdr flags) " "))
                              (consult--split-escaped arg)
                              '("--") (list howm-view-title-regexp-grep) paths)
                      (apply-partially #'consult--highlight-regexps
                                       (list (regexp-quote arg)) ignore-case))
              (pcase-let ((`(,res . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
                (when res
                  (cons (append `("rgn" ,(car flags)
                                  ,(string-join (append (cdr flags) (and (eq type 'pcre) '("-P"))) " "))
                                res '("--") (list howm-view-title-regexp-grep) paths)
                        hl))))))))

    (defun consult-howm-grep (&optional initial)
      (interactive)
      (consult--grep "Notes" #'consult--ripgrep-note-make-builder howm-directory initial)
      (howm-mode))

    (keymap-set search-map "m" #'consult-howm-grep)

    (defun consult--howm-link-state ()
      (let ((open (consult--temporary-files))
            (jump (consult--jump-state)))
        (lambda (action cand)
          (pcase action
            ('return cand)
            (_
             (unless cand (funcall open))
             (funcall jump action (consult--grep-position cand open)))))))

    (defun consult-howm-link (&optional initial)
      (interactive)
      (let* ((default-directory howm-directory)
             (builder (funcall #'consult--ripgrep-note-make-builder '(".")))
             (note (consult--read
                    (consult--async-command builder
                      (consult--grep-format builder))
                    :prompt "Note: "
                    :lookup #'consult--lookup-member
                    :state (consult--howm-link-state)
                    :initial (consult--async-split-initial initial)
                    :add-history (consult--async-split-thingatpt 'symbol)
                    :require-match t
                    :category 'consult-grep
                    :group #'consult--prefix-group
                    :history '(:input consult--grep-history)
                    :sort nil))
             (file-end (next-single-property-change 0 'face note))
             (file (substring-no-properties note 0 file-end))
             (line (substring-no-properties note (1+ file-end)))
             (line (substring line (1+ (seq-position line ?:)))))
        (insert
         (org-link-make-string
          (concat "file:" (expand-file-name file default-directory) "::" line)
          (read-string "Description: " line)))))

    (keymap-set howm-mode-map "C-c C-l" 'consult-howm-link)

    (with-eval-after-load 'embark
      (defvar-keymap embark-action-lock-map
        "RET" 'action-lock-magic-return)

      (cl-defun embark-howm-consult (&key target candidates &allow-other-keys)
        (consult-howm-grep (or (string-replace " " "\ " target)
                               (string-join candidates "\ "))))

      (cl-pushnew 'consult-howm-grep embark-multitarget-actions)
      (cl-pushnew #'embark-howm-consult
                  (alist-get 'consult-howm-grep embark-around-action-hooks))

      (setf (alist-get 'consult-howm-grep embark-target-injection-hooks)
            (list #'embark--allow-edit))

      (keymap-set embark-general-map "u m" 'consult-howm-grep)
      (keymap-set embark-region-map "u m" 'consult-howm-grep))))

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
