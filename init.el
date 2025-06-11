;;; -*- lexical-binding: t; eval: (outline-minor-mode 1); -*-

;;; Elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;; Built-in

(setq magit-define-global-key-bindings nil)

(defvar my-to-incremental-load nil)

(defun my-do-incremental-load ()
  (while (and my-to-incremental-load
              (not (input-pending-p)))
    (with-demoted-errors "Error in incremental loader: %s"
      (let ((inhibit-message t))
        (funcall (pop my-to-incremental-load)))))
  (when my-to-incremental-load
    (run-with-idle-timer 3 nil 'my-do-incremental-load)))
(run-with-idle-timer 3 nil 'my-do-incremental-load)

;;;; emacs

;; help-window-select t
;; visual-order-cursor-movement t
(setq edmacro-reverse-macro-lines t
      git-commit-major-mode 'log-edit-mode
      next-line-add-newlines t
      scroll-conservatively 0
      visual-order-cursor-movement t
      register-use-preview nil
      comment-empty-lines 'eol
      vc-display-status 'no-backend
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t
      ffap-machine-p-known 'reject
      word-wrap t
      truncate-string-ellipsis "…"
      comment-multi-line t
      help-enable-symbol-autoload t
      mac-option-modifier 'meta
      mac-command-modifier 'super
      hi-lock-auto-select-face t
      mark-even-if-inactive t
      even-window-sizes nil
      scroll-preserve-screen-position t
      delete-active-region nil
      fill-column 70
      use-short-answers t
      y-or-n-p-use-read-key t
      xref-search-program 'ripgrep
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
      show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      sentence-end-double-space t
      tab-always-indent 'complete
      read-minibuffer-restore-windows nil
      dired-listing-switches "-alFh --dired --group-directories-first"
      enable-recursive-minibuffers t
      ediff-window-setup-function #'ediff-setup-windows-plain
      uniquify-buffer-name-style 'post-forward
      uniquify-separator " | "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      global-mark-ring-max 32
      mark-ring-max 12
      undo-limit 8000000
      undo-strong-limit 16000000
      undo-outer-limit 32000000
      mouse-wheel-progressive-speed nil
      register-separator ?+
      history-delete-duplicates t
      disabled-command-function nil
      switch-to-buffer-obey-display-actions t
      minibuffer-prompt-properties '( read-only t
                                      cursor-intangible t
                                      face minibuffer-prompt)
      exec-path (cons (expand-file-name "scripts/" user-emacs-directory) exec-path)
      edebug-inhibit-emacs-lisp-mode-bindings t
      cycle-spacing-actions '( just-one-space
                               delete-all-space
                               delete-space-after
                               delete-space-before
                               restore)
      project-vc-extra-root-markers '(".projectile" ".project"))

(defun my-kill-buffer ()
  (interactive)
  (kill-buffer))
(keymap-global-set "C-x k" 'my-kill-buffer)

(setopt mouse-wheel-scroll-amount '(0.33
                                    ((shift) . hscroll)
                                    ((meta))
                                    ((control meta) . global-text-scale)
                                    ((control) . text-scale)))

(setq-default indent-tabs-mode nil)

(minibuffer-depth-indicate-mode 1)
(global-goto-address-mode 1)
(show-paren-mode 1)
(delete-selection-mode -1)
(column-number-mode 1)
(line-number-mode 1)
(undelete-frame-mode 1)
(context-menu-mode -1)
(save-place-mode 1)

(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-x C-z")

(keymap-global-set "<remap> <eval-expression>" 'pp-eval-expression)

(keymap-global-set "C-z" #'pop-to-mark-command)
(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "C-M-<return>"#'default-indent-new-line)
(keymap-global-set "S-<backspace>" #'cycle-spacing)
(keymap-global-set "C-j" #'join-line)
(keymap-global-set "C-:" #'read-only-mode)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "M-;" #'comment-line)
(keymap-global-set "C-c c" #'compile)
(keymap-global-set "C-S-w" #'delete-region)
(keymap-global-set "<f2>" #'other-window)
(keymap-global-set "M-z" #'transient-resume)
(keymap-global-set "C-h A" #'describe-char)
(keymap-global-set "C-x j" #'dired-jump)
(keymap-global-set "C-/" #'undo-only)
(keymap-global-set "C-x <" #'scroll-right)
(keymap-global-set "C-x >" #'scroll-left)

(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)

(keymap-global-set "C-x t u" #'tab-previous)
(keymap-global-set "C-x t /" #'tab-undo)

(keymap-global-set "C-x C-b" #'scratch-buffer)

(keymap-set tab-bar-switch-repeat-map "u" #'tab-previous)
(keymap-unset tab-bar-switch-repeat-map "O")

(defvar-keymap scroll-repeat-map
  :repeat t
  "<" 'scroll-right
  ">" 'scroll-left)

(put 'other-window 'repeat-map nil)

(keymap-set help-map "M-k" #'describe-keymap)

(define-keymap
  :keymap goto-map
  "o" #'pop-to-mark-command
  "," #'xref-go-back
  "." #'xref-go-forward)

(defvar-keymap xref-go-back-repeat-map
  :repeat t
  "," #'xref-go-back
  "." #'xref-go-forward)

(define-keymap
  :keymap ctl-x-map
  "s"   #'save-buffer
  "C-s" #'save-some-buffers)

(define-keymap
  :keymap window-prefix-map
  "d" #'tab-detach
  "t" #'tear-off-window)

(define-keymap
  :keymap emacs-lisp-mode-map
  "C-c C-z" #'eval-buffer
  "C-c C-m" #'emacs-lisp-macroexpand
  "C-c C-e" #'eval-print-last-sexp
  "C-c C-f" #'find-function
  "C-c C-l" #'pp-eval-last-sexp
  "C-c C-r" #'eval-region)

(keymap-global-set "C-x f" 'find-file)
(keymap-global-set "C-x C-f" 'set-fill-column)

(defun my-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'my-show-trailing-whitespace)

(defun my-quit-other-window-for-scrolling ()
  (interactive)
  (with-selected-window (other-window-for-scrolling)
    (quit-window)))
(keymap-global-set "C-M-S-u" 'my-quit-other-window-for-scrolling)
(keymap-global-set "C-M-S-i" #'scroll-other-window-down)
(keymap-global-set "C-M-S-k" #'scroll-other-window)
(keymap-global-set "C-M-S-." #'end-of-buffer-other-window)
(keymap-global-set "C-M-S-," #'beginning-of-buffer-other-window)

(defun my-select-other-window-for-scrolling ()
  (interactive)
  (when-let* ((win (other-window-for-scrolling)))
    (select-window win)))
(keymap-global-set "C-M-S-o" #'my-select-other-window-for-scrolling)

(defun kill-frame-and-buffer ()
  (interactive)
  (when-let* ((buf (window-buffer (frame-root-window))))
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

(defun my-copy-line-or-region ()
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-end-position)
                  (use-region-p)))
(keymap-global-set "M-w" 'my-copy-line-or-region)

(defun ediff-split-fn ()
  (if (> (frame-width) 150)
      'split-window-horizontally
    'split-window-vertically))

(find-function-setup-keys)

(defun my-forward-page ()
  (interactive)
  (when (looking-at page-delimiter)
    (forward-char))
  (forward-page)
  (beginning-of-line))

(defun my-backward-page ()
  (interactive)
  (when (looking-at page-delimiter)
    (forward-char))
  (backward-page)
  (beginning-of-line))

(defun my-inside-regexp-in-line (regexp)
  (let ((pt (point))
        result)
    (save-excursion
      (with-restriction (line-beginning-position) (line-end-position)
        (goto-char (point-min))
        (while (and (re-search-forward regexp nil t)
                    (not result))
          (when (and (<= (match-beginning 0) pt)
                     (<= pt (point)))
            (setq result (propertize
                          (match-string 0)
                          'match-data (match-data)))))))
    result))

(defun customize-read-group-ad ()
  (let ((completion-ignore-case t)
        (def (custom-group-of-mode major-mode)))
    (completing-read (format-prompt "Customize group" def)
                     obarray
                     (lambda (symbol)
                       (or (and (get symbol 'custom-loads)
                                (not (get symbol 'custom-autoload)))
                           (get symbol 'custom-group)))
                     t nil nil def)))
(advice-add 'customize-read-group :override 'customize-read-group-ad)

(defun my-recenter-pulse-ad (&rest _)
  (pulse-momentary-highlight-one-line))
(advice-add 'recenter-top-bottom :after 'my-recenter-pulse-ad)

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 4))

;;;; lisp

(with-eval-after-load 'elisp-mode
  (define-keymap
    :keymap emacs-lisp-mode-map
    "C-c x" 'eval-defun)

  (define-keymap
    :keymap lisp-interaction-mode-map
    "C-c x" 'eval-defun))

(push (lambda ()
        (setq initial-major-mode 'lisp-interaction-mode))
      my-to-incremental-load)

(defmacro my-comment (&rest _))

(defun lexical-in-temp ()
  (unless (buffer-file-name)
    (setq-local lexical-binding t)))
(add-hook 'emacs-lisp-mode-hook 'lexical-in-temp)

;;;; paren context

;; Fix bug causing indent commands to delete text when
;; show-paren-context-when-offscreen is set to 'overlay

(with-eval-after-load 'paren
  (defun show-paren--delete-context-overlay ()
    (when show-paren--context-overlay
      (delete-overlay show-paren--context-overlay)
      (setq show-paren--context-overlay nil))
    (remove-hook 'pre-command-hook #'show-paren--delete-overlays
                 'local))

  (defun show-paren--show-context-in-overlay (text)
    "Show TEXT in an overlay at the top-left of the current window."
    (setq text (replace-regexp-in-string "\n" " " text))
    (show-paren--delete-context-overlay)
    (let* ((beg (window-start))
           (end (save-excursion
                  (goto-char beg)
                  (line-end-position))))
      (setq show-paren--context-overlay (make-overlay beg end)))
    (overlay-put show-paren--context-overlay 'display text)
    ;; Use the (default very high) `show-paren-priority' ensuring that
    ;; not other overlays shine through (bug#59527).
    (overlay-put show-paren--context-overlay 'priority
                 show-paren-priority)
    (overlay-put show-paren--context-overlay
                 'face `(:box
                         ( :line-width (1 . -1)
                           :color ,(face-attribute 'shadow :foreground))))
    (add-hook 'pre-command-hook #'show-paren--delete-context-overlay
              nil 'local)))


;;;; hideshow

(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun my-hs-toggle-hiding ()
  (interactive)
  (require 'hideshow)
  (save-excursion (hs-toggle-hiding)))

(with-eval-after-load 'hideshow
  (with-eval-after-load 'diminish
    (diminish 'hs-minor-mode ""))

  (define-keymap
    :keymap hs-minor-mode-map
    "C-," 'my-hs-toggle-hiding
    "M-s h h" #'hs-hide-all
    "M-s h ," #'hs-hide-all
    "M-s h s" #'hs-show-all
    "M-s h v" #'hs-hide-level))


;;;; org

(elpaca (org :repo ("https://code.tecosaur.net/tec/org-mode.git/" . "org")
             :branch "dev"
             :pre-build (progn (require 'elpaca-menu-org) (elpaca-menu-org--build))
             :autoloads "org-loaddefs.el"
             :build (:not elpaca--generate-autoloads-async)
             :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")))
  ;; (push (lambda () (require 'org)) my-to-incremental-load)
  (setq org-highlight-latex-and-related '(native script entities)
        org-refile-use-outline-path nil
        org-outline-path-complete-in-steps nil
        org-agenda-start-on-weekday nil
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-agenda-include-diary t
        org-src-window-setup 'plain
        org-startup-truncated nil
        org-insert-mode-line-in-empty-file t
        org-confirm-babel-evaluate nil
        org-startup-indented nil
        org-agenda-files (list "~/Documents/notes/")
        org-agenda-file-regexp "\\`[^.].*_agenda\\(_.*\\.\\|\\.\\)\\org\\'")

  (keymap-global-set "C-c p" 'org-capture)
  (keymap-global-set "C-c o" 'org-store-link)
  (keymap-global-set "C-c l" 'org-insert-link-global)
  (keymap-global-set "C-c a" 'org-agenda)

  (add-hook 'org-mode-hook 'word-wrap-whitespace-mode)
  ;; (add-hook 'org-mode-hook 'abbrev-mode)

  (with-eval-after-load 'org
    (with-eval-after-load 'conn
      (keymap-set org-mode-map "C-c b" (conn-remap-key "C-c C-v"))
      (keymap-set org-mode-map "C-c x" (conn-remap-key "C-c C-x"))
      (keymap-set org-mode-map "M-j" 'org-return-and-maybe-indent)
      (keymap-unset org-mode-map "C-j")
      (keymap-set (conn-get-major-mode-map 'conn-command-state 'org-mode) "TAB" 'org-cycle)
      (keymap-set org-mode-map "C-c t" 'org-todo))

    ;; Increase preview width
    (plist-put org-latex-preview-appearance-options
               :page-width 0.8)

    (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

    ;; Block C-n, C-p etc from opening up previews when using auto-mode
    (setq org-latex-preview-auto-ignored-commands
          '(next-line previous-line mwheel-scroll
                      scroll-up-command scroll-down-command))

    ;; Enable consistent equation numbering
    (setq org-latex-preview-numbered t)

    ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
    ;; fragment and updates the preview in real-time as you edit it.
    ;; To preview only environments, set it to '(block edit-special) instead
    (setq org-latex-preview-live t)

    ;; More immediate live-previews -- the default delay is 1 second
    (setq org-latex-preview-live-debounce 0.25)
    (setq org-latex-preview-cache 'temp)
    (setq org-element-cache-persistent nil)

    (cl-loop for c across "abcdefghijklmnopqrstuvwxyz" do
             (keymap-unset org-mode-map (concat "C-c " (string c)) t)
             (keymap-unset org-mode-map (concat "C-c " (upcase (string c))) t)
             (keymap-unset outline-mode-map (concat "C-c " (string c)) t)
             (keymap-unset outline-mode-map (concat "C-c " (upcase (string c))) t))

    (keymap-unset org-mode-map "C-'")
    (keymap-unset org-mode-map "C-,")
    (keymap-set org-mode-map "M-s s" 'org-sparse-tree)

    ;; (setf (plist-get org-format-latex-options :scale) 3)

    (keymap-set org-mode-map "C-c v" 'latex-math-mode)
    (autoload 'latex-math-mode "latex")

    (defun my-org-edit-elem-ad (&rest app)
      (let ((state conn-current-state))
        (apply app)
        (funcall state)))
    (advice-add 'org-src--edit-element :around 'my-org-edit-elem-ad)

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

;; (elpaca (org-luhmann :host github :repo "yibie/org-luhmann")
;;   (with-eval-after-load 'org
;;     (org-luhmann-setup)))
;; 
;; (elpaca org-fragtog
;;   (add-hook 'org-mode-hook 'org-fragtog-mode))


;;;; Lisp Indentation

(put 'iterate 'common-lisp-indent-function 1)
(put 'mapping 'common-lisp-indent-function 1)
(put 'gathering 'common-lisp-indent-function 1)
(put 'collect 'common-lisp-indent-function 1)
(put 'collect-minimize 'common-lisp-indent-function 2)


;;;; Abbrev

(setq abbrev-all-caps t
      hippie-expand-try-functions-list '( ;; try-complete-file-name-partially
                                         ;; try-complete-file-name
                                         try-expand-all-abbrevs
                                         ;; try-expand-list
                                         ;; try-expand-line
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill))

(keymap-global-set "C-M-h" 'hippie-expand)

(with-eval-after-load 'abbrev
  (setf (alist-get 'abbrev-mode minor-mode-alist) (list "")))


;;;; electric pair

(defun my-electric-pair-turn-on ()
  (electric-pair-local-mode 1))

(define-globalized-minor-mode my-electric-pair-mode
  electric-pair-mode
  my-electric-pair-turn-on
  :predicate '((not lisp-data-mode)
               prog-mode)
  :group 'electricity)

(my-electric-pair-mode 1)


;;;; misearch

(defun my-read-buffers (&optional predicate)
  "Return a list of buffers specified interactively, one by one."
  (cl-loop for buf = (read-buffer "Buffer: " nil t
                                  (pcase-lambda (`(,name . ,buf))
                                    (and
                                     (if predicate (funcall predicate buf) t)
                                     (not (member name selected)))))
           until (equal buf "") collect buf into selected
           finally return selected))

(defun my-read-files (&optional predicate)
  "Return a list of buffers specified interactively, one by one."
  (cl-loop for file = (file-truename
                       (read-file-name "Files to search: "
                                       default-directory default-directory nil nil
                                       (lambda (filename)
                                         (and
                                          (if predicate (funcall predicate filename) t)
                                          (not (member (file-truename filename) selected))))))
           until (or (not (file-exists-p file))
                     (file-directory-p file))
           collect file into selected
           finally return selected))

(with-eval-after-load 'misearch
  (advice-add 'multi-isearch-read-buffers :override 'my-read-buffers)
  (advice-add 'multi-isearch-read-files :override 'my-read-files))


;;;; bookmarks

(setq bookmark-save-flag 1)


;;;; view-mode

(setq view-read-only t)


;;;; outline-minor-mode

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

(with-eval-after-load 'outline
  (setq outline-minor-mode-prefix (kbd "C-c u"))

  (keymap-set (conn-get-mode-map 'conn-command-state 'outline-minor-mode)
              "<conn-thing-map> h" 'outline-previous-visible-heading)
  (define-keymap
    :keymap outline-mode-prefix-map
    "@" 'outline-mark-subtree
    "n" 'outline-next-visible-heading
    "p" 'outline-previous-visible-heading
    "i" 'outline-show-children
    "s" 'outline-show-subtree
    "d" 'outline-hide-subtree
    "u" 'outline-up-heading
    "f" 'outline-forward-same-level
    "b" 'outline-backward-same-level
    "t" 'outline-hide-body
    "a" 'outline-show-all
    "c" 'outline-hide-entry
    "e" 'outline-show-entry
    "l" 'outline-hide-leaves
    "k" 'outline-show-branches
    "q" 'outline-hide-sublevels
    "o" 'outline-hide-other
    "^" 'outline-move-subtree-up
    "v" 'outline-move-subtree-down
    "m" 'outline-insert-heading
    ">" 'outline-promote
    "<" 'outline-demote)

  (pcase-dolist (`(,_ . ,def) (cdr outline-navigation-repeat-map))
    (put def 'repeat-map nil)))


;;;; line numbers

(with-eval-after-load 'display-line-numbers
  (face-spec-set 'line-number-current-line '((t :background "#ccdee3")))

  (setq-default display-line-numbers-width 2)
  (setq display-line-numbers-width-start nil
        display-line-numbers-grow-only nil
        display-line-numbers-type 'visual
        display-line-numbers-current-absolute nil
        display-line-numbers-major-tick 0))


;;;; dictionary

(with-eval-after-load 'dictionary
  (setopt dictionary-server "dict.org")
  (keymap-global-set "C-c d" #'dictionary-lookup-definition))


;;;; isearch

(setq isearch-lazy-count t
      isearch-allow-motion t
      isearch-resume-in-command-history t)

(keymap-set isearch-mode-map "M-DEL" 'isearch-delete-char)
(keymap-set isearch-mode-map "M-DEL" 'isearch-del-char)
(keymap-set isearch-mode-map "M-z"   'transient-resume)

(defun my-isearch-yank-region ()
  (interactive)
  (isearch-yank-internal (lambda () (mark t))))
(keymap-set isearch-mode-map "M-Y" 'my-isearch-yank-region)

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

(defun isearch-globs-compile (string &optional lax)
  (string-join
   (mapcar (lambda (string)
             (string-join
              (mapcar (lambda (string)
                        (string-join
                         (mapcar (lambda (string)
                                   (regexp-quote string))
                                 (isearch-escapable-split-on-char string "."))
                         ".+?"))
                      (isearch-escapable-split-on-char string "&"))
              "\\(?:\\s_\\|\\w\\)*"))
           (isearch-escapable-split-on-char string " "))
   search-whitespace-regexp))

(isearch-define-mode-toggle globs "*" isearch-globs-compile "\
Turning on globs turns off regexp mode.")
(put 'isearch-globs-compile 'isearch-message-prefix
     (propertize "glob " 'face 'minibuffer-prompt))

(defun isearch-forward-glob (&optional arg no-recursive-edit)
  "Do incremental search forward.
See command `isearch-forward' for more information."
  (interactive "P\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers))
          (t (isearch-mode t (not (null arg)) nil (not no-recursive-edit)
                           #'isearch-globs-compile)))))
(keymap-global-set "C-s" 'isearch-forward-glob)

(defun isearch-backward-glob (&optional arg no-recursive-edit)
  "do incremental search backward.
see command `isearch-forward' for more information."
  (interactive "p\np")
  (let ((numarg  (prefix-numeric-value arg)))
    (cond ((and (eq arg '-)  (fboundp 'multi-isearch-buffers))
           (let ((current-prefix-arg  nil)) (call-interactively #'multi-isearch-buffers)))
          ((and arg  (fboundp 'multi-isearch-buffers)  (< numarg 0))
           (call-interactively #'multi-isearch-buffers))
          (t (isearch-mode nil (not (null arg)) nil (not no-recursive-edit)
                           #'isearch-globs-compile)))))
(keymap-global-set "C-r" 'isearch-backward-glob)

(with-eval-after-load 'conn
  (cl-defmethod conn-dispatch-nav-commands ((_command (eql isearch-forward-glob)))
    (conn-with-dispatch-suspended
      (isearch-forward-glob)))
  (cl-defmethod conn-dispatch-nav-commands ((_command (eql isearch-backward-glob)))
    (conn-with-dispatch-suspended
      (isearch-backward-glob))))

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

(setopt tab-bar-define-keys nil
        tab-bar-show nil
        tab-bar-tab-name-function 'tab-bar-tab-name-all)

(setq tab-bar-new-tab-choice t)

(letrec ((loader (lambda ()
                   (tab-bar-mode 1)
                   (tab-bar-history-mode 1)
                   (remove-hook 'pre-command-hook loader))))
  (add-hook 'pre-command-hook loader))

(defun my-tab-bar-new-message (&rest _)
  (message "Added new tab at %s" tab-bar-new-tab-to))
(advice-add 'tab-bar-new-tab-to :after #'my-tab-bar-new-message)


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
    "TAB"'c-indent-then-complete
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

  (setq savehist-file (expand-file-name "var/savehist/hist" user-emacs-directory))

  (savehist-mode 1))


;;;; repeat

(setq repeat-exit-timeout nil
      repeat-on-final-keystroke t
      repeat-keep-prefix nil)

(letrec ((loader (lambda ()
                   (repeat-mode 1)
                   (remove-hook 'pre-command-hook loader))))
  (add-hook 'pre-command-hook loader))

(keymap-global-set "C-x c" 'repeat)


;;;; autorevert

(setq auto-revert-interval .01)
(letrec ((loader (lambda ()
                   (global-auto-revert-mode 1)
                   (remove-hook 'pre-command-hook loader))))
  (add-hook 'pre-command-hook loader))
(with-eval-after-load 'diminish
  (diminish 'auto-revert-mode))


;;;; recentf

(with-eval-after-load 'no-littering
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15
        recentf-auto-cleanup (if (daemonp) 300 'mode))

  (recentf-mode 1)

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


;;;; outline

(keymap-global-set "C-c L" 'outline-minor-mode)
(with-eval-after-load 'outline
  (with-eval-after-load 'nerd-icons
    (setf (alist-get 'outline-minor-mode minor-mode-alist)
          (list (concat (nerd-icons-codicon "nf-cod-blank")
                        (nerd-icons-mdicon "nf-md-file_tree_outline")))))
  ;; (with-eval-after-load 'diminish
  ;;   (diminish 'outline-minor-mode " *"))
  )


;;;; ibuffer

(push (lambda () (require 'ibuffer)) my-to-incremental-load)
(with-eval-after-load 'ibuffer
  (setq ibuffer-human-readable-size t))


;;;; dired

(keymap-global-set "C-x h" 'dired-jump)

(push (lambda () (require 'dired)) my-to-incremental-load)

(with-eval-after-load 'dired
  (setq dired-omit-files (rx (or (seq string-start (1+ ".") (1+ (not ".")))
                                 (seq string-start (1+ "#"))))
        dired-dwim-target 'dired-dwim-target-recent
        dired-movement-style 'bounded
        dired-recursive-deletes 'top
        dired-recursive-copies 'always
        dired-create-destination-dirs 'ask
        ;; dired-auto-revert-buffer #'dired-buffer-stale-p
        )

  ;; (define-keymap
  ;;   :keymap dired-mode-map
  ;;   "/" 'dired-undo
  ;;   "C-<tab>" 'dired-maybe-insert-subdir
  ;;   "<backtab>" 'dired-kill-subdir
  ;;   "<remap> <dired-do-find-regexp-and-replace>" 'dired-do-replace-regexp-as-diff
  ;;   "b" 'dired-up-directory
  ;;   "* e" 'dired-mark-executables
  ;;   "* l" 'dired-mark-symlinks
  ;;   "* d" 'dired-mark-directories
  ;;   "* r" 'dired-mark-files-regexp
  ;;   "% c" 'dired-do-copy-regexp
  ;;   "% h" 'dired-do-hardlink-regexp
  ;;   "% s" 'dired-do-symlink-regexp
  ;;   "% y" 'dired-do-relsymlink-regexp
  ;;   "% t" 'dired-flag-garbage-files
  ;;   "F" 'dired-create-empty-file
  ;;   "M-s M-s" 'dired-do-isearch
  ;;   "M-s s" 'dired-do-isearch
  ;;   "M-s M-r" 'dired-do-isearch-regexp
  ;;   "M-s r" 'dired-do-isearch-regexp)
  )


;;;; eldoc

(with-eval-after-load 'diminish
  (diminish 'eldoc-mode))

(setq eldoc-echo-area-prefer-doc-buffer t)

(elpaca eldoc-box
  (with-eval-after-load 'conn
    (keymap-set (conn-get-state-map 'conn-command-state)
                "g h" 'eldoc-box-help-at-point)))


;;;; erc

(with-eval-after-load 'erc
  (setq erc-fill-function 'erc-fill-static
        erc-fill-static-center 22
        erc-header-line-format "%n on %t (%m)"
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-lurker-threshold-time 43200
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" ; modes https://www.alien.net.au/irc/irc2numerics.html
                                  "329" ; channel creation date
                                  "332" ; topic notice
                                  "333" ; who set the topic
                                  "353" ; names notice
                                  )
        erc-autojoin-channels-alist '(("Libera.Chat" "#emacs"))
        erc-services-mode t
        erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-modules '(autojoin
                      button
                      completion
                      fill
                      irccontrols
                      list
                      log
                      match
                      menu
                      move-to-prompt
                      netsplit
                      networks
                      noncommands
                      notifications
                      readonly
                      ring
                      services
                      smiley
                      spelling
                      stamp
                      track
                      unmorse))

  (keymap-set erc-mode-map "RET" nil)
  (keymap-set erc-mode-map "C-c RET" 'erc-send-current-line)
  (keymap-set erc-mode-map "C-c C-RET" 'erc-send-current-line))


;;; Packages

;;;; Transient

(elpaca transient
  (setq transient-enable-popup-navigation nil
        ;; transient-display-buffer-action
        ;; '(display-buffer-below-selected
        ;;   (dedicated . t)
        ;;   (inhibit-same-window . t))
        transient-mode-line-format 'line))

;;;; treesit-auto

(elpaca treesit-auto
  (with-eval-after-load 'treesit
    (require 'treesit-auto)
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode 1)))


;;;; Compat

;; (elpaca (compat :repo "emacs-compat/compat"
;;                 :host github))


;;;; Diminish

(elpaca diminish
  (diminish 'visual-line-mode))

;;;; minions

;; (elpaca minions
;;   (setq minions-mode-line-lighter " ≡"
;;         minions-prominent-modes (list 'conn-local-mode
;;                                       'conn-dot-mode
;;                                       'conn-wincontrol-mode
;;                                       'defining-kbd-macro))
;;   (minions-mode 1))


;;;; expreg

(elpaca (expreg :host github :repo "casouri/expreg"))


;;;; helpful

;; (with-eval-after-load 'conn
;;   (conn-help-state-mode 1))

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


;;;; sly

(elpaca sly
  (with-eval-after-load 'sly
    (setq sly-default-lisp 'sbcl
          sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "4096"))))

    (defun sly-setup-embark ()
      (require 'embark)
      (make-local-variable 'embark-default-action-overrides)
      (make-local-variable 'conn-embark-alt-default-action-overrides)
      (setf (alist-get 'expression embark-default-action-overrides) 'sly-interactive-eval
            (alist-get 'defun embark-default-action-overrides) 'sly-eval-defun
            (alist-get 'identifier embark-default-action-overrides) 'sly-edit-definition
            (alist-get 'identifier conn-embark-alt-default-action-overrides) 'sly-documentation-lookup))
    (add-hook 'sly-mode-hook #'sly-setup-embark)
    (add-hook 'sly-repl-mode-hook #'sly-setup-embark))

  (with-eval-after-load 'smartparens
    (add-hook 'sly-mrepl-mode-hook 'smartparens-mode)))


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

(elpaca (rustic :host github
                :repo "emacs-rustic/rustic")
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

;; (elpaca lsp-mode
;;   (add-hook 'lsp-mode-hook 'lsp-ui-peek-mode)
;;   (add-hook 'lsp-mode-hook 'lsp-modeline-code-actions-mode)
;;
;;   (setq lsp-keymap-prefix "C-c s"
;;         lsp-eldoc-render-all nil
;;         lsp-enable-on-type-formatting nil
;;         lsp-ui-doc-alignment 'window
;;         lsp-ui-doc-header t
;;         lsp-ui-doc-border "black"
;;         lsp-ui-doc-background '((t (:background "#dfd9cf")))
;;         lsp-inlay-hint-face '((t (:inherit shadow :height 0.8))))
;;
;;   (setq lsp-clients-clangd-args '("-j=4"
;;                                   "--log=error"
;;                                   "--background-index"
;;                                   "--clang-tidy"
;;                                   "--cross-file-rename"
;;                                   "--header-insertion=never")
;;         lsp-zig-zls-executable "~/build/zls/zig-out/bin/zls")
;;
;;   (defun my-lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))) ;; Configure orderless
;;   (add-hook 'lsp-completion-mode-hook #'my-lsp-mode-setup-completion)
;;
;;   (with-eval-after-load 'lsp-mode
;;     (define-keymap
;;       :keymap lsp-mode-map
;;       "C-c I" 'lsp-inlay-hints-mode)))

;;;;; lsp-ui

;; (elpaca lsp-ui
;;   (with-eval-after-load 'lsp
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;     (keymap-set lsp-mode-map "M-g m" 'lsp-ui-imenu)))

;;;; dape

(elpaca dape)


;;;; j-mode

(elpaca j-mode
  (setq j-console-cmd
        (locate-file "~/build/j9.5/bin/jconsole" nil
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
  ;; (with-eval-after-load 'org
  ;;   (require 'pdf-tools))

  (setq pdf-info-epdfinfo-program "~/.emacs.d/elpaca/builds/pdf-tools/server/epdfinfo")

  (with-eval-after-load 'pdf-tools
    (setq pdf-annot-latex-header "")
    (keymap-set pdf-view-mode-map "s a" #'pdf-view-auto-slice-minor-mode)
    (keymap-set pdf-view-mode-map "c" #'pdf-view-center-in-window))

  (pdf-loader-install))

;;;;; org-pdf-tools

;; (elpaca org-pdftools
;;   (with-eval-after-load 'pdf-tools
;;     (require 'org)
;;     (require 'org-pdftools)
;;     (org-pdftools-setup-link)))


;;;; tex

(elpaca (auctex :pre-build (("./autogen.sh")
                            ("./configure"
                             "--with-texmf-dir=$(kpsewhich -var-value TEXMFHOME)")
                            ("make")))
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))


;;;; cdlatex

(elpaca cdlatex
  (add-hook 'latex-mode-hook #'cdlatex-mode)
  (add-hook 'org-mode-hook #'org-cdlatex-mode)

  (defun my-cdlatex-tab-handler ()
    (letrec ((tick (buffer-chars-modified-tick))
             (beg (point))
             (hook (lambda ()
                     (when (and (eql tick (buffer-chars-modified-tick))
                                (/= (point) beg))
                       (conn--push-ephemeral-mark beg))
                     (remove-hook 'post-command-hook hook))))
      (add-hook 'post-command-hook hook)
      nil))
  (add-hook 'cdlatex-tab-hook 'my-cdlatex-tab-handler 91)

  (with-eval-after-load 'cdlatex
    (setq
     cdlatex-math-symbol-alist '((?. ("\\cdot" "\\ldot"))
                                 (?c ("\\circ"))
                                 (?j ("\\,d")))
     cdlatex-math-modify-alist
     '((?w "\\mathbb" nil t nil nil ))
     cdlatex-command-alist
     '(("alin"      "Insert an ALIGN* environment template"
        "" cdlatex-environment ("align*") t nil)
       ("alitn"     "Insert an ALIGNAT* environment template"
        "" cdlatex-environment ("alignat*") t nil)
       ("xxan"      "Insert a XXALIGNAT environment template"
        "" cdlatex-environment ("xxalignat") t nil)
       ("muln"      "Insert a MULTINE* environment template"
        "" cdlatex-environment ("multline*") t nil)
       ("gatn"      "Insert a GATHER* environment template"
        "" cdlatex-environment ("gather*") t nil)
       ("flan"      "Insert a FLALIGN* environment template"
        "" cdlatex-environment ("flalign*") t nil))))

  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-i" 'org-cdlatex-environment-indent)))


;;;; math-delimiters

(elpaca (math-delimiters :host github :repo "oantolin/math-delimiters")
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-SPC" 'math-delimiters-insert)))


;;;; dtrt-indent

(elpaca dtrt-indent
  (push (lambda () (dtrt-indent-global-mode 1))
        my-to-incremental-load)
  (with-eval-after-load 'dtrt-indent
    (with-eval-after-load 'diminish
      (diminish 'dtrt-indent-mode))))


;;;; exec-path-from-shell

(elpaca exec-path-from-shell
  (when (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))


;;;; modus-themes

(elpaca modus-themes
  (require 'modus-themes)

  (setq modus-themes-common-palette-overrides
        (seq-concatenate
         'list
         `((bg-main "#fff5e8")
           (fg-active-argument "#630863")
           (bg-active-argument "#fcd1fc")
           (cursor "#7d0002")
           (bg-region "#f7dbd6")
           (fg-region unspecified)
           (fg-completion-match-0 "#353b44")
           (bg-completion-match-0 "#d0e4ff")
           (fg-completion-match-1 "#384231")
           (bg-completion-match-1 "#cdf3b5")
           (fg-completion-match-2 "#3b3544")
           (bg-completion-match-2 "#d1baf1")
           (fg-completion-match-3 "#313c37")
           (bg-completion-match-3 "#bef1da")
           (bg-search-lazy bg-magenta-subtle)
           (bg-search-current bg-yellow-intense))
         modus-themes-preset-overrides-warmer)
        hi-lock-face-defaults '("modus-themes-subtle-cyan"
                                "modus-themes-subtle-red"
                                "modus-themes-subtle-green"
                                "modus-themes-subtle-blue"
                                "modus-themes-subtle-yellow"))
  (custom-set-faces
   `(transient-key-stay ((t :inherit modus-themes-key-binding
                            :foreground "#008900"))))
  (load-theme 'modus-operandi-tinted t))

;;;; doric

(elpaca (doric-themes :host github
                      :repo "protesilaos/doric-themes"))


;;;; no-littering

(elpaca no-littering
  (letrec ((loader (lambda ()
                     (require 'no-littering)
                     (remove-hook 'pre-command-hook loader))))
    (add-hook 'pre-command-hook loader))

  (setq backup-by-copying t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (no-littering-theme-backups))


;;;; crux

(elpaca crux
  ;; (keymap-global-set "C-<return>"   'crux-smart-open-line)
  ;; (keymap-global-set "C-S-h" #'crux-other-window-or-switch-buffer)
  (keymap-global-set "S-<return>"    'crux-smart-open-line)
  (keymap-global-set "C-x F"         'crux-sudo-edit)
  (keymap-global-set "C-x W"         'crux-open-with)
  (keymap-global-set "C-<backspace>" 'crux-kill-whole-line)
  (keymap-global-set "C-c s" 'crux-create-scratch-buffer)
  (define-key global-map [remap kill-whole-line] 'crux-kill-whole-line)
  (define-key global-map [remap kill-line] 'crux-smart-kill-line)
  (define-key global-map [remap open-line] 'crux-smart-open-line)
  (keymap-global-set "<remap> <whitespace-cleanup>" 'crux-cleanup-buffer-or-region)
  (keymap-global-set "C-S-k" 'crux-kill-line-backwards)
  (keymap-global-set "C-c S" 'crux-visit-shell-buffer)

  (with-eval-after-load 'conn
    (keymap-set (conn-get-state-map 'conn-command-state) "S" 'crux-visit-shell-buffer)
    (keymap-set ctl-x-x-map "b" 'crux-rename-file-and-buffer)

    (define-keymap
      :keymap global-map
      "<conn-edit-map> D"   'crux-duplicate-and-comment-current-line-or-region
      "<conn-edit-map> @"   'crux-insert-date)))


;;;; posframe

(elpaca spinner)

(when window-system
  (elpaca posframe
    (push (lambda () (require 'posframe))
          my-to-incremental-load)))


;;;; isearch+

;; (elpaca (isearch+ :host github
;;                   :repo "emacsmirror/isearch-plus"
;;                   :main "isearch+.el")
;;   (run-with-timer 2 nil (lambda () (require 'isearch+)))
;;   (with-eval-after-load 'isearch+
;;     ;; (require 'isearch+)
;;     (setq isearchp-dimming-color "#cddfcc"
;;           isearchp-lazy-dim-filter-failures-flag nil
;;           isearchp-restrict-to-region-flag nil
;;           isearchp-deactivate-region-flag nil
;;           isearchp-movement-unit-alist '((?w . forward-word)
;;                                          (?s . forward-sexp)
;;                                          (?i . forward-list)
;;                                          (?s . forward-sentence)
;;                                          (?c . forward-char)
;;                                          (?l . forward-line)))
;;     (require 'transient)
;;
;;     (setopt isearchp-initiate-edit-commands nil)
;;
;;     (define-keymap
;;       :keymap isearch-mode-map
;;       "C-y m" 'isearchp-yank-sexp-symbol-or-char
;;       "C-y o" 'isearchp-yank-word-or-char-forward
;;       "C-y u" 'isearchp-yank-word-or-char-backward
;;       "C-y i" 'isearchp-yank-line-backward
;;       "C-y k" 'isearchp-yank-line-forward
;;       "C-y l" 'isearchp-yank-char
;;       "C-y r" 'my-isearch-yank-region)
;;
;;     (defun my-supress-in-macro () executing-kbd-macro)
;;     (advice-add 'isearchp-highlight-lighter :before-until 'my-supress-in-macro)
;;
;;     (transient-define-prefix my-isearch+-do-filter (filter-action)
;;       "Isearch+ add filter-action prefix"
;;       [ :description "Filter"
;;         [("c" " [;]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[;]"         isearchp-in-comment-p               "[;]")))
;;           :transient transient--do-return)
;;          ("C" "~[;]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[;]"         isearchp-not-in-comment-p           "~[;]")))
;;           :transient transient--do-return)
;;
;;          ("'" " [\"]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[\"]"        isearchp-in-string-p                "[\"]")))
;;           :transient transient--do-return)
;;          ("\"" "~[\"]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[\"]"        isearchp-not-in-string-p            "~[\"]")))
;;           :transient transient--do-return)]
;;
;;         [(";" " [\"|;]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[\"|;]"      isearchp-in-string-or-comment-p     "[\"|;]")))
;;           :transient transient--do-return)
;;          (":" "~[\"|;]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[\"|;]"      isearchp-not-in-string-or-comment-p "~[\"|;]")))
;;           :transient transient--do-return)
;;
;;          ("d" " [defun]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[defun]"     isearchp-in-defun-p                 "[DEFUN]")))
;;           :transient transient--do-return)
;;          ("D" "~[defun]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[defun]"     isearchp-not-in-defun-p             "~[DEFUN]")))
;;           :transient transient--do-return)]
;;
;;         [("(" " [()]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[()]"        isearchp-in-list-p                  "[()]")))
;;           :transient transient--do-return)
;;          (")" "~[()]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[()]"        isearchp-not-in-list-p              "~[()]")))
;;           :transient transient--do-return)
;;
;;          ("[" " [page]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[page]"      isearchp-in-page-p                  "[PAGE]")))
;;           :transient transient--do-return)
;;          ("]" "~[page]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[page]"      isearchp-not-in-page-p              "~[PAGE]")))
;;           :transient transient--do-return)]
;;
;;         [("f" " [file|url]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[file|url])" isearchp-in-file-or-url-p           "[FILE|URL])")))
;;           :transient transient--do-return)
;;          ("F" "~[file|url]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[file|url])" isearchp-not-in-file-or-url-p       "~[FILE|URL])")))
;;           :transient transient--do-return)
;;
;;          ("n" " [narrow]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '( "[narrow]"     conn-isearch-in-narrow-p         "[NARROW]")))
;;           :transient transient--do-return)
;;          ("N" "~[narrow]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (funcall
;;              filter-action
;;              '("~[narrow]"     conn-isearch-in-narrow-p     "~[NARROW]")))
;;           :transient transient--do-return)]
;;
;;         [("t" " [thing]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (let* ((regions
;;                     (catch 'regions
;;                       (with-isearch-suspended
;;                        (throw 'regions
;;                               (cdr (conn-read-thing-region "Thing Mover"))))))
;;                    (regions (or (conn--merge-regions (cdr regions) t)
;;                                 regions))
;;                    (in-regions-p (lambda (beg end)
;;                                    (cl-loop for (nbeg . nend) in regions
;;                                             thereis (<= nbeg beg end nend)))))
;;               (funcall
;;                filter-action
;;                `("[thing]"     ,in-regions-p     "[THING]"))))
;;           :transient transient--do-return)
;;          ("T" " ~[thing]"
;;           (lambda (filter-action)
;;             (interactive (list (oref transient-current-prefix scope)))
;;             (let* ((regions
;;                     (catch 'regions
;;                       (with-isearch-suspended
;;                        (throw 'regions
;;                               (cdr (conn-read-thing-region "Thing Mover"))))))
;;                    (regions (or (conn--merge-regions (cdr regions) t)
;;                                 regions))
;;                    (not-in-regions-p (lambda (beg end)
;;                                        (cl-loop for (nbeg . nend) in regions
;;                                                 never (or (<= nbeg beg nend)
;;                                                           (<= nbeg end nend))))))
;;               (funcall
;;                filter-action
;;                `("~[thing]"     ,not-in-regions-p     "~[THING]"))))
;;           :transient transient--do-return)]]
;;       (interactive (list nil))
;;       (transient-setup 'my-isearch+-do-filter nil nil :scope filter-action))
;;
;;     (transient-define-prefix my-isearch+-filter-prefix ()
;;       "Isearch+ filter prefix"
;;       :transient-non-suffix 'transient--do-leave
;;       [["Filter"
;;         ("k" "Keep" isearchp-keep-filter-predicate :transient t)
;;         ("s" "Set" isearchp-keep-filter-predicate :transient t)
;;         ("0" "Reset" isearchp-reset-filter-predicate :transient t)]
;;        ["Last Filter"
;;         ("p" "Pop"
;;          (lambda ()
;;            (interactive)
;;            (isearchp-remove-filter-predicate
;;             (format "%s"
;;                     (and (advice--p isearch-filter-predicate)
;;                          (isearchp-last-isearch-advice)))
;;             t))
;;          :transient t)
;;         ("l" "Or"
;;          (lambda ()
;;            (interactive)
;;            (my-isearch+-do-filter 'isearchp-or-last-filter))
;;          :transient transient--do-recurse)
;;         ("n" "Negate" isearchp-negate-last-filter :transient t)]
;;        ["Add Filter"
;;         ("a" "And"
;;          (lambda ()
;;            (interactive)
;;            (my-isearch+-do-filter 'isearchp-add-filter-predicate))
;;          :transient transient--do-recurse)
;;         ("o" "Or"
;;          (lambda ()
;;            (interactive)
;;            (my-isearch+-do-filter 'isearchp-or-filter-predicate))
;;          :transient transient--do-recurse)
;;         ("c" "Complement" isearchp-complement-filter :transient t)]])
;;
;;     (keymap-unset isearch-mode-map "C-t")
;;     (keymap-set isearch-mode-map "C-;" 'my-isearch+-filter-prefix)
;;     (keymap-set isearch-mode-map "C-y m" 'isearchp-yank-sexp-symbol-or-char)
;;     (keymap-set isearch-mode-map "C-y o" 'isearchp-yank-word-or-char-forward)
;;     (keymap-set isearch-mode-map "C-y u" 'isearchp-yank-word-or-char-backward)
;;     (keymap-set isearch-mode-map "C-y i" 'isearchp-yank-line-backward)
;;     (keymap-set isearch-mode-map "C-y k" 'isearchp-yank-line-forward)
;;     (keymap-set isearch-mode-map "C-y l" 'isearchp-yank-char)
;;     (keymap-set isearch-mode-map "C-M-o" 'isearchp-open-recursive-edit)
;;     (keymap-set isearchp-filter-map "f" 'isearchp-add-filter-predicate)
;;     (keymap-set isearchp-filter-map "r" 'isearchp-add-regexp-filter-predicate)))


;;;; elixir-ts

(elpaca elixir-ts-mode)


;;;; conn

(elpaca (conn :host github
              :depth nil
              :repo "mtll/conn")
  (with-eval-after-load 'org
    (require 'conn-org))

  (custom-set-faces
   '(conn-mark-face ((default (:inherit cursor :background "#b8a2f0"))
                     (((background light)) (:inherit cursor :background "#b8a2f0"))
                     (((background dark)) (:inherit cursor :background "#a742b0"))))
   '(conn-dispatch-label-face ((t :background "#ff8bd1" :foreground "black" :bold t)))
   '(conn-dispatch-mode-line-face ((t (:inherit mode-line :background "#9ac793"))))
   '(conn-read-thing-mode-line-face ((t (:inherit mode-line :background "#98a3d4")))))

  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "f" 'conn-dispatch-on-things))

  (with-eval-after-load 'org
    (keymap-set org-mode-map "<conn-thing-map> M" 'conn-mark-org-math)
    (keymap-set org-mode-map "<conn-thing-map> m" 'conn-mark-org-inner-math))

  (with-eval-after-load 'ibuffer
    (keymap-set ibuffer-mode-map "f" 'conn-dispatch-on-things))

  (with-eval-after-load 'conn
    (keymap-global-set "M-n" (conn-remap-key "<conn-edit-map>"))
    (keymap-global-set "M-r" (conn-remap-key "<conn-region-map>"))
    (keymap-set (conn-get-state-map 'conn-emacs-state) "C-'" 'conntext-state))

  (setq conn-wincontrol-initial-help nil
        conn-read-string-timeout 0.35
        conn-bind-isearch-mode-keys t)

  (defun my-add-mode-abbrev (arg)
    (interactive "P")
    (add-mode-abbrev (or arg 0)))

  (conn-mode 1)
  (conntext-outline-mode 1)

  (setq conn-simple-label-characters
        (list "d" "j" "f" "k" "s" "g" "h" "l" "w" "e"
              "r" "t" "y" "u" "i" "c" "v" "b" "n" "m"
              "2" "3" "4" "5" "6" "7" "8" "x" "," "a"
              ";" "q" "p"))

  (keymap-global-set "C-x l" 'next-buffer)
  (keymap-global-set "C-x j" 'previous-buffer)
  (keymap-set (conn-get-state-map 'conn-command-state) "<up>" 'conn-backward-line)
  (keymap-set (conn-get-state-map 'conn-command-state) "<down>" 'forward-line)
  (keymap-set (conn-get-state-map 'conn-command-state) "C-<left>" 'conntext-state)

  (defvar-keymap conn-buffer-repeat-map
    :repeat t
    "l" 'next-buffer
    "j" 'previous-buffer)
  (add-hook 'outline-minor-mode-hook 'conntext-outline-mode)

  (defun my-org-capture-buffer-p (buffer &rest _alist)
    (bound-and-true-p org-capture-mode))

  (setf (alist-get "\\*Edit Macro\\*" conn-buffer-state-setup-alist #'equal)
        #'conn-setup-command-state
        (alist-get 'my-org-capture-buffer-p conn-buffer-state-setup-alist)
        #'conn-setup-command-state)

  (define-keymap
    :keymap global-map
    "<remap> <scroll-other-window>" 'conn-wincontrol-other-window-scroll-up
    "<remap> <scroll-other-window-down>" 'conn-wincontrol-other-window-scroll-down
    "M-\\"  'conn-kapply-prefix
    "C-x ," 'subword-mode
    "C-;" 'conn-wincontrol
    "C-c v" 'conn-toggle-mark-command
    "M-j" 'conn-open-line-and-indent
    "M-o" 'conn-open-line
    "C-o" 'conn-open-line-above
    "C-." 'conn-dispatch-on-things
    "C-<backspace>" 'kill-whole-line
    "S-<return>" 'conn-open-line-and-indent
    "M-`" 'conn-wincontrol-quit-other-window-for-scrolling
    "M-U" 'conn-wincontrol-maximize-vertically
    "M-z" 'conn-exchange-mark-command
    "C-SPC" 'conn-set-mark-command
    "C-x n" 'set-goal-column)

  ;; (keymap-set (conn-get-state-map 'conn-emacs-state) "<escape>" 'conn-command-state)
  (keymap-set (conn-get-state-map 'conn-org-state) "<f8>" 'conn-command-state)
  ;; (keymap-set (conn-get-major-mode-map 'conn-command-state 'org-mode) "," 'conn-org-edit-state)
  ;; (keymap-set (conn-get-major-mode-map 'conn-emacs-state 'org-mode) "<f9>" 'conn-org-edit-state)
  (keymap-set (conn-get-state-map 'conn-emacs-state) "C-M-;" 'conn-wincontrol-one-command)
  (keymap-set (conn-get-state-map 'conn-command-state) "B" 'my-ibuffer-maybe-project)
  (keymap-set (conn-get-state-map 'conn-command-state) "C-M-;" 'conn-wincontrol-one-command)
  (keymap-set (conn-get-state-map 'conn-command-state) "*" 'calc-dispatch)
  (keymap-set (conn-get-state-map 'conn-command-state) "!" 'my-add-mode-abbrev)
  (keymap-set (conn-get-state-map 'conn-command-state) "@" 'inverse-add-mode-abbrev)
  (keymap-global-set "C-c c" (conn-remap-key "C-c C-c"))
  (keymap-global-set "<mouse-3>" 'conn-last-dispatch-at-mouse)
  (with-eval-after-load 'outline
    (keymap-set outline-minor-mode-map "M-h" 'conn-outline-state-prev-heading))
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-h" 'conn-org-edit-state-prev-heading))

  (defun my-space-after-point (N)
    (interactive "p")
    (save-excursion
      (let ((last-command-event ?\ ))
        (self-insert-command N))))
  (keymap-global-set "S-SPC" 'my-space-after-point))

;;;;; conn extensions

(elpaca (conn-posframe :host github
                       :repo "mtll/conn"
                       :files ("extensions/conn-posframe.el"))
  (letrec ((hook (lambda ()
                   (require 'posframe)
                   (remove-hook 'conn-wincontrol-mode-hook hook))))
    (add-hook 'conn-wincontrol-mode-hook hook))
  (setq conn-window-labeling-function 'conn-posframe-window-label)
  (with-eval-after-load 'posframe
    (conn-posframe-mode 1)))

(elpaca (conn-consult :host github
                      :repo "mtll/conn"
                      :files ("extensions/conn-consult.el"))
  (with-eval-after-load 'consult
    (require 'conn-consult))
  (with-eval-after-load 'conn
    (keymap-global-set "<conn-region-map> o" 'conn-consult-line-region)
    (keymap-global-set "<conn-region-map> O" 'conn-consult-line-multi-region)
    (keymap-global-set "<conn-region-map> g" 'conn-consult-ripgrep-region)
    (keymap-global-set "<conn-region-map> v" 'conn-consult-git-grep-region)))

(with-eval-after-load 'conn
  (keymap-set (conn-get-state-map 'conn-command-state) "TAB" 'conn-embark-dwim-either)
  (keymap-set (conn-get-state-map 'conn-org-state) "TAB" 'conn-embark-dwim-either)

  (defun conn-embark-dwim-either (&optional arg)
    (interactive "P")
    (require 'embark)
    (if arg (conn-embark-alt-dwim) (embark-dwim)))

  (defvar my-embark-smart-tab-target-finders
    '(;; embark-target-active-region
      my-embark-abbrev-target-finder
      my-embark-commit-target-finder
      my-embark-button-target
      my-embark-cve-target-finder
      my-embark-gnu-bug-finder
      my-embark-gh-issue-finder
      my-embark-abbrev-target-finder
      embark-org-target-link
      embark-target-collect-candidate
      embark-target-text-heading-at-point
      embark-start-of-defun-target-finder
      ;; embark-target-flymake-at-point
      ;; embark-target-package-at-point
      embark-target-url-at-point
      embark-target-file-at-point
      ;; embark-target-custom-variable-at-point
      ;; embark-target-identifier-at-point
      ;; embark-target-prog-heading-at-point
      ))

  (defun my-embark-smart-tab (arg)
    (interactive "P")
    (require 'embark)
    (pcase indent-line-function
      ('indent-relative
       (or (indent-relative nil t)
           (condition-case _
               (conn-embark-dwim-either arg)
             (user-error (completion-at-point)))))
      ((or 'indent-relative-first-indent-point
           'indent-relative-maybe)
       (or (marker-position (indent-relative nil t))
           (condition-case _
               (conn-embark-dwim-either arg)
             (user-error (completion-at-point)))))
      (_
       (condition-case _
           (cl-letf (((symbol-function 'completion-at-point)))
             (advice-add 'completion-at-point :override
                         (lambda ()
                           (let ((embark-target-finders
                                  (seq-intersection my-embark-smart-tab-target-finders
                                                    embark-target-finders
                                                    #'eq)))
                             (conn-embark-dwim-either arg))))
             (indent-for-tab-command))
         (user-error (completion-at-point))))))

  (keymap-global-set "TAB" 'my-embark-smart-tab)

  (with-eval-after-load 'embark
    (defcustom conn-embark-alt-default-action-overrides
      '((identifier . xref-find-references))
      "`embark-default-action-overrides' for alternate actions."
      :type '(alist :key-type (choice (symbol :tag "Type")
                                      (cons (symbol :tag "Type")
                                            (symbol :tag "Command")))
                    :value-type (function :tag "Default action"))
      :group 'conn-embark)

    (defcustom conn-embark-alt-key "M-RET"
      "Key for embark-alt-dwim."
      :type 'string
      :group 'conn-embark)

    (defun conn-embark-alt--default-action (type)
      "`embark--default-action' for alt actions"
      (or (alist-get (cons type embark--command) conn-embark-alt-default-action-overrides
                     nil nil #'equal)
          (alist-get type conn-embark-alt-default-action-overrides)
          (alist-get t conn-embark-alt-default-action-overrides)
          (keymap-lookup (embark--raw-action-keymap type) conn-embark-alt-key)))

    (defun conn-embark-alt-dwim (&optional arg)
      "alternate `embark-dwim'."
      (interactive "P")
      (if-let* ((targets (embark--targets)))
          (let* ((target
                  (or (nth
                       (if (or (null arg) (minibufferp))
                           0
                         (mod (prefix-numeric-value arg) (length targets)))
                       targets)))
                 (type (plist-get target :type))
                 (default-action (conn-embark-alt--default-action type))
                 (action (or (command-remapping default-action) default-action)))
            (unless action
              (user-error "No alt action for %s targets" type))
            (when (and arg (minibufferp)) (setq embark--toggle-quit t))
            (embark--act action
                         (if (and (eq default-action embark--command)
                                  (not (memq default-action
                                             embark-multitarget-actions)))
                             (embark--orig-target target)
                           target)
                         (embark--quit-p action)))
        (user-error "No target found.")))

    (keymap-global-unset "S-<down-mouse-1>")

    (defun my-embark-dwim-mouse (event)
      (interactive "e")
      (mouse-minibuffer-check event)
      (let* ((start-posn (event-start event))
             (start-point (posn-point start-posn))
             (start-window (posn-window start-posn)))
        (with-selected-window start-window
          (goto-char start-point)
          (embark-dwim))))
    (keymap-global-set "S-<mouse-1>" 'my-embark-dwim-mouse)

    (defun my-embark-alt-dwim-mouse (event)
      (interactive "e")
      (mouse-minibuffer-check event)
      (let* ((start-posn (event-start event))
             (start-point (posn-point start-posn))
             (start-window (posn-window start-posn)))
        (with-selected-window start-window
          (goto-char start-point)
          (conn-embark-alt-dwim))))
    (keymap-global-set "S-<mouse-3>" 'my-embark-alt-dwim-mouse)

    (keymap-global-set "<mouse-2>" 'xref-go-back)

    (keymap-set (conn-get-state-map 'conn-emacs-state) "C-TAB" 'embark-act)

    (define-keymap
      :keymap embark-general-map
      "R" 'conn-embark-replace-region)

    (keymap-set embark-kill-ring-map "r" 'conn-embark-replace-region)
    (keymap-unset embark-expression-map "D")
    (keymap-unset embark-defun-map "D")))

(elpaca (conn-expand-region :host github
                            :repo "mtll/conn"
                            :files ("extensions/conn-expand-region.el"))
  (cl-pushnew 'conn-er-expansions conn-expansion-functions))

(elpaca (conn-expreg :host github
                     :repo "mtll/conn"
                     :files ("extensions/conn-expreg.el"))
  (cl-pushnew 'conn-expreg-expansions conn-expansion-functions))

(elpaca (conn-smartparens :host github
                          :repo "mtll/conn"
                          :files ("extensions/conn-smartparens.el"))
  (with-eval-after-load 'smartparens
    (require 'conn-smartparens)
    (add-hook 'lisp-data-mode-hook 'conntext-smartparens-mode)))

;; (when (>= emacs-major-version 30)
;;   (elpaca (conn-treesit :host github
;;                         :repo "mtll/conn"
;;                         :files ("extensions/conn-treesit.el"))
;;     (with-eval-after-load 'treesit
;;       (require 'conn-treesit))))


;;;; orderless set operations

;; (elpaca (orderless-set-operations :host github
;;                                   :repo "mtll/orderless-set-operations")
;;   (with-eval-after-load 'orderless
;;     (oso-mode 1)))


;;;; expand region

(elpaca expand-region)


;;;; ialign

(elpaca ialign
  (with-eval-after-load 'conn
    (keymap-global-set "<conn-region-map> a i" 'ialign))

  (with-eval-after-load 'embark
    (defun embark-ialign (_reg)
      (ialign (region-beginning) (region-end)))

    (keymap-set embark-region-map "a" 'embark-ialign)))


;;;; magit

(elpaca llama)

(elpaca (magit :host github :repo "magit/magit" :files (:defaults "git-commit.el"))
  (push (lambda () (require 'magit)) my-to-incremental-load)
  (with-eval-after-load 'nerd-icons
    (setq magit-format-file-function #'magit-format-file-nerd-icons))

  (keymap-global-set "C-c m f" 'magit-file-dispatch)
  (keymap-global-set "C-c m s" 'magit-status)
  (keymap-global-set "C-c m d" 'magit-dispatch))


;;;; flycheck

(elpaca flycheck
  (setq lsp-diagnostics-flycheck-default-level 'warning))


;;;; cape

(elpaca cape
  ;; (keymap-global-set "M-L" #'cape-line)
  ;; (keymap-global-set "M-K" #'cape-dict)
  ;; (keymap-global-set "C-M-h" #'cape-dabbrev)
  ;; M-h C-M-j M-u M-n M-p

  (cl-pushnew #'cape-file completion-at-point-functions)

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
    (add-hook 'eglot-managed-mode-hook #'wrap-eglot-capf))

  ;; (defun dictionary-doc-lookup (cand)
  ;;   (let* ((buffer)
  ;;          (dictionary-display-definition-function
  ;;           (lambda (word dictionary definition)
  ;;             (let ((help-buffer-under-preparation t))
  ;;               (help-setup-xref (list #'dictionary-search word dictionary)
  ;;                                (called-interactively-p 'interactive))
  ;;               (with-current-buffer (help-buffer)
  ;;                 (insert definition)
  ;;                 (goto-char (point-min))
  ;;                 (setq buffer (current-buffer)))))))
  ;;     (dictionary-search cand)
  ;;     buffer))
  ;;
  ;; (add-hook 'text-mode-hook
  ;;           (lambda ()
  ;;             (keymap-set text-mode-map "C-M-i" 'completion-at-point)
  ;;             (add-to-list 'completion-at-point-functions
  ;;                          (cape-capf-properties
  ;;                           #'cape-dict
  ;;                           :company-doc-buffer #'dictionary-doc-lookup))))
  )


;;;; embark

(elpaca embark
  (with-eval-after-load 'consult
    (require 'embark))

  (setq embark-quit-after-action t
        embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator)
        embark-prompter 'embark-keymap-prompter
        embark-cycle-key "<tab>"
        embark-help-key "?"
        embark-confirm-act-all nil
        prefix-help-command 'embark-prefix-help-command)

  (keymap-global-set "M-." 'embark-act)
  (keymap-global-set "C-<tab>" 'embark-act)
  ;; (keymap-global-set "C-<tab>" 'embark-act)
  (keymap-global-set "M-S-<iso-lefttab>" 'embark-bindings)
  (keymap-set minibuffer-mode-map "C-M-," 'embark-export)

  (defun embark-start-of-defun-target-finder ()
    (when-let* ((bounds (bounds-of-thing-at-point 'defun))
                ((= (point) (car bounds))))
      (cons 'defun (cons (buffer-substring (car bounds) (cdr bounds))
                         bounds))))

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
    "RET" 'my-forward-page
    "M-RET" 'my-backward-page
    "n" 'forward-page
    "p" 'backward-page
    "u" 'narrow-to-page
    "m" 'mark-page)

  (defvar-keymap my-embark-tab-map
    "d" 'embark-tab-delete
    "r" 'embark-tab-rename
    "t" 'embark-tab-detach)

  (with-eval-after-load 'embark
    (keymap-set embark-file-map "C-s" 'multi-isearch-files)
    (cl-pushnew 'multi-isearch-files embark-multitarget-actions)

    (keymap-set embark-buffer-map "t" 'switch-to-buffer-other-tab)
    (keymap-set embark-buffer-map "C-s" 'multi-isearch-buffers)
    (cl-pushnew 'multi-isearch-buffers embark-multitarget-actions)

    (keymap-set embark-general-map "M-TAB" 'embark-toggle-quit)
    (keymap-set embark-general-map "M-<tab>" 'embark-toggle-quit)
    (keymap-unset embark-general-map "q")

    (cl-pushnew 'my-embark-tab-map (alist-get 'tab embark-keymap-alist))
    (setf (alist-get 'page embark-keymap-alist) (list 'embark-page-map))

    (keymap-set embark-region-map "RET" 'copy-region-as-kill)
    (keymap-set embark-defun-map "n" 'narrow-to-defun)
    (keymap-set embark-symbol-map "h" 'helpful-symbol)
    (keymap-set embark-collect-mode-map "C-j" 'consult-preview-at-point)
    (keymap-set embark-identifier-map "M-RET" 'xref-find-references)
    (keymap-set embark-heading-map "RET" #'outline-cycle)
    (keymap-set embark-heading-map "M-RET" #'outline-up-heading)
    (keymap-set embark-symbol-map "RET" #'xref-find-definitions)

    (keymap-set embark-file-map "O" 'find-file-other-frame)
    (keymap-set embark-buffer-map "O" 'display-buffer-other-frame)

    (keymap-set embark-heading-map "RET" #'bicycle-cycle)
    (with-eval-after-load 'org
      (with-eval-after-load 'embark
        (keymap-set embark-org-heading-map "RET" #'bicycle-cycle)))

    ;; (keymap-set embark-heading-map "RET" #'conn-outline-state)
    ;; (with-eval-after-load 'org
    ;;   (with-eval-after-load 'embark
    ;;     (keymap-set embark-org-heading-map "RET" #'conn-org-edit-state)))

    (defun my-embark-abbrev-target-finder ()
      (pcase-let ((`(,sym ,name ,wordstart ,wordend) (abbrev--before-point)))
        (when sym `(abbrev ,name ,wordstart . ,wordend))))
    (cl-pushnew 'my-embark-abbrev-target-finder embark-target-finders)

    (defvar-keymap my-embark-abbrev-map
      "RET" 'expand-abbrev
      "M-RET" 'edit-abbrevs)
    (setf (alist-get 'abbrev embark-keymap-alist)
          (list 'my-embark-abbrev-map))

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
      (when (and (save-excursion
                   (beginning-of-line)
                   (looking-at page-delimiter))
                 (not (window-minibuffer-p (selected-window))))
        (let ((bounds (bounds-of-thing-at-point 'page)))
          (cons 'page (cons
                       (buffer-substring (car bounds) (cdr bounds))
                       bounds)))))
    (add-hook 'embark-target-finders #'embark-looking-at-page-target-finder -80)

    (defun embark-page-target-finder ()
      (when-let* ((bounds (bounds-of-thing-at-point 'page)))
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

      (add-hook 'embark-target-finders 'embark-org-target-link -85)

      (defun embark-refile-buffers-targets ()
        (cl-loop for buf in (buffer-list)
                 when (and (eq 'org-mode (buffer-local-value 'major-mode buf))
                           (buffer-file-name buf))
                 collect buf))
      (add-to-list 'org-refile-targets '(embark-refile-buffers-targets :maxlevel . 1))

      (defun embark-refile-grep-candidates (cands)
        (let* ((rfloc)
               (headings
                (thread-first
                  (cl-loop for cand in cands
                           for marker = (car (consult--grep-position cand))
                           for heading = (with-current-buffer (marker-buffer marker)
                                           (when (and (eq major-mode 'org-mode)
                                                      (goto-char marker)
                                                      (ignore-errors (org-back-to-heading))
                                                      (org-at-heading-p))
                                             (unless rfloc
                                               (setq rfloc (org-refile-get-location
                                                            "Refile"
                                                            nil
                                                            org-refile-allow-creating-parent-nodes)))
                                             (point-marker)))
                           when heading collect heading)
                  (delete-dups))))
          (dolist (heading headings)
            (with-current-buffer (marker-buffer heading)
              (goto-char heading)
              (org-refile nil nil rfloc)))
          (find-file (nth 1 rfloc))))
      (cl-pushnew 'embark-refile-grep-candidates embark-multitarget-actions)

      (defun embark-refile-copy-grep-candidates (cands)
        (let* ((rfloc)
               (org-refile-keep t)
               (headings
                (thread-first
                  (cl-loop for cand in cands
                           for marker = (car (consult--grep-position cand))
                           for heading = (with-current-buffer (marker-buffer marker)
                                           (when (and (eq major-mode 'org-mode)
                                                      (goto-char marker)
                                                      (ignore-errors (org-back-to-heading))
                                                      (org-at-heading-p))
                                             (unless rfloc
                                               (setq rfloc (org-refile-get-location
                                                            "Copy"
                                                            nil
                                                            org-refile-allow-creating-parent-nodes)))
                                             (point-marker)))
                           when heading collect heading)
                  (delete-dups))))
          (dolist (heading headings)
            (with-current-buffer (marker-buffer heading)
              (goto-char heading)
              (org-refile nil nil rfloc)))
          (find-file (nth 1 rfloc))))
      (cl-pushnew 'embark-refile-copy-grep-candidates embark-multitarget-actions)

      (defvar-keymap embark-refile-grep-map
        "C-w" 'embark-refile-grep-candidates
        "M-w" 'embark-refile-copy-grep-candidates)
      (cl-pushnew 'embark-refile-grep-map (alist-get 'consult-grep embark-keymap-alist))

      (defun embark-refile-copy-location-candidates (cands)
        (when (eq major-mode 'org-mode)
          (let* ((rfloc (org-refile-get-location
                         "Copy"
                         nil
                         org-refile-allow-creating-parent-nodes))
                 (org-refile-keep t)
                 (headings
                  (thread-first
                    (cl-loop for cand in cands
                             for loc = (car (consult--get-location cand))
                             for heading = (when (and (goto-char loc)
                                                      (ignore-errors (org-back-to-heading))
                                                      (org-at-heading-p))
                                             (point))
                             when heading collect heading)
                    (delete-dups))))
            (dolist (heading headings)
              (goto-char heading)
              (org-refile nil nil rfloc))
            (find-file (nth 1 rfloc)))))
      (cl-pushnew 'embark-refile-copy-location-candidates embark-multitarget-actions)

      (defun embark-refile-location-candidates (cands)
        (when (eq major-mode 'org-mode)
          (let* ((rfloc (org-refile-get-location
                         "Refile"
                         nil
                         org-refile-allow-creating-parent-nodes))
                 (headings
                  (thread-first
                    (cl-loop for cand in cands
                             for loc = (car (consult--get-location cand))
                             for heading = (when (and (goto-char loc)
                                                      (ignore-errors (org-back-to-heading))
                                                      (org-at-heading-p))
                                             (point))
                             when heading collect heading)
                    (delete-dups))))
            (dolist (heading headings)
              (goto-char heading)
              (org-refile nil nil rfloc))
            (find-file (nth 1 rfloc)))))
      (cl-pushnew 'embark-refile-location-candidates embark-multitarget-actions)

      (defvar-keymap embark-refile-location-map
        "C-w" 'embark-refile-location-candidates
        "M-w" 'embark-refile-copy-location-candidates)
      (cl-pushnew 'embark-refile-location-map
                  (alist-get 'consult-location embark-keymap-alist))))

  (with-eval-after-load 'vertico
    (define-keymap
      :keymap vertico-map
      "C-<tab>" 'embark-act-all
      ;; "C-<tab>" 'embark-act-all
      "M-TAB" 'embark-act-persist
      "M-<tab>" 'embark-act-persist
      "C-SPC" 'embark-select
      "TAB" 'embark-act-marked
      ;; "<tab>" 'embark-act-marked
      )))

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
      "h v" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h g" 'consult-ripgrep)

    (define-keymap
      :keymap embark-general-map
      "h l" 'consult-line
      "h f" 'consult-find
      "h v" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h g" 'consult-ripgrep)))

;;;;; embark buttons

(with-eval-after-load 'embark
  (defun my-embark-gh-issue-finder ()
    (when-let* ((button (and (not (minibufferp))
                             (my-inside-regexp-in-line
                              "gh:\\([a-zA-Z-]*/[a-zA-Z-]*\\)#\\([0-9]*\\)"))))
      `(url
        ,(format "www.github.com/%s/issues/%s"
                 (match-string-no-properties 1)
                 (match-string-no-properties 2))
        ,(match-beginning 0) . ,(match-end 0))))
  (add-hook 'embark-target-finders 'my-embark-gh-issue-finder)

  (defun my-embark-gnu-bug-finder ()
    (when-let* ((button (and (not (minibufferp))
                             (my-inside-regexp-in-line
                              "bug#\\([0-9]*\\)"))))
      `(url
        ,(format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                 (match-string-no-properties 1))
        ,(match-beginning 0) . ,(match-end 0))))
  (add-hook 'embark-target-finders 'my-embark-gnu-bug-finder)

  (defun my-embark-cve-target-finder ()
    (when-let* ((button (and (not (minibufferp))
                             (my-inside-regexp-in-line
                              "\\(CVE-[0-9]\\{4\\}-[0-9]+\\)"))))
      `(url
        ,(format "https://www.cve.org/CVERecord?id=%s"
                 (match-string-no-properties 1))
        ,(match-beginning 0) . ,(match-end 0))))
  (add-hook 'embark-target-finders 'my-embark-cve-target-finder)

  (with-eval-after-load 'magit
    (defun my-embark-commit-target-finder ()
      (require 'vc)
      (when-let* ((_ (eq 'Git (vc-deduce-backend)))
                  (commit (or (magit-thing-at-point 'git-revision t)
                              (magit-branch-or-commit-at-point))))
        `(git-commit ,commit)))
    (add-hook 'embark-target-finders 'my-embark-commit-target-finder))

  (setf (alist-get 'git-commit embark-default-action-overrides)
        'magit-show-commit)

  (defvar my-button-target-functions nil)

  (defun my-embark-button-target ()
    (when (my-inside-regexp-in-line "<\\[\\([^:]+\\):\\(.*\\)\\]>")
      (when-let* ((tar (run-hook-with-args-until-success
                        'my-button-target-functions
                        (match-string 1) (match-string 2))))
        (append tar (cons (match-beginning 1) (match-end 2))))))
  (add-hook 'embark-target-finders 'my-embark-button-target)

  (defun my-bookmark-button (type bookmark)
    (require 'bookmark)
    (when (and (equal type "bmk")
               (bookmark-get-bookmark bookmark t))
      `(bookmark ,bookmark)))
  (add-hook 'my-button-target-functions 'my-bookmark-button)

  (defun my-insert-bookmark-button (bmk)
    (interactive (list (bookmark-completing-read "Bookmark: ")))
    (insert "<[bmk:" bmk "]>")))


;;;; company

;; (elpaca company
;;   (run-with-timer
;;    2 nil
;;    (lambda ()
;;      (global-company-mode 1)))

;;   (with-eval-after-load 'company
;;     (diminish 'company-mode)

;;     (define-keymap
;;       :keymap company-active-map
;;       "<tab>" 'company-complete-selection
;;       "C-n" nil
;;       "C-p" nil
;;       "<return>" nil
;;       "RET" nil)

;;     (defun just-one-face (fn &rest args)
;;       (let ((orderless-match-faces [completions-common-part]))
;;         (apply fn args)))
;;     (advice-add 'company-capf--candidates :around #'just-one-face)

;;     (defun company-capf--candidates-ad (input suffix)
;;       (require 'vertico)
;;       (let* ((res (company--capf-data))
;;              (table (nth 3 res))
;;              (pred (plist-get (nthcdr 4 res) :predicate))
;;              (meta (and res
;;                         (completion-metadata
;;                          (buffer-substring (nth 1 res) (nth 2 res))
;;                          table pred))))
;;         (company-capf--save-current-data res meta)
;;         (when res
;;           (let* ((interrupt (plist-get (nthcdr 4 res) :company-use-while-no-input))
;;                  (all-result (company-capf--candidates-1 input suffix
;;                                                          table pred
;;                                                          meta
;;                                                          (and non-essential
;;                                                               (eq interrupt t))))
;;                  (sortfun (or (cdr (assq 'display-sort-function meta))
;;                               #'vertico-sort-length-alpha))
;;                  (candidates (assoc-default :completions all-result)))
;;             (setq company-capf--sorted (functionp sortfun))
;;             (when candidates
;;               (setq company-capf--current-boundaries
;;                     (company--capf-boundaries-markers
;;                      (assoc-default :boundaries all-result)
;;                      company-capf--current-boundaries)))
;;             (when sortfun
;;               (setq candidates (funcall sortfun candidates)))
;;             candidates))))
;;     (advice-add 'company-capf--candidates :override 'company-capf--candidates-ad)))


;;;; corfu

(elpaca corfu
  (letrec ((loader (lambda ()
                     (global-corfu-mode 1)
                     (corfu-popupinfo-mode 1)
                     (corfu-echo-mode 1)
                     (corfu-history-mode 1))))
    (add-hook 'prog-mode-hook loader))

  (setq corfu-scroll-margin 2
        corfu-bar-width 0.4
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match nil
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-auto nil
        corfu-preselect 'valid
        corfu-auto-delay nil
        corfu-auto-prefix 3
        ;; corfu-map (define-keymap
        ;;             "<remap> <forward-sentence>" 'corfu-prompt-end
        ;;             "<remap> <backward-sentence>" 'corfu-prompt-beginning
        ;;             "<remap> <scroll-down-command>" #'corfu-scroll-down
        ;;             "<remap> <scroll-up-command>" #'corfu-scroll-up
        ;;             "<tab>" #'corfu-complete
        ;;             ;; "RET" nil
        ;;             ;; "<return>" nil
        ;;             "SPC" 'corfu-insert-separator
        ;;             "C-h" #'corfu-info-documentation
        ;;             "M-h" #'corfu-info-location
        ;;             "M-<" #'corfu-first
        ;;             "M->" #'corfu-last
        ;;             "M-n" #'corfu-next
        ;;             ;; "C-n" nil
        ;;             ;; "C-j" nil
        ;;             "M-p" #'corfu-previous
        ;;             ;; "C-p" #'corfu-previous
        ;;             "C-g" #'corfu-quit
        ;;             "TAB" #'corfu-complete)
        )

  (defun my-corfu-auto-on ()
    (setq-local corfu-auto t))
  ;; (add-hook 'prog-mode-hook 'my-corfu-auto-on)

  (with-eval-after-load 'corfu
    (define-keymap
      :keymap corfu-map
      "C-h" 'corfu-info-documentation
      "M-h" 'corfu-info-location
      "M-TAB" 'corfu-insert-separator
      "TAB" 'corfu-insert
      "C-g" 'corfu-quit)

    (keymap-unset corfu-map "RET" t)

    (defun corfu-sep-and-start ()
      (interactive)
      (completion-at-point)
      (corfu-insert-separator))

    (keymap-set corfu-mode-map "<remap> <completion-at-point>" #'corfu-sep-and-start)

    (with-eval-after-load 'conn
      (defun my-corfu-off ()
        (global-corfu-mode -1))
      (add-hook 'conn-macro-dispatch-start-hook 'my-corfu-off)

      (defun my-corfu-on ()
        (global-corfu-mode 1))
      (add-hook 'conn-macro-dispatch-end-hook 'my-corfu-on))))


;;;; nerd icons

(elpaca nerd-icons
  (push (lambda () (require 'nerd-icons))
        my-to-incremental-load))

(elpaca nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (with-eval-after-load 'diminish
    (diminish 'nerd-icons-dired-mode)))

(elpaca nerd-icons-corfu
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(elpaca nerd-icons-ibuffer
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  (with-eval-after-load 'diminish
    (diminish 'nerd-icons-ibuffer-mode)))

(elpaca nerd-icons-completion
  (with-eval-after-load 'marginalia
    (nerd-icons-completion-marginalia-setup)))


;;;; bicycle

(elpaca bicycle
  (with-eval-after-load 'outline
    (define-keymap
      :keymap outline-minor-mode-map
      "<backtab>" 'bicycle-cycle-global))

  (with-eval-after-load 'org
    (define-keymap
      :keymap org-mode-map
      "<backtab>" 'bicycle-cycle-global)))


;;;; outline-minor-faces

(elpaca outline-minor-faces
  (with-eval-after-load 'outline
    (add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode)))


;;;; keycast

(elpaca keycast)


;;;; wgrep

(elpaca wgrep)


;;;; separedit

(elpaca separedit
  (keymap-set prog-mode-map "C-c '" 'separedit)
  (keymap-set minibuffer-local-map "C-c '" 'separedit)
  (with-eval-after-load 'help-mode
    (keymap-set help-mode-map "C-c '" 'separedit)))


;;;; package-link-flymake

(elpaca package-lint)


;;;; orderless

(elpaca orderless
  (letrec ((loader (lambda ()
                     (require 'orderless)
                     (remove-hook 'minibuffer-setup-hook loader))))
    (add-hook 'minibuffer-setup-hook loader))
  (with-eval-after-load 'orderless
    (setq orderless-affix-dispatch-alist '((?\= . orderless-literal)
                                           (?! . orderless-without-literal)
                                           (?, . orderless-initialism))
          completion-styles '(orderless basic)
          orderless-kwd-prefix ?`
          orderless-kwd-separator "`="
          orderless-matching-styles '(orderless-literal
                                      orderless-regexp)
          orderless-style-dispatchers '(orderless-kwd-dispatch
                                        orderless-affix-dispatch
                                        flex-first-if-completing)
          completion-category-overrides '((file (styles orderless+flex
                                                        partial-completion
                                                        basic))
                                          (lsp-capf (styles orderless+flex))
                                          (consult-location (styles orderless-loc))
                                          (consult-grep (styles orderless-loc)))
          orderless-component-separator #'orderless-escapable-split-on-space
          orderless-smart-case t
          read-file-name-completion-ignore-case nil
          read-buffer-completion-ignore-case nil)

    (defun my-quote-region-for-orderless (string)
      (concat
       (string (car (rassq 'orderless-literal orderless-affix-dispatch-alist)))
       (string-replace " " "\\ " string)))
    (setq conn-completion-region-quote-function 'my-quote-region-for-orderless)

    (defun flex-first-if-completing (pattern index _total)
      (when (and (= index 0)
                 (or completion-in-region-mode
                     (bound-and-true-p company-candidates)))
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
                                     flex-first)))

    (orderless-define-completion-style orderless-loc
      (orderless-matching-styles '(orderless-literal orderless-regexp))
      (orderless-affix-dispatch '((?\= . orderless-literal)))
      (orderless-style-dispatchers '(orderless-affix-dispatch)))

    (orderless-define-completion-style orderless-literal
      (orderless-matching-styles '(orderless-literal))
      (orderless-affix-dispatch '((?\~ . orderless-regexp)))
      (orderless-style-dispatchers '(orderless-affix-dispatch)))))


;;;; consult

(elpaca consult
  (setq consult--gc-percentage 0.5
        consult-async-min-input 3
        consult-yank-rotate t
        consult-narrow-key "M-N"
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0.3
        ;; register-preview-function #'consult-register-format
        completion-in-region-function #'consult-completion-in-region
        consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-modified-buffer
                                 consult--source-buffer
                                 consult--source-bookmark
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-project-buffer-hidden
                                 consult--source-project-recent-file-hidden))

  (keymap-global-set "M-g y" #'consult-global-mark)
  (keymap-global-set "<remap> <Info-search>" #'consult-info)
  (keymap-global-set "<remap> <bookmark-jump>" #'consult-bookmark)
  (keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
  (keymap-global-set "<remap> <yank-from-kill-ring>" #'consult-yank-from-kill-ring)
  (keymap-global-set "<remap> <jump-to-register>" #'consult-register-load)
  (keymap-global-set "<remap> <switch-to-buffer>" 'consult-buffer)
  (keymap-global-set "<remap> <execute-extended-command-for-buffer>" 'consult-mode-command)
  (keymap-global-set "C-x M-:" 'consult-complex-command)
  (keymap-global-set "<remap> <repeat-complex-command>" 'consult-complex-command)
  (keymap-global-set "C-h i" 'consult-info)
  (keymap-global-set "C-h TAB" 'info)
  (keymap-global-set "<remap> <imenu>" 'consult-imenu)
  (keymap-global-set "M-g I" 'consult-imenu-multi)

  (keymap-set minibuffer-local-map "M-r" 'consult-history)

  (define-keymap
    :keymap search-map
    "c" 'occur
    "y" 'rgrep
    "p" 'consult-page
    "K" 'consult-kmacro
    "w" 'consult-man
    "e" 'consult-isearch-history
    "t" 'consult-outline
    "o" 'consult-line
    "O" 'consult-line-multi
    "v" 'consult-git-grep
    "g" 'consult-ripgrep
    "f" 'consult-find
    "L" 'consult-locate
    "k" 'consult-keep-lines
    "h f" 'consult-focus-lines
    "i" 'my-consult-grep-file)

  (keymap-set goto-map "g" 'consult-goto-line)
  (keymap-global-set "<remap> <project-switch-to-buffer>" 'consult-project-buffer)

  (define-keymap
    :keymap isearch-mode-map
    "M-s j" 'consult-line
    "M-s J" 'consult-line-multi)

  (defun my-consult-grep-file ()
    (interactive)
    (if buffer-file-name
        (consult-ripgrep (list buffer-file-name))
      (consult-line)))

  (with-eval-after-load 'consult
    (consult-customize consult-completion-in-region :preview-key nil)
    (consult-customize consult--source-bookmark :preview-key "C-o")
    (consult-customize consult-bookmark :preview-key "C-o")
    (consult-customize consult-buffer :preview-key "C-o")
    (consult-customize consult-project-buffer :preview-key "C-o"))

  (defun conn-occur-keep-lines ()
    (interactive)
    (let ((inhibit-read-only t))
      (call-interactively 'consult-keep-lines)))

  (defun conn-occur-flush-lines ()
    (interactive)
    (let ((inhibit-read-only t))
      (call-interactively 'consult-keep-lines)))

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
                 (if-let* (fun (bound-and-true-p outline-search-function))
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
  ;; (advice-add 'register-preview :override #'consult-register-window)

  (defun my-consult-goto-edit ()
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
  (keymap-set goto-map "x" 'my-consult-goto-edit)

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
        "M-RET" 'consult-org-link-location))))

;;;;; consult-lsp

;; (elpaca consult-lsp
;;   (with-eval-after-load 'lsp-mode
;;     (keymap-set lsp-mode-map "M-s x" #'consult-lsp-symbols)
;;     (keymap-set lsp-mode-map "M-s >" #'consult-lsp-diagnostics)
;;     (keymap-set lsp-mode-map "M-s <" #'consult-lsp-file-symbols)))

;;;;; consult-projectile

(elpaca consult-projectile
  (keymap-global-set "C-c j" 'consult-projectile)
  (with-eval-after-load 'projectile
    (with-eval-after-load 'consult
      (consult-customize consult-projectile :preview-key "C-o"))))


;;;; vertico

(elpaca (vertico :files (:defaults "extensions/*"))
  (setq resize-mini-windows t
        vertico-preselect 'first
        vertico-buffer-hide-prompt nil
        vertico-cycle t
        vertico-multiform-categories '((t buffer))
        vertico-count 0)

  (face-spec-set 'vertico-current
                 '((t :inherit region)))
  (face-spec-set 'vertico-group-title
                 '((t :inherit modus-themes-heading-0 :italic t :bold t)))

  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)

  (keymap-set vertico-multiform-map "M-i" 'vertico-multiform-buffer)
  (keymap-set vertico-multiform-map "M-h" 'vertico-multiform-flat)

  (defun vertico-buffer-setup-ad ()
    (with-selected-window (active-minibuffer-window)
      (setq-local header-line-format nil
                  truncate-lines t)))
  (advice-add 'vertico-buffer--setup :after #'vertico-buffer-setup-ad)

  (defun vertico-buffer-setup-save-point (&rest app)
    (let ((old-state nil))
      (dolist (w (window-list))
        (push (list w (window-point w) (window-buffer w)) old-state))
      (apply app)
      (when-let* ((win (overlay-get vertico--candidates-ov 'window))
		  (state (alist-get win old-state))
                  (pt (apply #'set-marker (make-marker) state)))
        (advice-add vertico-buffer--restore :after
                    (lambda ()
                      (set-window-point win (marker-position pt))
                      (set-marker pt nil))))))
  (advice-add 'vertico-buffer--setup :around #'vertico-buffer-setup-save-point)

  ;; (defun vertico-buffer--redisplay-ad (win)
  ;;   (let ((mbwin (active-minibuffer-window)))
  ;;     (when (and mbwin vertico-buffer-mode
  ;;                (eq (window-buffer mbwin) (current-buffer))
  ;;                (not (eq win mbwin))
  ;;                ;; Without this check we would be running this
  ;;                ;; in any vertico-posframe windows every time.
  ;;                (not (equal "posframe" (frame-parameter (window-frame win) 'title))))
  ;;       (setq-local mode-line-format nil
  ;;                   header-line-format (or header-line-format "Sets:")))))
  ;; (advice-remove 'vertico-buffer--redisplay 'vertico-buffer--redisplay-ad)

  ;; I prefer it if the vertico buffer mode-line face
  ;; is not remapped to always appear active.
  ;; (defun my-vertico-buffer-stop-face-remap ()
  ;;   (setq-local face-remapping-alist
  ;;               (seq-remove (lambda (cons)
  ;;                             (eq (car cons) 'mode-line-inactive))
  ;;                           face-remapping-alist)))
  ;; not needed since I am hiding the mode-line above
  ;; (advice-add 'vertico-buffer--setup :after #'my-vertico-buffer-stop-face-remap)

  ;; Refocus the minibuffer if vertico-repeat is called with a minibuffer open.
  (defun vertico-repeat-ad (&rest _)
    (when (> (minibuffer-depth) 0)
      (select-window
       (if (and (equal (selected-window) (minibuffer-window))
                (not (with-current-buffer
                         (window-buffer (minibuffer-selected-window))
                       (eq major-mode 'minibuffer-mode))))
           (let ((buf (minibuffer-selected-window)))
             (message "Switched to %s" (buffer-name (window-buffer buf)))
             buf)
         (message "Switched to *MINIBUFFER*")
         (minibuffer-window)))
      t))
  (advice-add 'vertico-repeat :before-until #'vertico-repeat-ad)

  (defun vertico-focus-selected-window ()
    (interactive)
    (select-window (minibuffer-selected-window))
    (message "Focused other window"))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (keymap-global-set "<deletechar>" 'vertico-repeat)

  (keymap-unset vertico-map "C-j")

  (define-keymap
    :keymap vertico-map
    "<next>" #'vertico-scroll-up
    "<prior>" #'vertico-scroll-down
    "M-<up>" #'previous-history-element
    "M-<down>" #'next-history-element
    "M-i" #'vertico-insert
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    "M-<backspace>" #'vertico-directory-delete-word
    "M-DEL" #'vertico-directory-up
    "C-l" #'vertico-directory-up
    "M-RET" #'vertico-exit-input
    "C-M-<return>" #'vertico-exit-input
    "M-j" #'vertico-quick-exit
    "C-j" #'vertico-exit-input
    "C-M-j" #'vertico-quick-jump)

  (defun my-vertico-copy-or-kill (beg end)
    (interactive (list (region-beginning) (region-end)))
    (if (or (use-region-p) (not transient-mark-mode))
        (call-interactively #'kill-region)
      (kill-new (let ((cand (vertico--candidate)))
                  (if (consult--tofu-p (aref cand (1- (length cand))))
                      (substring cand 0 -1)
                    cand))))))


;;;; marginalia

(elpaca marginalia
  (letrec ((loader (lambda ()
                     (marginalia-mode 1)
                     (remove-hook 'minibuffer-setup-hook loader))))
    (add-hook 'minibuffer-setup-hook loader))

  (setq marginalia-align 'column)

  (keymap-global-set "M-A" 'marginalia-cycle)
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)

  (with-eval-after-load 'marginalia
    (defun marginalia-annotate-alias (cand)
      "Annotate CAND with the function it aliases."
      (when-let* ((sym (intern-soft cand))
                  (alias (car (last (function-alias-p sym t))))
                  (name (and (symbolp alias) (symbol-name alias))))
        (format #(" (%s)" 1 5 (face marginalia-function)) name)))

    (defun my-marginalia-annotate-binding (cand)
      "Annotate command CAND with keybinding."
      (when-let* ((sym (intern-soft cand))
                  (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
        (format #(" {%s}" 1 5 (face marginalia-key)) (key-description key))))

    (defun marginalia-annotate-command-with-alias (cand)
      "Annotate command CAND with its documentation string.
    Similar to `marginalia-annotate-symbol', but does not show symbol class."
      (when-let* ((sym (intern-soft cand)))
        (concat
         (my-marginalia-annotate-binding cand)
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
       (let ((align (text-property-any 0 (length ann) 'marginalia--align t ann)))
         (when align
           (setq marginalia--cand-width-max
                 (max marginalia--cand-width-max
                      (* (ceiling (+ (string-width cand)
                                     (compat-call string-width ann 0 align))
                                  marginalia--cand-width-step)
                         marginalia--cand-width-step))))))
      (cl-loop
       for (cand . ann) in cands collect
       (let ((align (text-property-any 0 (length ann) 'marginalia--align t ann)))
         (when align
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
    (advice-add 'marginalia--align :override 'marginalia--align-column)))


;;;; tempel

;; (elpaca tempel
;;   (keymap-global-set "M-I" 'tempel-insert)
;;   (keymap-global-set "M-TAB" 'my-tempel-expand-or-complete)
;;   (global-tempel-abbrev-mode 1)

;;   (defun my-tempel-expand-or-complete (&optional interactive)
;;     (interactive (list t))
;;     (require 'tempel)
;;     (if interactive
;;         (tempel--interactive #'my-tempel-expand-or-complete)
;;       (if-let ((templates (tempel--templates))
;;                (bounds (tempel--prefix-bounds))
;;                (name (buffer-substring-no-properties
;;                       (car bounds) (cdr bounds)))
;;                (sym (intern-soft name))
;;                (template (assq sym templates)))
;;           (progn
;;             (setq templates (list template))
;;             (list (car bounds) (cdr bounds) templates
;;                   :category 'tempel
;;                   :exclusive 'no
;;                   :exit-function (apply-partially #'tempel--exit templates nil)))
;;         (tempel-complete))))

;;   (with-eval-after-load 'tempel
;;     (keymap-set tempel-map "M-n" 'tempel-next)
;;     (keymap-set tempel-map "M-p" 'tempel-previous)
;;     (keymap-set tempel-map "M-i" 'tempel-done)

;;     (setq tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))

;;     (defun tempel-edit-template ()
;;       (interactive)
;;       (let ((default-directory (expand-file-name "templates/" user-emacs-directory)))
;;         (call-interactively 'find-file)))

;;     (defun conn-tempel-insert-ad (fn &rest args)
;;       (apply fn args)
;;       (when tempel--active (conn-emacs-state)))
;;     (advice-add 'tempel-insert :around 'conn-tempel-insert-ad)))


;;;; vundo

(elpaca vundo
  (keymap-global-set "C-x u" 'vundo)
  (with-eval-after-load 'vundo
    (setq vundo-glyph-alist vundo-unicode-symbols)))


;;;; htmlize

(elpaca htmlize)


;;;; page-break-lines

;; (elpaca page-break-lines
;;   (setq page-break-lines-max-width 72)
;;   (global-page-break-lines-mode)
;;   (with-eval-after-load 'diminish
;;     (diminish 'page-break-lines-mode)))


;;;; tuareg

;; (elpaca tuareg)


;;;; rfc-mode

(elpaca rfc-mode)


;;;; heex-ts-mode

(elpaca heex-ts-mode)


;;;; polymode

;; (elpaca polymode)


;;;; denote

(elpaca (denote :files (:defaults "denote-org-extras.el"))
  (push (lambda () (require 'denote)) my-to-incremental-load)

  (with-eval-after-load 'denote
    (denote-rename-buffer-mode 1)

    (setq denote-file-type 'text
          denote-history-completion-in-prompts nil
          denote-org-front-matter "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+category:   Denote
\n"))

  (defun denote-dired-directory ()
    (interactive)
    (require 'denote)
    (dired denote-directory))

  (defun my-denote-org ()
    (interactive)
    (require 'denote)
    (require 'org)
    (let ((denote-file-type 'org))
      (call-interactively #'denote)))

  ;; (keymap-global-set "C-c n e" #'denote-org-extras-extract-org-subtree)
  (keymap-global-set "C-c n t" #'denote)
  (keymap-global-set "C-c n s" #'denote-signature)
  (keymap-global-set "C-c n k" #'denote-rename-file-keywords)
  (keymap-global-set "C-c n r" #'denote-rename-file)
  (keymap-global-set "C-c n b" #'denote-backlinks)
  (keymap-global-set "C-c n c" #'denote-link)
  (keymap-global-set "C-c n B" #'denote-find-backlink)
  (keymap-global-set "C-c n l" #'denote-find-link)
  (keymap-global-set "C-c n o" #'my-denote-org)
  (keymap-global-set "C-c n d" #'denote-dired-directory)

  (defun my-denote-consult-find ()
    (interactive)
    (require 'denote)
    (require 'consult)
    (let ((consult-async-min-input 2)
          (consult-async-input-debounce 0.08))
      (consult-find denote-directory)))
  (keymap-global-set "C-c n f" #'my-denote-consult-find)

  (defun my-consult-denote-ripgrep-make-builder (paths)
    "Create ripgrep command line builder given PATHS."
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
              (cons (append cmd (list "-e" arg)
                            (list "-torg" "-ttxt" "-tmarkdown" "-ttoml")
                            opts paths)
                    (apply-partially #'consult--highlight-regexps
                                     (list (regexp-quote arg)) ignore-case))
            (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
              (when re
                (cons (append cmd (and (eq type 'pcre) '("-P"))
                              (list "-e" (consult--join-regexps re type))
                              (list "-torg" "-ttxt" "-tmarkdown" "-ttoml")
                              opts paths)
                      hl))))))))

  (defun my-denote-consult-ripgrep ()
    (interactive)
    (require 'denote)
    (require 'consult)
    (consult--grep "Notes" #'my-consult-denote-ripgrep-make-builder denote-directory nil))
  (keymap-global-set "C-c n g" #'my-denote-consult-ripgrep)

  (defun my-denote-consult-ripgrep-heading ()
    (interactive)
    (require 'denote)
    (require 'consult)
    (let ((style (alist-get consult-async-split-style consult-async-split-styles-alist)))
      (consult--grep "Notes" #'my-consult-denote-ripgrep-make-builder denote-directory
                     (concat "^[*]+"
                             (string (or (plist-get style :separator)
                                         (plist-get style :initial)))))))
  (keymap-global-set "C-c n h" #'my-denote-consult-ripgrep-heading)

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
         nil)))))


;;;; teco

;; (elpaca teco)


;;;; dumb-jump

(elpaca dumb-jump
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))


;;;; jinx

(elpaca jinx
  (push (lambda () (global-jinx-mode 1))
        my-to-incremental-load)

  (with-eval-after-load 'jinx
    ;; (with-eval-after-load 'diminish
    ;;   (diminish 'jinx-mode " $"))

    (with-eval-after-load 'nerd-icons
      (setf (alist-get 'jinx-mode minor-mode-alist)
            (list (concat (nerd-icons-codicon "nf-cod-blank")
                          (nerd-icons-mdicon "nf-md-spellcheck")))))

    (define-keymap
      :keymap (conn-get-mode-map 'conn-command-state 'jinx-mode)
      "<remap> <ispell-word>" 'jinx-correct-nearest
      "$" 'jinx-correct-nearest
      "b $" 'jinx-correct-all)

    (defun my-jinx-dispatch-check (window pt _thing)
      (interactive)
      (with-selected-window window
        (save-excursion
          (goto-char pt)
          (jinx-correct-nearest))))

    (defun my--conn-dispatch-jinx (&optional in-windows)
      (cl-loop for win in (conn--preview-get-windows in-windows)
               nconc (with-selected-window win
                       (cl-loop for ov in (jinx--get-overlays (window-start) (window-end))
                                collect (conn--make-preview-overlay
                                         (overlay-start ov)
                                         (- (overlay-end ov) (overlay-start ov)))))))

    (conn-register-thing-commands
     'word nil
     'jinx-correct-nearest
     'jinx-correct
     'jinx-correct-all)

    (let ((fn (apply-partially 'conn--dispatch-all-things 'word t)))
      (dolist (cmd '(jinx-correct-nearest
                     jinx-correct
                     jinx-correct-all))
        (setf (alist-get cmd conn-dispatch-default-action-alist)
              'my-jinx-dispatch-check)))))


;;;; ef-themes

(elpaca ef-themes)


;;;; pgmacs

;; (elpaca (pgmacs :host github
;;                 :repo "emarsden/pgmacs"))


;;;; eat

;; (elpaca eat)


;;;; beancount

;; (elpaca beancount)


;;;; projectile

(elpaca projectile
  (setq projectile-mode-line-prefix ""
        projectile-dynamic-mode-line nil)
  (push (lambda () (projectile-mode 1))
        my-to-incremental-load)

  (defun my-ibuffer-maybe-project (&optional all)
    (interactive "P")
    (require 'projectile)
    (if (or all (not (cdr (project-current))))
        (ibuffer)
      (projectile-ibuffer nil)))

  (with-eval-after-load 'projectile
    (keymap-global-unset "C-x p")
    (keymap-global-set "C-x p" 'projectile-command-map)
    (keymap-global-set "C-c c" 'projectile-command-map)

    (define-keymap
      :keymap projectile-command-map
      "e" 'projectile-run-eshell
      "d" 'projectile-dired
      "D" 'projectile-find-dir
      "j" 'projectile-run-gdb)))

;;;; smart parens

(elpaca (smartparens :host github
                     :repo "Fuco1/smartparens")
  (with-eval-after-load 'smartparens
    (with-eval-after-load 'diminish
      (diminish 'smartparens-mode)))

  (letrec ((loader (lambda ()
                     (require 'smartparens-config)
                     (smartparens-global-mode 1)
                     ;; (show-smartparens-global-mode 1)
                     (remove-hook 'prog-mode-hook loader))))
    (add-hook 'prog-mode-hook loader))

  (with-eval-after-load 'smartparens
    (setq sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay t
          sp-echo-match-when-invisible nil)

    (add-hook 'lisp-data-mode-hook 'smartparens-strict-mode)

    ;; (defun conn-progressive-read (prompt collection)
    ;;   (let ((so-far "")
    ;;         (narrowed collection)
    ;;         (prompt (propertize (concat prompt ": ")
    ;;                             'face 'minibuffer-prompt))
    ;;         (display "")
    ;;         (next nil)
    ;;         (next-char nil))
    ;;     (while (not (length= narrowed 1))
    ;;       (while (not next)
    ;;         (setq display (cl-loop with display = ""
    ;;                                for i from 0
    ;;                                for item in narrowed
    ;;                                while (length< display 100)
    ;;                                do (setq display
    ;;                                         (concat display item
    ;;                                                 (propertize " | " 'face 'shadow)))
    ;;                                finally return
    ;;                                (concat (propertize "{" 'face 'minibuffer-prompt)
    ;;                                        (substring display 0 -3)
    ;;                                        (when (length> narrowed i)
    ;;                                          (propertize "..." 'face 'minibuffer-prompt))
    ;;                                        (propertize "}" 'face 'minibuffer-prompt))))
    ;;         (setq next-char (read-char (concat prompt so-far "  " display) t))
    ;;         (setq next (cl-loop for item in narrowed
    ;;                             when (eql (aref item (length so-far))
    ;;                                       next-char)
    ;;                             collect item)))
    ;;       (setq narrowed next
    ;;             next nil)
    ;;       (setq so-far (concat so-far (string next-char))))
    ;;     (car narrowed)))

    (with-eval-after-load 'conn
      (defun conn-progressive-read (prompt collection)
        (let ((so-far "")
              (narrowed (mapcar #'copy-sequence
                                (vertico-sort-length-alpha
                                 (delete-dups (copy-sequence collection)))))
              (prompt (propertize (concat prompt ": ")
                                  'face 'minibuffer-prompt))
              (display "")
              (next nil)
              (next-char nil))
          (unwind-protect
              (while (not (length= narrowed 1))
                (while (not next)
                  (setq display (cl-loop with display = ""
                                         for i from 0 below 10
                                         for item in narrowed
                                         do (setq display (concat display item "\n"))
                                         finally return display))
                  (posframe-show " *conn pair posframe*"
                                 :string display
                                 :left-fringe 0
                                 :right-fringe 0
                                 :background-color (face-attribute 'menu :background)
                                 :border-width conn-posframe-border-width
                                 :border-color conn-posframe-border-color
                                 :min-width 8)
                  (setq next-char (read-char (concat prompt so-far) t))
                  (setq next (cl-loop for item in narrowed
                                      when (eql (aref item (length so-far))
                                                next-char)
                                      do (add-text-properties
                                          0 (1+ (length so-far))
                                          '(face completions-highlight)
                                          item)
                                      and collect item)))
                (setq narrowed next
                      next nil)
                (setq so-far (concat so-far (string next-char))))
            (posframe-hide " *conn pair posframe*"))
          (car narrowed)))

      (defun conn-sp-wrap-region ()
        (interactive)
        (activate-mark)
        (unwind-protect
            (save-excursion
              (sp-wrap-with-pair
               (conn-progressive-read
                "Pair"
                (mapcar
                 (pcase-lambda ((map :trigger :open))
                   (or trigger open))
                 (append
                  (alist-get t sp-pairs)
                  (alist-get major-mode sp-pairs
                             nil nil
                             (lambda (a b)
                               (or (eq t a)
                                   (provided-mode-derived-p b a)))))))))
          (deactivate-mark)))

      (define-keymap
        :keymap (conn-get-mode-map 'conn-command-state 'smartparens-mode)
        "M-s" 'sp-splice-sexp
        "M-r" 'sp-splice-sexp-killing-around
        "<left>" 'sp-backward-symbol
        "<right>" 'sp-forward-symbol))

    (define-keymap
      :keymap smartparens-mode-map
      "C-M-f" `(menu-item
                "forward-sexp"
                sp-forward-sexp
                :filter ,(lambda (&rest _)
                           (if (bound-and-true-p treesit-primary-parser)
                               'forward-sexp
                             'sp-forward-sexp)))
      "C-M-b" `(menu-item
                "forward-sexp"
                sp-backward-sexp
                :filter ,(lambda (&rest _)
                           (if (bound-and-true-p treesit-primary-parser)
                               'backward-sexp
                             'sp-backward-sexp)))
      "C-M-u" 'sp-backward-up-sexp
      "C-M-d" 'sp-down-sexp
      "C-M-p" 'sp-backward-down-sexp
      "C-M-n" 'sp-up-sexp
      "M-C" 'sp-copy-sexp
      "M-(" 'sp-splice-sexp-killing-backward ;; depth-changing commands
      "M-)" 'sp-splice-sexp-killing-forward
      "M-K" 'sp-raise-sexp
      "M-I" 'sp-splice-sexp
      "M-J" 'sp-backward-slurp-sexp
      "M-L" 'sp-forward-slurp-sexp
      "M-O" 'sp-forward-barf-sexp
      "M-U" 'sp-backward-barf-sexp
      "M-B" 'sp-convolute-sexp
      "M-H" 'sp-join-sexp
      "M-N" 'sp-beginning-of-sexp
      "M-M" 'sp-end-of-sexp)

    (define-keymap
      :keymap smartparens-mode-map
      "<conn-thing-map> n" 'sp-beginning-of-sexp
      "<conn-thing-map> m" 'sp-end-of-sexp)))


;;;; djvu

(elpaca djvu)


;;;; hide mode line

(elpaca hide-mode-line)


;;;; aggressive indent mode

(elpaca aggressive-indent
  (with-eval-after-load 'aggressive-indent
    (with-eval-after-load 'nerd-icons
      (setf (alist-get 'aggressive-indent-mode minor-mode-alist)
            (list (concat (nerd-icons-codicon "nf-cod-blank")
                          (nerd-icons-mdicon "nf-md-keyboard_tab"))))))
  (add-hook 'lisp-data-mode-hook 'aggressive-indent-mode))

;;;; org-modern

(elpaca org-modern
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…")

  (setq org-modern-block-indent t  ; to enable org-modern-indent when org-indent is active
        org-modern-hide-stars nil
        org-modern-todo-faces
        '(("STARTED" :foreground "yellow")
          ("CANCELED" org-special-keyword :inverse-video t :weight bold))
        org-modern-list
        '((?* . "•")
          (?+ . "‣"))
        org-modern-fold-stars
        '(("▶" . "▼")
          ("▷" . "▽")
          ("▸" . "▾")
          ("▹" . "▿"))
        org-modern-checkbox
        '((?X . "✔")
          (?- . "┅")
          (?\s . " "))
        org-modern-label-border 1)

  (with-eval-after-load 'org
    (global-org-modern-mode 1)))

;;;; org-ql

(elpaca org-ql
  (defun my-org-ql-search-denote-files ()
    (interactive)
    (require 'denote)
    (require 'org)
    (require 'org-ql)
    (let ((org-directory denote-directory))
      (org-ql-search
        (org-ql-search-directories-files)
        (read-string "Query: " (when org-ql-view-query
                                 (format "%S" org-ql-view-query)))
        :narrow (or org-ql-view-narrow (equal current-prefix-arg '(4)))
        :super-groups (org-ql-view--complete-super-groups)
        :sort (org-ql-view--complete-sort))))
  (keymap-global-set "C-c n q" 'my-org-ql-search-denote-files)

  (with-eval-after-load 'embark
    (defun my-embark-org-ql-files (files)
      (require 'denote)
      (require 'org)
      (require 'org-ql)
      (let ((org-directory files))
        (org-ql-search
          files
          (read-string "Query: " (when org-ql-view-query
                                   (format "%S" org-ql-view-query)))
          :narrow (or org-ql-view-narrow (equal current-prefix-arg '(4)))
          :super-groups (org-ql-view--complete-super-groups)
          :sort (org-ql-view--complete-sort))))
    (cl-pushnew 'my-embark-org-ql-files embark-multitarget-actions)
    (keymap-set embark-file-map "q" 'my-embark-org-ql-files))

  (with-eval-after-load 'org
    (require 'org-ql)))


;;;; org-super-agenda

;; (elpaca (org-super-agenda :host github :repo "alphapapa/org-super-agenda"))


;;;; yasnippet

(elpaca yasnippet
  (push (lambda () (yas-global-mode 1))
        my-to-incremental-load)

  (with-eval-after-load 'yasnippet
    ;; Can't do this through diminish since it wants to append a
    ;; space.
    (with-eval-after-load 'nerd-icons
      (setf (alist-get 'yas-minor-mode minor-mode-alist)
            (list (concat (nerd-icons-codicon "nf-cod-blank")
                          (nerd-icons-codicon "nf-cod-symbol_snippet")))))

    (setq yas-wrap-around-region t
          yas-key-syntaxes '(yas-try-key-from-whitespace "w_.()" "w_." "w_"))

    (define-keymap
      :keymap yas-minor-mode-map
      "C-c y" 'yas-new-snippet
      "C-c Y" 'yas-visit-snippet-file)

    (define-keymap
      :keymap yas-keymap
      "TAB" nil
      "M-n" 'yas-next-field
      "M-p" 'yas-prev-field)))

(elpaca consult-yasnippet
  (with-eval-after-load 'consult-yasnippet
    (consult-customize consult-yasnippet :preview-key nil))

  (with-eval-after-load 'yasnippet
    (define-keymap
      :keymap yas-minor-mode-map
      "M-P" 'consult-yasnippet)))

;; (elpaca yasnippet-snippets)


;;;; spacious padding

(elpaca spacious-padding
  (setq spacious-padding-widths '( :internal-border-width 16
                                   :header-line-width 4
                                   :mode-line-width 1
                                   :tab-width 4
                                   :right-divider-width 20
                                   :scroll-bar-width 8
                                   :fringe-width 10))
  (spacious-padding-mode 1))


;;;; rg

(elpaca rg
  (define-keymap
    :keymap search-map
    "y" 'rg-menu
    "u" 'rg))


;;;; treemacs

;; (elpaca treemacs
;;   (keymap-global-set "C-c h" 'treemacs-select-window)
;;   (keymap-global-set "C-c H" 'treemacs))

;;;; dirvish

(elpaca dirvish
  (custom-set-faces
   '(dirvish-hl-line ((t :inherit region :extend t)))
   '(dirvish-hl-line-inactive ((t :inherit region :extend t))))

  (setq dirvish-hide-cursor t)

  (with-eval-after-load 'dired
    (dirvish-override-dired-mode 1))

  (advice-add 'dirvish--maybe-toggle-cursor :override 'ignore))

;;;; goto-chg

(elpaca goto-chg
  (defvar-keymap goto-chg-repeat-map
    :repeat t
    "/" 'goto-last-change
    "?" 'goto-last-change-reverse)
  (keymap-global-set "M-g /" 'goto-last-change)
  (keymap-global-set "M-g ?" 'goto-last-change-reverse))

;;;; repeat-fu

;; (elpaca (repeat-fu :host codeberg
;;                    :repo "ideasman42/emacs-repeat-fu"))

;; Local Variables:
;; outline-regexp: ";;;;* [^    \n]"
;; End:
