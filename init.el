;;; -*- lexical-binding: t; eval: (outline-minor-mode 1); -*-

;;; Elpaca

(defvar elpaca-installer-version 0.9)
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
    (when (< emacs-major-version 28) (require 'subr-x))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;; Built-in

;;;; emacs

;; help-window-select t
(setq help-enable-symbol-autoload t
      mac-option-modifier 'meta
      mac-command-modifier 'super
      hi-lock-auto-select-face t
      mark-even-if-inactive t
      recenter-positions '(top middle bottom)
      even-window-sizes nil
      scroll-preserve-screen-position t
      delete-active-region t
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
      show-paren-context-when-offscreen 'overlay
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
      mark-ring-max 32
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
      bidi-inhibit-bpa t
      cycle-spacing-actions '(just-one-space
                              delete-all-space
                              delete-space-after
                              delete-space-before
                              restore)
      project-vc-extra-root-markers '(".projectile" ".project"))

(defun my-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" 'my-kill-buffer)

(setopt mouse-wheel-scroll-amount '(0.33
                                    ((shift) . hscroll)
                                    ((meta))
                                    ((control meta) . global-text-scale)
                                    ((control) . text-scale)))

(setq-default indent-tabs-mode nil)

(minibuffer-depth-indicate-mode 1)
(global-goto-address-mode 1)
;; (show-paren-mode 1)
(delete-selection-mode -1)
(column-number-mode 1)
(line-number-mode 1)
(undelete-frame-mode 1)
(context-menu-mode 1)
(save-place-mode 1)

(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-z")
(keymap-global-unset "C-x C-z")

(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "C-M-<return>"#'default-indent-new-line)
(keymap-global-set "S-<backspace>" #'cycle-spacing)
(keymap-global-set "M-N" #'tab-bar-switch-to-next-tab)
(keymap-global-set "M-P" #'tab-bar-switch-to-prev-tab)
(keymap-global-set "C-:" #'read-only-mode)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "M-;" #'comment-line)
(keymap-global-set "C-c c" #'compile)
(keymap-global-set "C-S-w" #'delete-region)
(keymap-global-set "C-S-o" #'other-window)
(keymap-global-set "<f2>" #'other-window)
(keymap-global-set "C-z" #'transient-resume)
(keymap-global-set "C-h A" #'describe-char)
(keymap-global-set "C-x j" #'dired-jump)
(keymap-global-set "C-/" #'undo-only)
(keymap-global-set "C-x <" #'scroll-right)
(keymap-global-set "C-x >" #'scroll-left)

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
  (when-let ((win (other-window-for-scrolling)))
    (select-window win)))
(keymap-global-set "C-M-S-o" #'my-select-other-window-for-scrolling)

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

(require 'c-ts-mode)
(setq c-ts-mode-indent-offset 4)


;;;; org

(elpaca (org :repo ("https://code.tecosaur.net/tec/org-mode.git/" . "org")
             :branch "dev"
             :pre-build (progn (require 'elpaca-menu-org) (elpaca-menu-org--build))
             :autoloads "org-loaddefs.el"
             :build (:not elpaca--generate-autoloads-async)
             :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")))
  (setq org-agenda-start-on-weekday nil
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
  (add-hook 'org-mode-hook 'abbrev-mode)

  (with-eval-after-load 'org
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

(elpaca (org-luhmann :host github :repo "yibie/org-luhmann")
  (with-eval-after-load 'org
    (org-luhmann-setup)))

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
      hippie-expand-try-functions-list '(try-expand-list
                                         try-expand-line))

(add-hook 'prog-mode-hook (lambda () (abbrev-mode 1)))

;; (keymap-unset completion-preview-active-mode-map "TAB")
;; (keymap-unset completion-preview-active-mode-map "M-i")

(keymap-global-set "M-n" 'hippie-expand)

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

  (setopt outline-minor-mode-prefix (kbd "C-c u"))

  (pcase-dolist (`(,_ . ,def) (cdr outline-navigation-repeat-map))
    (put def 'repeat-map nil)))


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

;; (require 'dictionary)
;; (setopt dictionary-server "localhost")
;; (keymap-global-set "C-c t d" #'dictionary-lookup-definition)


;;;; isearch

(setq isearch-lazy-count t
      isearch-allow-motion t)

(keymap-set isearch-mode-map "M-DEL" 'isearch-delete-char)
(keymap-set isearch-mode-map "M-DEL" 'isearch-del-char)
(keymap-set isearch-mode-map "C-z"   'transient-resume)

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
                                 (isearch-escapable-split-on-char string "."))
                         ".+?"))
                      (isearch-escapable-split-on-char string "&"))
              "\\(?:\\s_\\|\\w\\)*"))
           (isearch-escapable-split-on-char string " "))
   search-whitespace-regexp))

(isearch-define-mode-toggle wildcards "*" isearch-wildcards-compile "\
Turning on wildcards turns off regexp mode.")
(put 'isearch-wildcards-compile 'isearch-message-prefix
     (propertize "Wildcard " 'face 'minibuffer-prompt))

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
(keymap-global-set "<remap> <isearch-forward>" 'isearch-forward-wildcard)

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
(keymap-global-set "<remap> <isearch-backward>" 'isearch-backward-wildcard)

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

(setopt tab-bar-show nil
        tab-bar-tab-name-function 'tab-bar-tab-name-all)

(setq tab-bar-new-tab-choice t)

(tab-bar-mode 1)
(tab-bar-history-mode 1)

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

  (setq savehist-file (expand-file-name "var/savehist/hist" user-emacs-directory))

  (savehist-mode 1))


;;;; repeat

(setq repeat-exit-timeout nil
      repeat-on-final-keystroke t)

(setopt repeat-keep-prefix nil)

(repeat-mode 1)

(keymap-global-set "C-x c" 'repeat)


;;;; autorevert

(setopt auto-revert-interval .01)
(global-auto-revert-mode 1)
(with-eval-after-load 'diminish
  (diminish 'auto-revert-mode))


;;;; recentf

(with-eval-after-load 'no-littering
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15)

  (recentf-mode 1)

  (defvar my-recentf-autosave
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
    (diminish 'outline-minor-mode " *")))


;;;; ibuffer

(with-eval-after-load 'ibuffer)


;;;; dired

(with-eval-after-load 'dired
  (setq dired-omit-files (rx (or (seq string-start (1+ ".") (1+ (not ".")))
                                 (seq string-start (1+ "#"))))
        dired-dwim-target 'dired-dwim-target-recent)

  (define-keymap
    :keymap dired-mode-map
    "/" 'dired-undo
    "C-<tab>" 'dired-maybe-insert-subdir
    "<backtab>" 'dired-kill-subdir
    "<remap> <dired-do-find-regexp-and-replace>" 'dired-do-replace-regexp-as-diff))


;;;; eldoc

(with-eval-after-load 'diminish
  (diminish 'eldoc-mode))

(setq eldoc-echo-area-prefer-doc-buffer t)


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
        transient-mode-line-format nil)

  (with-eval-after-load 'ibuffer
    (require 'transient)

    (transient-define-prefix my-ibuffer-filter-prefix ()
      "Ibuffer filter prefix"
      [["Filters"
        ("s" "Save" ibuffer-save-filters :transient t)
        ("x" "Delete Saved" ibuffer-delete-saved-filters :transient t)
        ("/" "Disable" ibuffer-filter-disable :transient t)
        ("r" "Switch To" ibuffer-switch-to-saved-filters :transient t)
        ("p" "Pop" ibuffer-pop-filter :transient t)]
       ["Ops"
        ("!" "Negate" ibuffer-negate-filter :transient t)
        ("&" "And" ibuffer-and-filter :transient t)
        ("|" "Or" ibuffer-or-filter :transient t)
        ("D" "Decompose" ibuffer-decompose-filter :transient t)
        ("t" "Exchange" ibuffer-exchange-filters :transient t)]
       ["Groups"
        ("S" "Save" ibuffer-delete-saved-filter-groups :transient t)
        ("X" "Delete Saved" ibuffer-delete-saved-filter-groups :transient t)
        ("g" "Group" ibuffer-filters-to-filter-group :transient t)
        ("P" "Pop" ibuffer-pop-filter-group :transient t)
        ("R" "Switch To" ibuffer-switch-to-saved-filter-groups :transient t)]]
      ["Filter By"
       [("i" "Modified" ibuffer-filter-by-modified :transient t)
        ("m" "Mode" ibuffer-filter-by-mode :transient t)
        ("M" "Derived Mode" ibuffer-filter-by-derived-mode :transient t)
        ("." "Extension" ibuffer-filter-by-file-extension :transient t)
        ("*" "Starred Name" ibuffer-filter-by-starred-name :transient t)]
       [("c" "Content" ibuffer-filter-by-content :transient t)
        ("f" "Filename" ibuffer-filter-by-filename :transient t)
        ("F" "Directory" ibuffer-filter-by-directory :transient t)
        ("n" "Name" ibuffer-filter-by-name :transient t)
        ("v" "Visiting" ibuffer-filter-by-visiting-file :transient t)]
       [("<" "Size" ibuffer-filter-by-size-lt :transient t)
        (">" "Size" ibuffer-filter-by-size-gt :transient t)
        ("e" "Predicate" ibuffer-filter-by-predicate :transient t)
        ("b" "Basename" ibuffer-filter-by-basename :transient t)
        ("E" "Process" ibuffer-filter-by-process :transient t)]])

    (keymap-set ibuffer-mode-map "/" 'my-ibuffer-filter-prefix)))

;;;; treesit-auto

(elpaca treesit-auto
  (with-eval-after-load 'treesit
    (require 'treesit-auto)
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode 1)))


;;;; Compat

(elpaca (compat :repo "emacs-compat/compat"
                :host github))


;;;; Diminish

(elpaca diminish
  (diminish 'visual-line-mode))


;;;; benchmark-init

;; (elpaca benchmark-init
;;   (require 'benchmark-init)
;;   (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

;; (profiler-start 'cpu+mem)
;; (add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))


;;;; expreg

(elpaca (expreg :host github :repo "casouri/expreg"))


;;;; helpful

(elpaca helpful
  (keymap-global-set "C-h v" 'helpful-variable)
  (keymap-global-set "C-h k" 'helpful-key)
  (keymap-global-set "C-h ," 'display-local-help)
  (keymap-global-set "C-h ." 'helpful-at-point)
  (keymap-global-set "<remap> <describe-function>" 'helpful-callable)
  (keymap-global-set "<remap> <describe-variable>" 'helpful-variable)

  (push '(help-mode . helpful-mode) major-mode-remap-alist)

  (with-eval-after-load 'helpful
    (fset 'helpful--source #'ignore))

  (with-eval-after-load 'embark
    (keymap-set embark-symbol-map "M-RET" 'helpful-symbol)))


;;;; sly

(elpaca sly
  (with-eval-after-load 'sly
    (setq sly-default-lisp 'sbcl
          sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "4096"))))

    (defun sly-setup-embark ()
      (require 'embark)
      (require 'conn-embark)
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

(elpaca lsp-mode
  (add-hook 'lsp-mode-hook 'lsp-ui-peek-mode)

  (setq lsp-keymap-prefix "C-c s"
        lsp-eldoc-render-all nil
        lsp-enable-on-type-formatting nil
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-header t
        lsp-ui-doc-border "black"
        lsp-ui-doc-background '((t (:background "#dfd9cf")))
        lsp-inlay-hint-face '((t (:inherit shadow :height 0.8))))

  (setq lsp-clients-clangd-args '("-j=4"
                                  "--log=error"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--cross-file-rename"
                                  "--header-insertion=never")
        lsp-zig-zls-executable "~/build/zls/zig-out/bin/zls")

  (defun my-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (add-hook 'lsp-completion-mode-hook #'my-lsp-mode-setup-completion)

  (with-eval-after-load 'lsp-mode
    (define-keymap
      :keymap lsp-mode-map
      "C-c I" 'lsp-inlay-hints-mode)))

;;;;; lsp-ui

(elpaca lsp-ui
  (with-eval-after-load 'lsp
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (keymap-set lsp-mode-map "M-g m" 'lsp-ui-imenu)))


;;;; dap-mode

(elpaca dap-mode)


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
  (with-eval-after-load 'org
    (require 'pdf-tools))

  (with-eval-after-load 'pdf-tools
    (keymap-set pdf-view-mode-map "s a" #'pdf-view-auto-slice-minor-mode))

  (setopt pdf-info-epdfinfo-program "~/.emacs.d/elpaca/builds/pdf-tools/server/epdfinfo")
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
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'latex-mode-hook 'turn-on-cdlatex))


;;;; cdlatex

(elpaca cdlatex
  (add-hook 'latex-mode-hook #'cdlatex-mode)
  (add-hook 'org-mode-hook #'org-cdlatex-mode)

  (with-eval-after-load 'cdlatex
    (setq
     ;; cdlatex-math-symbol-alist nil
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
    (keymap-set org-mode-map "M-i" 'org-cdlatex-environment-indent)

    (defun my-special-edit-ad (&rest _)
      (when (eq major-mode 'org-mode)
        (org-edit-latex-environment)))
    (advice-add 'cdlatex-environment :after 'my-special-edit-ad))

  (with-eval-after-load 'conn
    (define-keymap
      :keymap (conn-get-mode-map 'conn-state 'org-mode)
      "'" 'org-cdlatex-math-modify)))


;;;; math-delimiters

(elpaca (math-delimiters :host github :repo "oantolin/math-delimiters")
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-SPC" 'math-delimiters-insert)))


;;;; dtrt-indent

(elpaca dtrt-indent
  (dtrt-indent-global-mode 1)
  (with-eval-after-load 'diminish
    (diminish 'dtrt-indent-mode)))


;;;; exec-path-from-shell

(elpaca exec-path-from-shell
  (when (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))


;;;; modus-themes

(when (< emacs-major-version 30)
  (elpaca modus-themes
    (require 'modus-themes)

    (setopt modus-themes-common-palette-overrides
            (seq-concatenate
             'list
             `((bg-main "#f7eee1")
               (cursor "#7d0002")
               (bg-region "#f1d5d0")
               (fg-active-argument "#930c93")
               (bg-active-argument "#f4caf4")
               (fg-region unspecified)
               (fg-completion-match-0 "#353b44")
               (bg-completion-match-0 "#c5dfff")
               (fg-completion-match-1 "#384231")
               (bg-completion-match-1 "#cdf3b5")
               (fg-completion-match-2 "#3b3544")
               (bg-completion-match-2 "#d1baf1")
               (fg-completion-match-3 "#313c37")
               (bg-completion-match-3 "#bef1da")
               (bg-search-lazy bg-magenta-subtle)
               (bg-search-current bg-yellow-intense))
             modus-themes-preset-overrides-warmer))

    (load-theme 'modus-operandi-tinted t)

    (custom-set-faces
     `(transient-key-exit ((t :inherit modus-themes-key-binding :foreground "#a60000")))
     `(transient-argument ((t :inherit font-lock-string-face :weight bold
                              :foreground "#930c93" :background "#f4caf4")))
     `(transient-key-return ((t :inherit modus-themes-key-binding :foreground "#6f5500")))
     `(transient-key-stay ((t :inherit modus-themes-key-binding :foreground "#008900"))))))

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
  (keymap-global-set "S-<return>"    'crux-smart-open-line)
  (keymap-global-set "C-x F"         'crux-sudo-edit)
  (keymap-global-set "C-x W"         'crux-open-with)
  (keymap-global-set "C-<backspace>" 'crux-kill-whole-line)
  (define-key global-map [remap kill-whole-line] 'crux-kill-whole-line)
  (define-key global-map [remap kill-line] 'crux-smart-kill-line)
  (define-key global-map [remap open-line] 'crux-smart-open-line)
  (keymap-global-set "<remap> <whitespace-cleanup>" 'crux-cleanup-buffer-or-region)
  (keymap-global-set "C-S-k" 'crux-kill-line-backwards)
  (keymap-global-set "C-c S" 'crux-visit-shell-buffer)

  (with-eval-after-load 'conn
    (keymap-set conn-state-map "S" 'crux-visit-shell-buffer)
    (keymap-set ctl-x-x-map "b" 'crux-rename-file-and-buffer)

    (define-keymap
      :keymap conn-edit-map
      "D"   'crux-duplicate-and-comment-current-line-or-region
      "@"   'crux-insert-date)))


;;;; posframe

(elpaca spinner)

(when window-system (elpaca posframe))


;;;; isearch+

(elpaca (isearch+ :host github
                  :repo "emacsmirror/isearch-plus"
                  :main "isearch+.el")
  (run-with-timer 0.33 nil (lambda () (require 'isearch+)))

  (setq isearchp-dimming-color "#cddfcc"
        isearchp-lazy-dim-filter-failures-flag nil
        isearchp-restrict-to-region-flag nil
        isearchp-deactivate-region-flag nil
        isearchp-movement-unit-alist '((?w . forward-word)
                                       (?s . forward-sexp)
                                       (?i . forward-list)
                                       (?s . forward-sentence)
                                       (?c . forward-char)
                                       (?l . forward-line)))

  (setopt isearchp-initiate-edit-commands nil)

  (with-eval-after-load 'isearch+
    (defun my-supress-in-macro () executing-kbd-macro)
    (advice-add 'isearchp-highlight-lighter :before-until 'my-supress-in-macro)

    (keymap-unset isearch-mode-map "C-t")
    (keymap-set isearch-mode-map "C-;" 'isearchp-filter-map)
    (keymap-set isearch-mode-map "C-y m" 'isearchp-yank-sexp-symbol-or-char)
    (keymap-set isearch-mode-map "C-y o" 'isearchp-yank-word-or-char-forward)
    (keymap-set isearch-mode-map "C-y u" 'isearchp-yank-word-or-char-backward)
    (keymap-set isearch-mode-map "C-y i" 'isearchp-yank-line-backward)
    (keymap-set isearch-mode-map "C-y k" 'isearchp-yank-line-forward)
    (keymap-set isearch-mode-map "C-y l" 'isearchp-yank-char)
    (keymap-set isearch-mode-map "C-M-o" 'isearchp-open-recursive-edit)
    (keymap-set isearchp-filter-map "f" 'isearchp-add-filter-predicate)
    (keymap-set isearchp-filter-map "r" 'isearchp-add-regexp-filter-predicate)))


;;;; elixir-ts

(elpaca elixir-ts-mode)


;;;; conn

(elpaca (conn :host github
              :depth nil
              :repo "mtll/conn")
  (setq conn-wincontrol-initial-help nil
        conn-state-cursor-type 'box
        conn-emacs-state-cursor-type '(hbar . 5)
        conn-mark-idle-timer 0.05
        conn-read-string-timeout 0.35)

  (setq-default cursor-type '(hbar . 5))

  (defun conn-mark-emacs-state-hook ()
    (when (and conn-emacs-state
               (not (use-region-p)))
      (conn--push-ephemeral-mark (point))))

  (defun my-add-mode-abbrev (arg)
    (interactive "P")
    (add-mode-abbrev (or arg 0)))

  (add-hook 'conn-transition-hook 'conn-mark-emacs-state-hook)

  (add-hook 'view-mode-hook #'conn-emacs-state)

  (conn-mode 1)

  (add-to-list 'conn-buffer-default-state-alist
               (cons "^\\*Echo.*" 'conn-emacs-state))
  (add-to-list 'conn-buffer-default-state-alist
               (cons "COMMIT_EDITMSG.*" 'conn-emacs-state))
  (add-to-list 'conn-buffer-default-state-alist
               (cons "\\*Edit Macro\\*" 'conn-state))
  (add-to-list 'conn-buffer-default-state-alist
               (cons (lambda (buffer &rest _args) (bound-and-true-p org-capture-mode))
                     'conn-state))

  (cl-pushnew 'conn-emacs-state conn-ephemeral-mark-states)

  (keymap-global-set "C-o" 'conn-open-line-above)
  (keymap-global-set "M-o" 'conn-open-line)
  (keymap-global-set "C-j" 'conn-open-line-and-indent)
  (keymap-global-set "C-c v" 'conn-toggle-mark-command)
  (keymap-global-set "C-;" 'conn-wincontrol)
  (keymap-global-set "C-x ," 'subword-mode)
  (keymap-global-set "M-\\"  'conn-kapply-prefix)
  (keymap-global-set "C-M-y" 'conn-yank-lines-as-rectangle)
  (keymap-set conn-emacs-state-map "<f8>" 'conn-state)
  (keymap-set conn-org-edit-state-map "<f8>" 'conn-state)
  (keymap-set (conn-get-mode-map 'conn-state 'org-mode) "<f9>" 'conn-org-edit-state)
  (keymap-set (conn-get-mode-map 'conn-emacs-state 'org-mode) "<f9>" 'conn-org-edit-state)
  (keymap-global-set "S-<return>" 'conn-open-line-and-indent)
  (keymap-global-set "C-," 'embark-dwim)
  (keymap-global-set "C-<backspace>" 'kill-whole-line)
  (keymap-global-set "C-0" 'delete-window)
  (keymap-global-set "C-1" 'delete-other-windows)
  (keymap-global-set "C-2" 'split-window-below)
  (keymap-global-set "C-3" 'split-window-right)
  (keymap-global-set "C-." 'conn-dispatch-on-things)
  (keymap-global-set "C-6" 'conn-swap-buffers)
  (keymap-global-set "C-7" 'conn-swap-windows)
  (keymap-global-set "C-9" 'tab-close)
  (keymap-set conn-state-map "B" 'ibuffer)
  (keymap-set conn-state-map "M-;" 'conn-wincontrol-one-command)
  (keymap-set conn-emacs-state-map "C-M-;" 'conn-wincontrol-one-command)
  (keymap-set conn-state-map "C-M-;" 'conn-wincontrol-one-command)
  (keymap-global-set "M-`" 'conn-wincontrol-quit-other-window-for-scrolling)
  (keymap-set conn-state-map "*" 'calc-dispatch)
  (keymap-set conn-state-map "$" 'ispell-word)
  (keymap-set conn-state-map "!" 'my-add-mode-abbrev)
  (keymap-set conn-state-map "@" 'inverse-add-mode-abbrev)
  (keymap-global-set "M-u" 'conn-region-case-prefix)
  (keymap-global-set "M-SPC" 'conn-toggle-mark-command)
  (keymap-global-set "C-SPC" 'conn-set-mark-command)
  (keymap-global-set "M-z" 'conn-exchange-mark-command)
  (keymap-global-set "C-t" 'conn-transpose-regions)
  (keymap-global-set "M-U" 'conn-wincontrol-maximize-vertically)
  (keymap-set conn-state-map "C-'" 'conn-dispatch-on-things)

  (dolist (state '(conn-state conn-emacs-state))
    (keymap-set (conn-get-mode-map state 'conn-kmacro-applying-p)
                "<escape>" 'exit-recursive-edit))

  (defun my-space-after-point (N)
    (interactive "p")
    (save-excursion
      (let ((last-command-event ?\ ))
        (self-insert-command N))))
  (keymap-global-set "S-SPC" 'my-space-after-point)

  (keymap-set conn-state-map "M-<tab>" 'indent-region))

;;;;; conn extensions

(elpaca (conn-consult :host github
                      :repo "mtll/conn"
                      :files ("extensions/conn-consult.el"))
  (with-eval-after-load 'consult
    (require 'conn-consult))
  (with-eval-after-load 'conn
    (keymap-set conn-region-map "o" 'conn-consult-line-region)
    (keymap-set conn-region-map "g" 'conn-consult-ripgrep-region)
    (keymap-set conn-region-map "h" 'conn-consult-region-search-map)
    (keymap-global-set "M-s T" 'conn-consult-thing)))

(elpaca (conn-embark :host github
                     :repo "mtll/conn"
                     :files ("extensions/conn-embark.el"))
  (keymap-set conn-state-map "," 'embark-act)
  (keymap-set conn-state-map "TAB" 'conn-embark-dwim-either)
  (keymap-set conn-state-map "<tab>" 'conn-embark-dwim-either)
  (keymap-set conn-org-edit-state-map "TAB" 'conn-embark-dwim-either)
  (keymap-global-set "C-M-S-<iso-lefttab>" 'conn-embark-conn-bindings)

  (defun conn-embark-dwim-either (&optional arg)
    (interactive "P")
    (require 'embark)
    (if arg (conn-embark-alt-dwim) (embark-dwim)))

  (with-eval-after-load 'embark
    (require 'conn-embark)

    (defcustom conn-embark-alt-default-action-overrides
      '((defun . comment-defun)
        (identifier . xref-find-references))
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
      (if-let ((targets (embark--targets)))
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

    (keymap-set embark-heading-map "N" 'conn-narrow-indirect-to-heading)

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

    (keymap-set conn-emacs-state-map "C-TAB" 'embark-act)

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

(elpaca (conn-isearch+ :host github
                       :repo "mtll/conn"
                       :files ("extensions/conn-isearch+.el"))
  (with-eval-after-load 'isearch+
    (require 'conn-isearch+)))

(elpaca (conn-calc :host github
                   :repo "mtll/conn"
                   :files ("extensions/conn-calc.el"))
  (with-eval-after-load 'calc
    (require 'conn-calc)))

(when (>= emacs-major-version 30)
  (elpaca (conn-treesit :host github
                        :repo "mtll/conn"
                        :files ("extensions/conn-treesit.el"))
    (with-eval-after-load 'treesit
      (require 'conn-treesit))))


;;;; orderless set operations

(elpaca (orderless-set-operations :host github
                                  :repo "mtll/orderless-set-operations")
  (with-eval-after-load 'orderless
    (oso-mode 1)))


;;;; expand region

(elpaca expand-region)


;;;; ialign

(elpaca ialign
  (with-eval-after-load 'conn
    (keymap-set conn-region-map "a i" 'ialign))

  (with-eval-after-load 'embark
    (defun embark-ialign (_reg)
      (ialign (region-beginning) (region-end)))

    (keymap-set embark-region-map "a" 'embark-ialign)))


;;;; magit

(elpaca (magit :host github :repo "magit/magit" :files (:defaults "git-commit.el"))
  (keymap-global-set "C-c m f" 'magit-file-dispatch)
  (keymap-global-set "C-c m s" 'magit-status)
  (keymap-global-set "C-c m d" 'magit-dispatch))


;;;; flycheck

(elpaca flycheck
  (setq lsp-diagnostics-flycheck-default-level 'warning))


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
              (keymap-set text-mode-map "C-M-i" 'completion-at-point)
              (add-to-list 'completion-at-point-functions
                           (cape-capf-properties
                            #'cape-dict
                            :company-doc-buffer #'dictionary-doc-lookup)))))


;;;; embark

(elpaca embark
  (with-eval-after-load 'consult
    (require 'embark))

  (with-eval-after-load 'embark
    (defun my-embark-abbrev-target-finder ()
      (pcase-let ((`(,sym ,name ,wordstart ,wordend) (abbrev--before-point)))
        (when sym `(abbrev ,name ,wordstart . ,wordend))))
    (add-to-list 'embark-target-finders 'my-embark-abbrev-target-finder))

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
  (keymap-global-set "C-TAB" 'embark-act)
  (keymap-global-set "C-<tab>" 'embark-act)
  (keymap-global-set "M-S-<iso-lefttab>" 'embark-bindings)
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
    (cl-pushnew 'my-embark-tab-map (alist-get 'tab embark-keymap-alist))
    (setf (alist-get 'page embark-keymap-alist) (list 'embark-page-map))

    (keymap-set embark-region-map "RET" 'copy-region-as-kill)
    (keymap-set embark-defun-map "n" 'narrow-to-defun)
    (keymap-set embark-symbol-map "h" 'helpful-symbol)
    (keymap-set embark-collect-mode-map "C-j" 'consult-preview-at-point)
    (keymap-set embark-defun-map "M-RET" 'comment-or-uncomment-region)
    (keymap-set embark-identifier-map "M-RET" 'xref-find-references)
    (keymap-set embark-heading-map "RET" #'outline-cycle)
    (keymap-set embark-heading-map "M-RET" #'outline-up-heading)
    (keymap-set embark-symbol-map "RET" #'xref-find-definitions)

    (keymap-set embark-file-map "O" 'find-file-other-frame)
    (keymap-set embark-buffer-map "O" 'display-buffer-other-frame)

    (keymap-set embark-heading-map "RET" #'bicycle-cycle)
    (with-eval-after-load 'org
      (keymap-set embark-org-heading-map "RET" #'bicycle-cycle))

    (defun my-embark-abbrev-target-finder ()
      (pcase-let ((`(,sym ,name ,wordstart ,wordend) (abbrev--before-point)))
        (when sym `(abbrev ,name ,wordstart . ,wordend))))
    (add-to-list 'embark-target-finders 'my-embark-abbrev-target-finder)

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
      "h G" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h g" 'consult-ripgrep)

    (define-keymap
      :keymap embark-general-map
      "h l" 'consult-line
      "h f" 'consult-find
      "h G" 'consult-git-grep
      "h O" 'consult-locate
      "h i" 'consult-imenu
      "h I" 'consult-imenu-multi
      "h L" 'consult-line-multi
      "h g" 'consult-ripgrep)))

;;;;; embark buttons

(with-eval-after-load 'embark
  (defun my-embark-gh-issue-finder ()
    (when-let ((button (and (not (minibufferp))
                            (my-inside-regexp-in-line
                             "gh:\\([a-zA-Z-]*/[a-zA-Z-]*\\)#\\([0-9]*\\)"))))
      `(url
        ,(format "www.github.com/%s/issues/%s"
                 (match-string-no-properties 1)
                 (match-string-no-properties 2))
        ,(match-beginning 0) . ,(match-end 0))))
  (add-hook 'embark-target-finders 'my-embark-gh-issue-finder)

  (defun my-embark-gnu-bug-finder ()
    (when-let ((button (and (not (minibufferp))
                            (my-inside-regexp-in-line
                             "bug#\\([0-9]*\\)"))))
      `(url
        ,(format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                 (match-string-no-properties 1))
        ,(match-beginning 0) . ,(match-end 0))))
  (add-hook 'embark-target-finders 'my-embark-gnu-bug-finder)

  (defun my-embark-cve-target-finder ()
    (when-let ((button (and (not (minibufferp))
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
      (when-let ((_ (eq 'Git (vc-deduce-backend)))
                 (commit (or (magit-thing-at-point 'git-revision t)
                             (magit-branch-or-commit-at-point))))
        `(git-commit ,commit)))
    (add-hook 'embark-target-finders 'my-embark-commit-target-finder))

  (setf (alist-get 'git-commit embark-default-action-overrides)
        'magit-show-commit)

  (defvar my-button-target-functions nil)

  (defun my-embark-button-target ()
    (when (my-inside-regexp-in-line "<\\[\\([^:]+\\):\\(.*\\)\\]>")
      (when-let ((tar (run-hook-with-args-until-success
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

(elpaca company
  (run-with-timer
   2 nil
   (lambda ()
     (global-company-mode 1)))

  (with-eval-after-load 'company
    (diminish 'company-mode)

    (define-keymap
      :keymap company-active-map
      "<tab>" 'company-complete-selection
      "C-n" nil
      "C-p" nil
      "<return>" nil
      "RET" nil)

    (defun just-one-face (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'just-one-face)

    (defun company-capf--candidates-ad (input suffix)
      (require 'vertico)
      (let* ((res (company--capf-data))
             (table (nth 3 res))
             (pred (plist-get (nthcdr 4 res) :predicate))
             (meta (and res
                        (completion-metadata
                         (buffer-substring (nth 1 res) (nth 2 res))
                         table pred))))
        (company-capf--save-current-data res meta)
        (when res
          (let* ((interrupt (plist-get (nthcdr 4 res) :company-use-while-no-input))
                 (all-result (company-capf--candidates-1 input suffix
                                                         table pred
                                                         meta
                                                         (and non-essential
                                                              (eq interrupt t))))
                 (sortfun (or (cdr (assq 'display-sort-function meta))
                              #'vertico-sort-length-alpha))
                 (candidates (assoc-default :completions all-result)))
            (setq company-capf--sorted (functionp sortfun))
            (when candidates
              (setq company-capf--current-boundaries
                    (company--capf-boundaries-markers
                     (assoc-default :boundaries all-result)
                     company-capf--current-boundaries)))
            (when sortfun
              (setq candidates (funcall sortfun candidates)))
            candidates))))
    (advice-add 'company-capf--candidates :override 'company-capf--candidates-ad)))


;;;; corfu

;; (elpaca corfu
;;   (global-corfu-mode 1)
;;   (corfu-echo-mode 1)

;;   (setq corfu-quit-at-boundary 'separator
;;         corfu-quit-no-match nil
;;         corfu-preview-current 'insert
;;         corfu-on-exact-match nil
;;         corfu-auto nil
;;         corfu-preselect 'valid
;;         corfu-auto-delay 0.3
;;         corfu-auto-prefix 3
;;         corfu-map (define-keymap
;;                     "<remap> <forward-sentence>" 'corfu-prompt-end
;;                     "<remap> <backward-sentence>" 'corfu-prompt-beginning
;;                     "<remap> <scroll-down-command>" #'corfu-scroll-down
;;                     "<remap> <scroll-up-command>" #'corfu-scroll-up
;;                     "<tab>" #'corfu-complete
;;                     "RET" nil
;;                     "<return>" nil
;;                     "M-SPC" 'corfu-insert-separator
;;                     "C-h" #'corfu-info-documentation
;;                     "M-h" #'corfu-info-location
;;                     "M-<" #'corfu-first
;;                     "M->" #'corfu-last
;;                     "M-n" #'corfu-next
;;                     "C-n" nil
;;                     "C-j" nil
;;                     "M-p" #'corfu-previous
;;                     "C-p" #'corfu-previous
;;                     "C-g" #'corfu-quit
;;                     "TAB" #'corfu-complete))

;;   (defun my-corfu-auto-on ()
;;     (setq-local corfu-auto t))
;;   (add-hook 'prog-mode-hook 'my-corfu-auto-on)

;;   (with-eval-after-load 'corfu
;;     (defun corfu-sep-and-start ()
;;       (interactive)
;;       (completion-at-point)
;;       (corfu-insert-separator))

;;     (keymap-set corfu-map "M-SPC" #'corfu-sep-and-start)

;;     (with-eval-after-load 'conn
;;       (defun my-corfu-off ()
;;         (global-corfu-mode -1))
;;       (add-hook 'conn-macro-dispatch-start-hook 'my-corfu-off)

;;       (defun my-corfu-on ()
;;         (global-corfu-mode 1))
;;       (add-hook 'conn-macro-dispatch-end-hook 'my-corfu-on)))

;;   (with-eval-after-load 'lsp-mode
;;     (defun wrap-lsp-capf ()
;;       (setq-local completion-at-point-functions
;;                   (cl-nsubst
;;                    (cape-capf-noninterruptible
;;                     (cape-capf-buster #'lsp-completion-at-point))
;;                    #'lsp-completion-at-point completion-at-point-functions)))
;;     (add-hook 'lsp-managed-mode-hook #'wrap-lsp-capf))

;;   (with-eval-after-load 'eglot
;;     (defun wrap-eglot-capf ()
;;       (setq-local completion-at-point-functions
;;                   (cl-nsubst
;;                    (cape-capf-noninterruptible
;;                     (cape-capf-buster #'eglot-completion-at-point))
;;                    #'eglot-completion-at-point completion-at-point-functions)))
;;     (add-hook 'eglot-managed-mode-hook #'wrap-eglot-capf)))


;;;; nerd icons

(elpaca nerd-icons)

(elpaca nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (with-eval-after-load 'diminish
    (diminish 'nerd-icons-dired-mode)))

(elpaca nerd-icons-completion
  (run-with-timer
   3 nil
   (lambda ()
     (with-eval-after-load 'marginalia
       (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))))


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
  (require 'orderless)
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
        completion-category-overrides '((file (styles basic
                                                      partial-completion
                                                      orderless+flex))
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
    (orderless-style-dispatchers '(orderless-affix-dispatch))))


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
    "p" 'consult-page
    "K" 'consult-kmacro
    "w" 'consult-man
    "e" 'consult-isearch-history
    "t" 'consult-outline
    "o" 'consult-line
    "O" 'consult-line-multi
    "G" 'consult-git-grep
    "g" 'consult-ripgrep
    "f" 'consult-find
    "L" 'consult-locate
    "v" 'consult-focus-lines
    "k" 'consult-keep-lines)

  (keymap-set goto-map "g" 'consult-goto-line)
  (keymap-global-set "<remap> <project-switch-to-buffer>" 'consult-project-buffer)

  (define-keymap
    :keymap isearch-mode-map
    "M-s j" 'consult-line
    "M-s J" 'consult-line-multi)

  (with-eval-after-load 'consult
    (setq consult-ripgrep-args (concat consult-ripgrep-args " --multiline"))

    (consult-customize consult-completion-in-region :preview-key nil)
    (consult-customize consult--source-bookmark :preview-key "C-o")
    (consult-customize consult-bookmark :preview-key "C-o")
    (consult-customize consult-buffer :preview-key "C-o")
    (consult-customize consult-project-buffer :preview-key "C-o"))

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
  (advice-add 'register-preview :override #'consult-register-window)

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
  (keymap-set goto-map "e" 'consult-goto-edit)

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

;;;;; consult-dir

(elpaca consult-dir
  (keymap-global-set "C-x C-d" 'consult-dir)

  (with-eval-after-load 'conn
    (keymap-set conn-state-map "D" 'consult-dir))

  (with-eval-after-load 'vertico
    (define-keymap
      :keymap vertico-map
      "C-x y" 'consult-dir
      "C-x C-j" 'consult-dir-jump-file)))

;;;;; consult-lsp

(elpaca consult-lsp
  (with-eval-after-load 'lsp-mode
    (keymap-set lsp-mode-map "M-s x" #'consult-lsp-symbols)
    (keymap-set lsp-mode-map "M-s >" #'consult-lsp-diagnostics)
    (keymap-set lsp-mode-map "M-s <" #'consult-lsp-file-symbols)))

;;;;; consult-projectile

(elpaca consult-projectile
  (with-eval-after-load 'projectile
    (keymap-set projectile-command-map "f" 'consult-projectile)))


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
  (defun my-vertico-buffer-stop-face-remap ()
    (setq-local face-remapping-alist
                (seq-remove (lambda (cons)
                              (eq (car cons) 'mode-line-inactive))
                            face-remapping-alist)))
  (advice-add 'vertico-buffer--setup :after #'my-vertico-buffer-stop-face-remap)

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
    "C-M-j" #'vertico-quick-jump
    "C-M-<return>" #'vertico-exit-input
    "C-j" #'vertico-exit-input
    "C-S-j" #'vertico-quick-exit
    "C-w" #'my-vertico-copy-or-kill)

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

  (defun my-marginalia-annotate-binding (cand)
    "Annotate command CAND with keybinding."
    (when-let ((sym (intern-soft cand))
               (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
      (format #(" {%s}" 1 5 (face marginalia-key)) (key-description key))))

  (defun marginalia-annotate-command-with-alias (cand)
    "Annotate command CAND with its documentation string.
    Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let ((sym (intern-soft cand)))
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
  (advice-add 'marginalia--align :override 'marginalia--align-column))


;;;; tempel

(elpaca tempel
  (keymap-global-set "M-I" 'tempel-insert)
  (keymap-global-set "M-TAB" 'my-tempel-expand-or-complete)
  ;; (global-tempel-abbrev-mode -1)

  (defun my-tempel-expand-or-complete (&optional interactive)
    (interactive (list t))
    (require 'tempel)
    (if interactive
        (tempel--interactive #'my-tempel-expand-or-complete)
      (if-let ((templates (tempel--templates))
               (bounds (tempel--prefix-bounds))
               (name (buffer-substring-no-properties
                      (car bounds) (cdr bounds)))
               (sym (intern-soft name))
               (template (assq sym templates)))
          (progn
            (setq templates (list template))
            (list (car bounds) (cdr bounds) templates
                  :category 'tempel
                  :exclusive 'no
                  :exit-function (apply-partially #'tempel--exit templates nil)))
        (tempel-complete))))

  (with-eval-after-load 'tempel
    (keymap-set tempel-map "M-n" 'tempel-next)
    (keymap-set tempel-map "M-p" 'tempel-previous)
    (keymap-set tempel-map "M-i" 'tempel-done)

    (setq tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))

    (defun tempel-edit-template ()
      (interactive)
      (let ((default-directory (expand-file-name "templates/" user-emacs-directory)))
        (call-interactively 'find-file)))

    (defun conn-tempel-insert-ad (fn &rest args)
      (apply fn args)
      (when tempel--active (conn-emacs-state)))
    (advice-add 'tempel-insert :around 'conn-tempel-insert-ad)))


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


;;;; tuareg

(elpaca tuareg)


;;;; rfc-mode

(elpaca rfc-mode)


;;;; heex-ts-mode

(elpaca heex-ts-mode)


;;;; polymode

(elpaca polymode)


;;;; denote

(elpaca (denote :files (:defaults "denote-org-extras.el"))
  (with-eval-after-load 'denote
    (require 'denote-silo-extras)
    (with-eval-after-load 'org
      (require 'denote-org-extras))
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

  (keymap-global-set "C-c n e" #'denote-org-extras-extract-org-subtree)
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

(elpaca teco)


;;;; dumb-jump

(elpaca dumb-jump
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))


;;;; jinx

(elpaca jinx
  (run-with-idle-timer 2 nil (lambda () (global-jinx-mode 1)))

  (with-eval-after-load 'jinx
    (with-eval-after-load 'diminish
      (diminish 'jinx-mode " $"))

    (define-keymap
      :keymap (conn-get-mode-map 'conn-state 'jinx-mode)
      "$" 'jinx-correct-nearest
      "C-$" 'jinx-correct-all)

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
        (setf (alist-get cmd conn-dispatch-default-actions-alist)
              'my-jinx-dispatch-check)))))


;;;; ef-themes

(elpaca ef-themes)


;;;; pgmacs

;; (elpaca (pgmacs :host github
;;                 :repo "emarsden/pgmacs"))


;;;; eat

(elpaca eat)


;;;; beancount

;; (elpaca beancount)


;;;; projectile

(elpaca projectile
  (setq projectile-mode-line-prefix ""
        projectile-dynamic-mode-line nil)
  (run-with-timer 2 nil (lambda () (projectile-mode 1)))

  (with-eval-after-load 'projectile
    (keymap-global-unset "C-x p")
    (keymap-global-set "C-x p" 'projectile-command-map)

    (define-keymap
      :keymap projectile-command-map
      "e" 'projectile-run-eshell
      "d" 'projectile-dired
      "D" 'projectile-find-dir
      "j" 'projectile-run-gdb))

  (with-eval-after-load 'conn
    (keymap-set (conn-get-mode-map 'conn-state 'projectile-mode)
                "," 'projectile-command-map)))

;;;; smart parens

(elpaca (smartparens :host github
                     :repo "Fuco1/smartparens")
  (with-eval-after-load 'smartparens
    (with-eval-after-load 'diminish
      (diminish 'smartparens-mode)))

  (add-hook 'lisp-data-mode-hook 'smartparens-strict-mode)
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)

  (define-keymap
    :keymap smartparens-mode-map
    "C-M-f" 'sp-forward-sexp ;; navigation
    "C-M-b" 'sp-backward-sexp
    "C-M-u" 'sp-backward-up-sexp
    "C-M-d" 'sp-down-sexp
    "C-M-p" 'sp-backward-down-sexp
    "C-M-n" 'sp-up-sexp
    "C-M-k" 'sp-kill-sexp
    "C-M-w" 'sp-copy-sexp
    "C-S-d" 'sp-beginning-of-sexp
    "C-S-a" 'sp-end-of-sexp
    "M-<up>" 'sp-splice-sexp-killing-backward ;; depth-changing commands
    "M-<down>" 'sp-splice-sexp-killing-forward
    "M-l" 'sp-splice-sexp
    "M-r" 'sp-splice-sexp-killing-around
    "M-(" 'sp-wrap-round
    "C-)" 'sp-forward-slurp-sexp ;; barf/slurp
    "C-<right>" 'sp-forward-slurp-sexp
    "C-}" 'sp-forward-barf-sexp
    "C-<left>" 'sp-forward-barf-sexp
    "C-(" 'sp-backward-slurp-sexp
    "C-M-<left>" 'sp-backward-slurp-sexp
    "C-{" 'sp-backward-barf-sexp
    "C-M-<right>" 'sp-backward-barf-sexp
    "C-S-l" 'sp-forward-slurp-sexp
    "C-S-o" 'sp-forward-barf-sexp
    "C-S-j" 'sp-backward-slurp-sexp
    "C-S-u" 'sp-backward-barf-sexp
    "C-S-i" 'sp-convolute-sexp
    "C-S-k" 'sp-join-sexps))


;;;; puni

;; (elpaca puni
;;   (add-hook 'prog-mode-hook #'puni-mode)
;;   (keymap-unset puni-mode-map "C-M-f")
;;   (keymap-unset puni-mode-map "C-M-b")
;;   (keymap-unset puni-mode-map "C-M-a")
;;   (keymap-unset puni-mode-map "C-M-e")
;;   (keymap-unset puni-mode-map "M-(")
;;   (keymap-unset puni-mode-map "M-)"))


;;;; djvu

(elpaca djvu)


;;;; hide mode line

(elpaca hide-mode-line)


;;;; aggressive indent mode

(elpaca aggressive-indent
  (add-hook 'lisp-data-mode-hook 'aggressive-indent-mode))

;; Local Variables:
;; outline-regexp: ";;;;* [^    \n]"
;; End:
