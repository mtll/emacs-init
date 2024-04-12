;; -*- lexical-binding: t; -*-
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 500000000)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            "Restore default values after init."
            (message "Emacs loaded %d packages in %s."
                     (cdar elpaca--status-counts)
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time))))
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 8 1024 1024))
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

(fringe-mode '(8 . 8))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)

(setq window-resize-pixelwise t
      custom-file (make-temp-file "emacs-custom-")
      frame-resize-pixelwise t
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      frame-title-format (list '(:eval (alist-get 'name (tab-bar--current-tab))))
      native-comp-async-report-warnings-errors 'silent
      frame-inhibit-implied-resize t
      package-enable-at-startup nil
      initial-buffer-choice nil
      ring-bell-function #'ignore
      inhibit-startup-screen t
      inhibit-x-resources t
      load-prefer-newer t)

(advice-add #'x-apply-session-resources :override #'ignore)

(load (expand-file-name "font.el" user-emacs-directory) t t)

(when (>= emacs-major-version 30)
  (with-eval-after-load 'modus-themes
    (setq modus-themes-common-palette-overrides
          (seq-concatenate
           'list
           `((bg-main "#fff5e8")
             (fg-active-argument "#930c93")
             (bg-active-argument "#f4caf4")
             (cursor "#00517d")
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
           modus-themes-preset-overrides-warmer))

    (load-theme 'modus-operandi-tinted t)

    (custom-set-faces
     `(transient-key-exit ((t :inherit modus-themes-key-binding :foreground "#a60000")))
     `(transient-key-return ((t :inherit modus-themes-key-binding :foreground "#6f5500")))
     `(transient-key-stay ((t :inherit modus-themes-key-binding :foreground "#008900"))))))

(provide 'early-init)
