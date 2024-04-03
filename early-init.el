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
      frame-title-format (list "%b")
      native-comp-async-report-warnings-errors 'silent
      frame-inhibit-implied-resize t
      package-enable-at-startup nil
      initial-buffer-choice nil
      ring-bell-function #'ignore
      inhibit-startup-screen t
      inhibit-x-resources t
      load-prefer-newer t)

(advice-add #'x-apply-session-resources :override #'ignore)

(face-spec-set 'default '((default (:slant normal
                                           :weight medium
                                           :height 142
                                           :width normal
                                           :family "Iosevka DK"))))

(when (>= emacs-major-version 30)
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
           modus-themes-preset-overrides-warmer)))

  (load-theme 'modus-operandi-tinted t))

(provide 'early-init)
