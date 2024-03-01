;; -*- lexical-binding: t; -*-
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 500000000)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            "Restore default values after init."
            (message "Emacs loaded %d packages in %s with %d garbage collections."
                     (cdar elpaca--status-counts)
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 8 1024 1024))
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

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
      inhibit-startup-screen t)

(advice-add #'x-apply-session-resources :override #'ignore)

(face-spec-set 'default '((t (:inherit nil :extend nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 142 :width normal :foundry "UKWN" :family "Iosevka DK"))))

(provide 'early-init)
