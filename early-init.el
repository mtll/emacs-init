;; -*- lexical-binding: t; -*-
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 500000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 5000000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      menu-bar-mode nil
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      frame-title-format (list "%b")
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      native-comp-async-report-warnings-errors 'silent
      frame-inhibit-implied-resize t
      package-enable-at-startup nil
      initial-buffer-choice nil
      inhibit-startup-screen t)

(advice-add #'x-apply-session-resources :override #'ignore)

(load custom-file 'noerror)

(provide 'early-init)
