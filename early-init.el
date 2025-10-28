;; -*- lexical-binding: t; -*-
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 500000000
      gc-cons-percentage 0.6)
(add-hook 'elpaca-after-init-hook
          (let ((format mode-line-format))
            (lambda ()
              "Restore default values after init."
              (setq-default mode-line-format format
                            inhibit-message nil
                            inhibit-redisplay nil)
              (setq file-name-handler-alist default-file-name-handler-alist)
              (setq gc-cons-threshold (* 32 1024 1024)
                    gc-cons-percentage 0.6)
              (if (boundp 'after-focus-change-function)
                  (add-function :after after-focus-change-function
                                (lambda ()
                                  (unless (frame-focus-state)
                                    (garbage-collect))))
                (add-hook 'focus-out-hook 'garbage-collect))
              (message "Emacs loaded %d packages in %s."
                       (cdar elpaca--status-counts)
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract (current-time) before-init-time)))))))

(setq-default mode-line-format nil
              inhibit-message t
              inhibit-redisplay t)

;; (load (expand-file-name "elpaca/repos/benchmark-init-el/benchmark-init.el" user-emacs-directory) t  t)
;; (load (expand-file-name "elpaca/repos/benchmark-init-el/benchmark-init-modes.el" user-emacs-directory) t  t)
;; (require 'benchmark-init)
;; (benchmark-init/activate)
;; (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate)

(setq-default truncate-lines nil
              ;; truncate-partial-width-windows nil
              window-resize-pixelwise nil)

;; redisplay
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      bidi-inhibit-bpa t
      load-path-filter-function 'load-path-filter-cache-directory-files)

(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

;; (fringe-mode '(10 . 10))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(blink-cursor-mode -1)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(setq window-resize-pixelwise t
      custom-file (make-temp-file "emacs-custom-")
      frame-resize-pixelwise t
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
      frame-title-format (list '(:eval (alist-get 'name (tab-bar--current-tab))))
      native-comp-async-report-warnings-errors 'silent
      frame-inhibit-implied-resize t
      package-enable-at-startup nil
      initial-buffer-choice t
      ring-bell-function #'ignore
      pulse-face-duration 0.15
      inhibit-startup-screen t
      inhibit-x-resources t
      load-prefer-newer t
      read-process-output-max (ash 1 18)
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      auto-mode-case-fold nil
      process-adaptive-read-buffering nil)

(advice-add 'display-startup-echo-area-message :override #'ignore)
(advice-add 'display-startup-screen :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)

(load (expand-file-name "font.el" user-emacs-directory) t t)
(load (expand-file-name "custom.el" user-emacs-directory) t t)

(provide 'early-init)
