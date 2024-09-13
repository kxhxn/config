(setq gc-cons-threshold 1000000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq-default gc-cons-threshold (* 1024 1024 100))
   ;; (message "gc-cons-threshold restored to %S" gc-cons-threshold)
   ))

(setq-default lexical-binding t
              load-prefer-newer t)

(setq package-enable-at-startup nil)
