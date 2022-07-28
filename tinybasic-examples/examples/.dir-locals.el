((nil
  (eval
   (lambda ()
     (when (and (buffer-file-name)
                (string= (file-name-extension (buffer-file-name))
                         "rkt")
                (not (member (file-name-nondirectory (buffer-file-name))
                             '("info.rkt" "main.rkt")))
                (not (equal major-mode 'basic-mode)))
       (message "dir-locals.el: Switching to BASIC mode...")
       (basic-mode))))))
