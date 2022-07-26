((nil
  (eval
   (lambda ()
     (when (and (buffer-file-name)
                (string= (file-name-extension (buffer-file-name))
                         "rkt")
                (not (string= (file-name-nondirectory (buffer-file-name))
                              "info"))
                (not (equal major-mode 'basic-mode)))
       (message "dir-locals.el: Switching to BASIC mode...")
       (basic-mode))))))
