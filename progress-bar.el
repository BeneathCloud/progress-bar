(setq symbol-step 10)
(setq step-symbols '("│" "│"))

(defun progress-bar (progress all)
  "Reture a text progress bar from Num progress and Num all"
  (interactive)
  (if (and (<= progress all)
           (>= progress 0))
      (let* ((visual-base (- (length step-symbols) 1))
             (visual-progress (/ progress symbol-step))
             (visual-all (/ all symbol-step))
             (visual-left (- visual-all visual-progress))
             (full-positions (/ visual-all visual-base))
             (full-progress-positions (/ visual-progress visual-base))
             (changing-position-detail (mod visual-progress visual-base))
             (full-left-positions (/ (- visual-left
                                        (if (= changing-position-detail 0)
                                            0
                                          (- visual-base
                                             changing-position-detail)))
                                     visual-base)))
        (concat (propertize (concat (s-repeat full-progress-positions (-last-item step-symbols))
                                    (if (or (= changing-position-detail 0)
                                            (>= full-progress-positions full-positions))
                                        ""
                                      (nth changing-position-detail step-symbols)))
                            'font-lock-face
                            (list :foreground nano-light-foreground :family "PragmataPro Mono"))
                (propertize (s-repeat full-left-positions (car step-symbols))
                            'font-lock-face
                            (list :foreground nano-light-faded :family "PragmataPro Mono")) " "
                (format "%d/%d %.1f%%" progress all (* (/ (float progress) all) 100))))
    (defun insert-progress-bar-at-point (progress all)
      (interactive)
      (let ((progress-bar (progress-bar progress all)))
        (when progress-bar
          (insert (progress-bar progress all)))))   (progn (message "ill progress") nil)))

(defun progress-bar-status (progress)
  "Return a '(progress all) from a text progress-bar"
  (interactive)
  (save-match-data
    (if (string-match "\\([0-9]+\\)\s*/\s*\\([0-9]+\\)" progress)
        (let ((progress (match-string 1 progress))
              (all (match-string 2 progress)))
          (mapcar 'string-to-number (list progress all)))
      (progn (message "no progress detected")
             nil))))

(defun get-progress-bar-status-at-point ()
  "Get '(progress all) from current line if successfully, otherwise nil"
  (interactive)
  (progress-bar-status (thing-at-point 'line t)))

(defun update-progress-bar-with-fn-at-point (fn)
  "fn should receive 2 args and return a status"
  (interactive)
  (let ((status (get-progress-bar-status-at-point)))
    (when status
      (let ((progress-bar (apply 'progress-bar
                                 (apply fn status))))
        (when progress-bar
          (kill-region (line-beginning-position) (line-end-position))
          (insert progress-bar))))))

(defun update-progress-bar-at-point ()
  "Update the progress bar at the line depend on the a/b in the line (e.g 12/20)"
  (interactive)
  (update-progress-bar-with-fn-at-point (lambda (p a) (list p a))))

(defun forward-progress-bar-at-point ()
  (interactive)
  (update-progress-bar-with-fn-at-point (lambda (p a) (list (+ p 1) a))))

(defun backward-progress-bar-at-point ()
  (interactive)
  (update-progress-bar-with-fn-at-point (lambda (p a) (list (- p 1) a))))

(defun expand-progress-bar-at-point ()
  (interactive)
  (update-progress-bar-with-fn-at-point (lambda (p a) (list p (+ a 1)))))

(defun shrink-progress-bar-at-point ()
  (interactive)
  (update-progress-bar-with-fn-at-point (lambda (p a) (list p (- a 1)))))

(provide 'progress-bar)
