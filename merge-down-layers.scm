(define (script-fu-merge-down image)
  (gimp-undo-push-group-start image)

  ;; Custom function to check if a string starts with a given prefix
  (define (string-starts-with? string prefix)
    (if (< (string-length string) (string-length prefix))
        #f
        (string=? (substring string 0 (string-length prefix)) prefix)))

  ;; Custom function to check if a string ends with a given suffix
  (define (string-ends-with? string suffix)
    (let ((string-length (string-length string))
          (suffix-length (string-length suffix)))
      (if (< string-length suffix-length)
          #f
          (string=? (substring string (- string-length suffix-length)) suffix))))

  ;; Custom function to replace a substring within a string
  (define (string-replace str old new)
    (let ((index (string-index str old)))
      (if index
          (string-append (substring str 0 index)
                         new
                         (string-replace (substring str (+ index (string-length old))) old new))
          str)))

  ;; Helper function to find the index of a substring in a string
  (define (string-index str sub)
    (let loop ((i 0))
      (if (> (+ i (string-length sub)) (string-length str))
          #f
          (if (string=? (substring str i (+ i (string-length sub))) sub)
              i
              (loop (+ i 1))))))

  ;; Function to determine if a layer should be merged down
  (define (should-merge-layer-group layer)
    (and (equal? (car (gimp-item-is-group layer)) TRUE)
         (not (string-starts-with? (car (gimp-item-get-name layer)) "*"))))

  ;; Recursive function to merge layer groups that do not start with "*"
  (define (merge-layer-groups layer)
    (when (equal? (car (gimp-item-is-group layer)) TRUE)
      (let ((children (vector->list (cadr (gimp-item-get-children layer)))))
        (for-each merge-layer-groups children)
        (if (should-merge-layer-group layer)
            (gimp-image-merge-layer-group image layer)))))

  ;; Main processing
  (let* ((top-level-data (gimp-image-get-layers image))
         (top-level-layers (vector->list (cadr top-level-data))))
    ;; Merge Layer Groups
    (for-each merge-layer-groups top-level-layers)
    )

  (gimp-undo-push-group-end image))

(script-fu-register "script-fu-merge-down"
                    "Merge-down layers for Rigging"
                    "Recursively merge layer groups not starting with '*'"
                    "Your Name"
                    "Your Name"
                    "2024"
                    "*"
                    SF-IMAGE "Image" 0)

(script-fu-menu-register "script-fu-merge-down" "<Image>/Layer")
