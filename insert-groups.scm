(define (script-fu-insert-group-layers image)
  (gimp-undo-push-group-start image)

  (define (should-process-layer layer)
    (and (not (char=? (string-ref (car (gimp-item-get-name layer)) 0) #\#))
         (not (equal? (car (gimp-item-is-group layer)) TRUE))))

  (define (process-layer layer parent-id)
    (if (should-process-layer layer)
        (let* ((name (car (gimp-item-get-name layer)))
               (position (get-position-in-parent layer parent-id))
               (new-group (car (gimp-layer-group-new image))))
          (gimp-item-set-name layer (string-append "#" name))
          (gimp-item-set-name new-group name)
          (gimp-image-insert-layer image new-group parent-id position)
          (gimp-image-reorder-item image layer new-group 0))))

  (define (get-position-in-parent layer parent-id)
    (let* ((siblings (if (= parent-id 0)
                         (vector->list (cadr (gimp-image-get-layers image)))
                         (vector->list (cadr (gimp-item-get-children parent-id)))))
           (position 0))
      (for-each (lambda (sibling)
                  (if (= sibling layer)
                      (begin)
                      (set! position (+ position 1))))
                siblings)
      position))

  (define (process-layers layers parent-id)
    (for-each (lambda (layer)
                (if (equal? (car (gimp-item-is-group layer)) TRUE)
                    (let* ((children-data (gimp-item-get-children layer))
                           (children (vector->list (cadr children-data))))
                      (process-layers children layer))
                    (process-layer layer parent-id)))
              layers))

  (let* ((top-level-data (gimp-image-get-layers image))
         (top-level-layers (vector->list (cadr top-level-data))))
    (process-layers top-level-layers 0))

  (gimp-undo-push-group-end image))

(script-fu-register "script-fu-insert-group-layers"
                    "Replace Layers to Layer Group"
                    "Create a new layer group for each layer unless the layer name begins with '#', preserving the original order"
                    "Your Name"
                    "Your Name"
                    "2024"
                    "*"
                    SF-IMAGE "Image" 0)

(script-fu-menu-register "script-fu-insert-group-layers" "<Image>/Layer")
