(define (script-fu-auto-crop-recursive image drawable)
  (define (auto-crop-layer layer)
    (if (= (car (gimp-item-is-group layer)) TRUE)
        (map auto-crop-layer (vector->list (cadr (gimp-item-get-children layer))))
        (begin
          (gimp-image-set-active-layer image layer)
          (plug-in-autocrop-layer RUN-NONINTERACTIVE image layer))))

  (gimp-image-undo-group-start image) ; Start of undo group

  (let* ((top-level-layers (vector->list (cadr (gimp-image-get-layers image)))))
    (map auto-crop-layer top-level-layers))

  (gimp-image-undo-group-end image) ; End of undo group
  (gimp-displays-flush))

(script-fu-register "script-fu-auto-crop-recursive"
                    "Auto-crop Recursive"
                    "Recursively auto-crops all layers and their descendants in an image. All operations are combined into a single undo step."
                    "Your Name"
                    "Your Name"
                    "2024"
                    "*"
                    SF-IMAGE    "Image"    0
                    SF-DRAWABLE "Drawable" 0)

(script-fu-menu-register "script-fu-auto-crop-recursive"
                         "<Image>/Layer")
