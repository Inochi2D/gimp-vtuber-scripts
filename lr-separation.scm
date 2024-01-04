(define (script-fu-lr-recursive-process image)
  (gimp-undo-push-group-start image)

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

  ;; Auxiliary function to get the position of a layer within its parent
  (define (get-layer-position-aux layer siblings position)
    (if (null? siblings)
        position
        (if (= (car siblings) layer)
            position
            (get-layer-position-aux layer (cdr siblings) (+ position 1)))))

  ;; Function to get the position of a layer in its parent
  (define (get-layer-position layer parent-id)
    (let ((siblings (if (= parent-id 0)
                        (vector->list (cadr (gimp-image-get-layers image)))
                        (vector->list (cadr (gimp-item-get-children parent-id))))))
      (get-layer-position-aux layer siblings 0)))

  ;; Function to duplicate and rename a layer or layer group
  (define (duplicate-and-rename-item item new-name-suffix parent-id)
    (let* ((new-name (string-replace (car (gimp-item-get-name item)) "::LR" new-name-suffix))
           (new-item (car (gimp-layer-copy item TRUE)))
           (position (get-layer-position item parent-id)))
      (gimp-item-set-name new-item new-name)
      (gimp-image-insert-layer image new-item parent-id position)
      new-item))

  (define (for-each-zip function array1 array2)
    (if (null? array1)
        #t
        (begin (function (car array1) (car array2))
               (for-each-zip function (cdr array1) (cdr array2)))
        )
  )

  ;; Function to crop a layer based on its name and content
  (define (crop-layer-by-name-and-content layer)
    (let* ((layer-name (car (gimp-item-get-name layer)))
           (width (car (gimp-drawable-width layer)))
           (half-width (/ width 2)))
      (cond
       ((string-ends-with? layer-name "::L")
        (gimp-layer-resize layer half-width (car (gimp-drawable-height layer)) (- half-width) 0))
       ((string-ends-with? layer-name "::R")
        (gimp-layer-resize layer half-width (car (gimp-drawable-height layer)) 0 0)))
      ));(plug-in-autocrop-layer RUN-NONINTERACTIVE image layer)))

  (define (process-lr-child rchild lchild)
    (let ((is-group (equal? (car (gimp-item-is-group rchild)) TRUE))
          (name (car (gimp-item-get-name rchild))))
      (gimp-item-set-name rchild (string-append name "::R"))
      (gimp-item-set-name lchild (string-append name "::L"))
      (if is-group
          (let ((rchildren (vector->list (cadr (gimp-item-get-children rchild))))
                (lchildren (vector->list (cadr (gimp-item-get-children lchild)))))
            (for-each-zip process-lr-child rchildren lchildren))
          (begin
            (crop-layer-by-name-and-content rchild)
            (crop-layer-by-name-and-content lchild)))))

  ;; Recursive function to process layers and layer groups
  (define (process-item item parent-id)
    (let ((item-name (car (gimp-item-get-name item)))
          (is-group (equal? (car (gimp-item-is-group item)) TRUE)))
      (if (string-ends-with? item-name "::LR")
          (let ((left-item (duplicate-and-rename-item item "::L" parent-id)))
            (gimp-item-set-name item (string-replace item-name "::LR" "::R"))
            (if is-group
                (let ((rchildren (vector->list (cadr (gimp-item-get-children item))))
                      (lchildren (vector->list (cadr (gimp-item-get-children left-item)))))
                    (for-each-zip process-lr-child rchildren lchildren))
                (begin
                    (crop-layer-by-name-and-content item)
                    (crop-layer-by-name-and-content left-item)))
            item)
          (if is-group
              (let ((children (vector->list (cadr (gimp-item-get-children item)))))
                (for-each (lambda (child)
                            (process-item child item))
                          children))))))

  ;; Main processing
  (let* ((top-level-data (gimp-image-get-layers image))
         (top-level-layers (vector->list (cadr top-level-data))))
    (for-each (lambda (layer)
                (process-item layer 0))
              top-level-layers))

  (gimp-undo-push-group-end image))

(script-fu-register "script-fu-lr-recursive-process"
                    "LR Recursive Process"
                    "Recursively process '::LR' layers and layer groups for duplication and renaming"
                    "Your Name"
                    "Your Name"
                    "2024"
                    "*"
                    SF-IMAGE "Image" 0)

(script-fu-menu-register "script-fu-lr-recursive-process" "<Image>/Layer")
