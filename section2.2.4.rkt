#lang racket

(require sicp-pict)

(displayln "The picture language")

(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4-old (below einstein2 einstein2))

(paint einstein4-old)

(define (flipped-pairs1 painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define einstein4 (flipped-pairs1 einstein))

(paint einstein4)

(define (up-split1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split1 painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split1 painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split1 painter (- n 1)))
            (right (right-split1 painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit1 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit1 einstein 1))

(displayln "Exercise 2.44")
(displayln "defined above")
(paint (right-split1 einstein 1))
(paint (up-split1 einstein 1))
(paint (right-split1 einstein 2))
(paint (up-split1 einstein 2))
(paint (corner-split einstein 2))

(displayln "Higher-order operations")

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(displayln "Exercise 2.45")
(define (split half quarter)
  (define (iter painter n)
    (let ((smaller (iter painter (- n 1))))
      (half painter (quarter smaller smaller))))
  (lambda (painter n)
    (iter painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split1 einstein 2))

(displayln "Frames")

(displayln "Exercise 2.46")

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(displayln "Exercise 2.47")

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame1 frame)
  (car frame))
(define (edge1-frame1 frame)
  (cadr frame))
(define (edge2-frame1 frame)
  (caddr frame))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(displayln "Painters")

(displayln "Exercise 2.48")
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (draw-line start end)
  (display "draw line from: ")
  (display start)
  (display " to ")
  (display end)
  (newline))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define std-frame (make-frame (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 1.0)))

(displayln "draw outline:")
(define draw-outline
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 0))
                           (make-segment (make-vect 1 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 0 0)))))
(draw-outline std-frame)

(displayln "draw x:")
(define draw-x
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 1)))))
(draw-x std-frame)

(displayln "draw diamond")
(define draw-diamond
  (segments->painter (list (make-segment (make-vect 0.5 0)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 0)))))
(draw-diamond std-frame)

(displayln "wave too hard")

(displayln "Transforming and combining painters")
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))
(define (flip-vert-seg painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right-seg painter)
  (transform-painter
   painter
   (make-vect 0.5 0.5)
   (make-vect 1.0 0.5)
   (make-vect 0.5 1.0)))
(define (rotate90-seg painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards-seg painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (beside-seg painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(displayln "Exercise 2.50")
(define (flip-horiz-seg painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate180-seg painter)
  (compose1 rotate90 rotate90))
(define (rotate270 painter)
  (compose1 rotate90 rotate90 rotate90))

(displayln "Exercise 2.51")
(define (below-seg painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-up
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-up frame)))))

(displayln "Levels of language for robust design")

(displayln "Exercise 2.52 pass")