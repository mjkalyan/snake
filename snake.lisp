;;;; snake.lisp

(in-package #:snake)

(defun make-coords (&optional x y)
  (make-array 2 :initial-contents (list (or x (+ *easing* (random (- *board-size*
                                                                   (* 2 *easing*)))))
                                        (or y (+ *easing* (random (- *board-size*
                                                                   (* 2 *easing*))))))))

(defun logical->px (c)
  "Calculate a pixel coordinate based on a logical coordinate C."
  (* c *px-per-seg*))

(defun random-heading ()
  "Pick a random direction."
  (alexandria:random-elt (list :up :down :left :right)))

(defun random-starting-trail (heading length)
  "Based on HEADING, pick a starting trail (position and size) for a snake."
  (loop for i from 1 to length
        with (x y) = (list (/ *board-size* 2) (/ *board-size* 2))
        for (xoff yoff) = (case heading
                            (:up    (list    0    i))
                            (:down  (list    0 (- i)))
                            (:left  (list    i    0))
                            (:right (list (- i)   0)))
        collect (make-coords (+ x xoff) (+ y yoff))))

(defclass snake ()
  ((%heading :initarg :heading
             :accessor heading
             :documentation "Which way is the snake going?")
   (%trail :initarg :trail
           :accessor trail
           :documentation "A list of the logical positions where the snake is and has been. This is
as long as the snake.")
   (%should-grow :initform nil
                 :accessor should-grow
                 :documentation "Whether the snake should grow.")))

(defun make-snake (&optional (heading (random-heading)) (length 3))
  (let ((trail (random-starting-trail heading length)))
    (make-instance 'snake :trail trail :heading heading)))

(defun overlapping-p (seg1 seg2)
  "Are these segments overlapping?"
  (and (= (elt seg1 0) (elt seg2 0))
       (= (elt seg1 1) (elt seg2 1))))

(defun hitting-wall-p (head)
  "Is the head hitting a wall?"
  (or (not (< -1 (elt head 0) *board-size*))
      (not (< -1 (elt head 1) *board-size*))))

(defun check-collisions (snake food)
  "Has the snake has collided with something? If so, should we eat it or die?"
  (let ((head (car (trail snake))))
    (cond ((overlapping-p head
                          ;; TODO not efficient, maybe food should have its own logical pos
                          (make-coords (truncate (/ (x food) *px-per-seg*))
                                       (truncate (/ (y food) *px-per-seg*))))
           :eat)
          ((or (hitting-wall-p head)
               (loop for segment in (cdr (trail snake))
                     when (overlapping-p head segment) return t))
           :die))))

(defun maybe-change-heading (snake)
  "Change the snake's heading when indicated by the player."
  (setf (heading snake)
        (cond ((and (is-key-pressed-p +key-up+)
                    (not (eq (heading snake) :down)))
               :up)
              ((and (is-key-pressed-p +key-down+)
                    (not (eq (heading snake) :up)))
               :down)
              ((and (is-key-pressed-p +key-left+)
                    (not (eq (heading snake) :right)))
               :left)
              ((and (is-key-pressed-p +key-right+)
                    (not (eq (heading snake) :left)))
               :right)
              (t (heading snake)))))

(defun update-trail (snake)
  "Move the snake, growing it when needed."
  (let ((new-segment (alexandria:copy-array (car (trail snake)))))
    (case (heading snake)
      (:right (incf (elt new-segment 0)))
      (:left  (decf (elt new-segment 0)))
      (:down  (incf (elt new-segment 1)))
      (:up    (decf (elt new-segment 1))))
    (setf (trail snake)
          (if (should-grow snake)
              (progn (setf (should-grow snake) nil)
                     (cons new-segment (trail snake)))
              (cons new-segment (butlast (trail snake)))))))

(defun new-pos (food snake)
  "Place the food at location not occupied by snake."
  (loop for xy = (make-coords)
        when (notany (lambda (snake-seg)
                       (overlapping-p xy snake-seg))
                     (trail snake))
          return (setf (x food) (logical->px (elt xy 0))
                       (y food) (logical->px (elt xy 1)))))

(defun draw-snake (snake)
  "Draw each segment of the snake."
  (loop for segment in (trail snake)
        do (claylib/ll:draw-rectangle (logical->px (elt segment 0))
                                      (logical->px (elt segment 1))
                                      *px-per-seg*
                                      *px-per-seg*
                                      (claylib::c-struct +green+))))

(defun prompt-restart (snake)
  "Ask the player if they want to start again."
  (with-drawing (:bgcolor +black+)
    (claylib/ll:draw-text
     (format nil "You died with a length of ~a!~%SPC to restart or ESC to ext."
             (length (trail snake)))
     (logical->px (/ *board-size* 4))
     (logical->px (/ *board-size* 2))
     20
     (claylib::c-struct +orange+)))
  (is-key-pressed-p +key-space+))

(defun reset (snake food)
  "Reset the snake and food."
  (setf (heading snake) (random-heading)
        (trail snake) (random-starting-trail (heading snake) 3)
        (should-grow snake) nil)
  (new-pos food snake))

(defun initialize ()
  "Initialize all global variables."

  (defparameter *board-size* 30
    "The number of x & y segments length of the game board.")

  (defparameter *px-per-seg* 20
    "How many pixels an edge of a segment is.")

  (defparameter *easing* 2
    "Minimum distance from the edge of the screen that food must be placed.")

  (defparameter *scene*
    (make-scene ()
                ((snake (make-snake))
                 (food (let ((xy (make-coords)))
                         (make-rectangle (logical->px (elt xy 0))
                                         (logical->px (elt xy 1))
                                         *px-per-seg* *px-per-seg*
                                         +orange+
                                         :filled nil
                                         :thickness 3))))))
  (print "Init done..."))

(defun main ()
  (initialize)
  (with-window (:title "Snake!"
                :width (logical->px *board-size*)
                :height (logical->px *board-size*)
                :fps 60)
    (with-scenes *scene*
      (with-scene-objects (snake food) *scene*
        (do-game-loop (:livesupport t
                       :vars ((diedp nil)
                              (frame-counter 0)
                              (frame-speed 1/124)))
          (if diedp
              (let ((restartp (prompt-restart snake)))
                (when restartp
                  (reset snake food)
                  (setf diedp nil)))
              (progn
                (maybe-change-heading snake)
                (when (>= (incf frame-counter) (/ 60 frame-speed))
                  (setf frame-counter 0)
                  (update-trail snake)
                  (case (check-collisions snake food)
                    (:die (setf diedp t))
                    (:eat (progn (setf (should-grow snake) t)
                                 (new-pos food snake))))
                  (with-drawing (:bgcolor +black+)
                    (draw-object food)
                    (draw-snake snake))))))))))
