;; this should be the path where the sfml libraries are located.
(setf *DEFAULT-PATHNAME-DEFAULTS* #P"~/dev/sfml/clasp-sfml-build/lib/boehm/release/")

(defvar *main-window* nil "The main window")

(defun load-sfml ()
  "Load all available SFML libraries."
  (load "libclasp-sfml-window.so")
  (load "libclasp-sfml-graphics.so")
  (load "libclasp-sfml-audio.so"))

(load-sfml)

(defgeneric draw (draw-target to-draw)
  (:documentation "Draw the GAME-OBJECT onto the screen"))

(defgeneric collide (go1 go2)
  (:documentation "Check whether GO1 and GO2 collide and perform the appropriate action."))

(defgeneric move (to-move)
  (:documentation "Move TO-MOVE by the specified MOVE-VECTOR"))

(defclass drawable-object ()
    ((drawable :initarg drawable
	       :accessor drawable)))

(defclass bounding-box-collider () ())

(defclass game-object (drawable-object bounding-box-collider) ())

(defclass movable-object (game-object)
  ((move-vector :initarg move-vector
		:accessor move-vector
		:initform '(0.0 0.0))))

(defclass paddle (movable-object)
  ((drawable
    :initform
    (sf:make-rectangle-shape '(80.0 10.0)))))

(defclass ball (movable-object)
  ((drawable
    :initform
    (sf:make-circle-shape 6 30))
   (move-vector :initform '(5.0 5.0))))

(defclass block (movable-object) ())

(defclass border (movable-object) ())

(defclass horizontal-border (border)
  ((drawable
     :initform
     (sf:make-rectangle-shape '(1024.0 5.0)))))

(defclass vertical-border (border)
  ((drawable
     :initform
     (sf:make-rectangle-shape '(5.0 786.0)))))

(defun about-equal (n1 n2)
  (< (abs (- n1 n2)) 0.001))

(defmethod draw (draw-target (to-draw drawable-object))
  (sf:draw draw-target (slot-value to-draw 'drawable)))

(defmethod move ((to-move movable-object))
  (sf:move (slot-value to-move 'drawable) (slot-value to-move 'move-vector)))

(defmethod move :after ((to-move paddle))
  (setf (move-vector to-move) '(0.0 0.0)))


(defmethod collide ((go1 game-object) (go2 game-object))
  (sf:float-rect-intersects
   (sf:get-global-bounds (drawable go1))
   (sf:get-global-bounds (drawable go2))))


;; (defmethod collide ((go1 game-object) (go2 game-object))
;;   (let* ((intersection-area (sf:make-float-rect 0.0 0.0 0.0 0.0))
;; 	 (intersects 
;; 	  (sf:float-rect-intersects-area
;; 	   (sf:get-global-bounds (drawable go1))
;; 	   (sf:get-global-bounds (drawable go2))
;; 	   intersection-area)))
;;     (values intersects intersection-area)))

;; (defmethod collide :around ((go1 movable-object) (go2 movable-object))
;;   (let* ((intersects nil)
;; 	 (intersection-area nil)
;; 	 (go1rect (sf:get-global-bounds (drawable go1)))
;; 	 (go2rect (sf:get-global-bounds (drawable go2))))
;;     (multiple-value-bind (intersects intersection-area) (call-next-method))
;;     (when intersects
;;       (cond ((about-equal (sf:float-rect-width intersection-area)
;; 			  (sf:float-rect-width go1rect))
;; 	     (setf (cadr (move-vector go1)) (* -1 (cadr (move-vector go1)))))
;; 	    ((eq (sf:float-rect-height intersection-area)
;; 		 (sf:float-rect-height go1rect))
;; 	     (setf (car (move-vector go1)) (* -1 (car (move-vector go1)))))
;; 	    ((about-equal (sf:float-rect-width intersection-area)
;; 			  (sf:float-rect-width go2rect))
;; 	     (setf (cadr (move-vector go2)) (* -1 (cadr (move-vector go2)))))
;; 	    ((eq (sf:float-rect-height intersection-area)
;; 		 (sf:float-rect-height go2rect))
;; 	     (setf (cadr (move-vector go2)) (* -1 (cadr (move-vector go2)))))))))

(defmethod collide :around ((go1 ball) (go2 paddle))
  (when (call-next-method)
    (setf  (cadr (move-vector go1)) (* -1 (cadr (move-vector go1))))))

(defmethod collide :around ((go1 ball) (go2 vertical-border))
  (when (call-next-method)
    (setf  (car (move-vector go1)) (* -1 (car (move-vector go1))))))

(defmethod collide :around ((go1 ball) (go2 horizontal-border))
  (when (call-next-method)
    (setf  (cadr (move-vector go1)) (* -1 (cadr (move-vector go1))))))

(defun run-game ()
  "Run the game"
  (setf *main-window* (sf:make-render-window-vs '(1024 768 32) "Claspnoid"))
  (sf:set-vertical-sync-enabled *main-window* t)
  (let ((event (sf:make-event))
	(event-type nil)
	(ball (make-instance 'ball))
	(paddle (make-instance 'paddle))
	(top-border (make-instance 'horizontal-border))
	(bottom-border (make-instance 'horizontal-border))
	(left-border (make-instance 'vertical-border))
	(right-border (make-instance 'vertical-border))
	(game-objects nil))
    (sf:set-position (slot-value bottom-border 'drawable) '(0.0 763.0))
    (sf:set-position (slot-value right-border 'drawable) '(1019.0 0.0))
    (sf:set-origin (slot-value ball 'drawable) '(3.0 3.0))
    (sf:set-origin (slot-value paddle 'drawable) '(40.0 5.0))
    (sf:set-position (slot-value ball 'drawable) '(20.0 20.0))
    (sf:set-position (slot-value  paddle 'drawable) '(512.0 740.0))
    (setf game-objects
	  (list ball paddle
		top-border bottom-border
		left-border right-border))
    (loop while (sf:is-open *main-window*) do
    	 (loop while (sf:poll-event *main-window* event) do
    	      (setf event-type (sf:type event))
    	      (cond
    		((eq event-type 'CLOSED) 
    		 (sf:close *main-window*))))
    	 (when (sf::keyboard/is-key-pressed 'keyboard-key/left)
    	   (setf (move-vector paddle) '(-10.0 0.0)))
    	 (when (sf::keyboard/is-key-pressed 'keyboard-key/right)
    	   (setf (move-vector paddle) '(10.0 0.0)))
	 ;(mapcar game-objects #'move)
	 (loop for o1 in game-objects do
	      (move o1))
	 (loop for (o1 . orest) on game-objects do
	      (loop for o2 in orest do
		   (collide o1 o2)))
	 ;(collide paddle ball)
    	 (sf:clear *main-window* '(0 0 0 255))
    	 (draw *main-window* ball)
    	 (draw *main-window* paddle)
	 (sf:display *main-window*))))
