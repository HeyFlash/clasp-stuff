
(defun load-sfml ()
  "Load all available SFML libraries."
  (core:load-bundle "libsfml-window.so")
  (core:load-bundle "libsfml-graphics.so"))

;; (core:load-bundle "libsfml-audio.so")
;; (core:load-bundle "libsfml-network.so")
;; (core:load-bundle "libsfml-system.so")

(let
    ((window (sf:make-render-window '(600 400 32) "This is a clasp SFML Window"))
     (event (sf:make-event))
     (texture (sf:make-texture))
     (sprite nil))
  (sf:load-from-file texture "merry.jpg" nil)
  (setf sprite (sf:make-sprite texture))
  (do ()
      ((not (sf:is-open window)))
    (do ()
	((not (sf:poll-event window event)))
      (let ((event-type (sf:get-type event)))
	(cond
	  ((eq event-type 'CLOSED) 
	   (sf:close window)))))
    (sf:clear window '(0 0 0 255))
    (sf:draw window sprite)
    (sf:display window)))
