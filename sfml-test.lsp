
(core:load-bundle "libsfml-window.so")
(core:load-bundle "libsfml-graphics.so")


;; (core:load-bundle "libsfml-audio.so")
;; (core:load-bundle "libsfml-network.so")
;; (core:load-bundle "libsfml-system.so")


(setf window (sf:make-render-window '(800 600 32) "This is a clasp SFML Window"))


(let
    ((window (sf:make-render-window '(800 600 32) "This is a clasp SFML Window"))
     (event nil))
  (do ()
      ((not (sf:is-open window)))
    (format t "getting event...\n")
    (do ()
	((not (sf:poll-event window event)))
      (let ((event-type (sf:get-type event)))
	(cond
	  ((eq 'event-type 'Closed) 
	   (sf:close window)))))
    (sf:clear window '(0 0 0 255))))
