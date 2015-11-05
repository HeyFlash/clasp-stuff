
;; this should be the path where the sfml libraries are located.
(setf *DEFAULT-PATHNAME-DEFAULTS* #P"~/dev/sfml/clasp-sfml-build/lib/")

;; this should be the path where the graphics and the music file
;; referenced below are located.
(ext:chdir #P"~/dev/clasp-stuff/resources")

(defun load-sfml ()
  "Load all available SFML libraries."
  (defmethod cleavir-environment::symbol-macro-expansion (sym (env core:environment))
    (cleavir-environment::symbol-macro-expansion sym nil))
  (defmethod cleavir-environment::macro-function (sym (env core:environment))
    (cleavir-environment::macro-function sym nil))
  (load "libclasp-sfml-window.so")
  (load "libclasp-sfml-graphics.so")
  (load "libclasp-sfml-audio.so"))

(load-sfml)

(let
    ((window (sf:make-render-window-vs '(600 400 32) "This is a clasp SFML Window"))
     (event (sf:make-event))
     (texture (sf:make-texture))
     (music (sf:make-music))
     (sprite nil)
     (event-type nil))
  (sf:load-from-file texture "jetfighter.png" nil)
  (setf sprite (sf:make-sprite texture))
  (sf:open-from-file music "Medieval Rondo.ogg")
  (sf:play music)
  (loop while (sf:is-open window) do
       (loop while (sf:poll-event window event) do
	    (setf event-type (sf:type event) ;(sf:get-type event) 		
		  )
	    (cond
	      ((eq event-type 'CLOSED) 
	       (sf:close window))
	      ((eq event-type 'KEY-PRESSED)
	       (cond
	       	 ((eq (sf:code (sf:key event)) 'keyboard-key/a)
	       	  (sf:rotate sprite 5))
	       	 ((eq (sf:code (sf:key event)) 'keyboard-key/d)
	       	  (sf:rotate sprite -5))
		 ((eq (sf:code (sf:key event)) 'keyboard-key/up)
		  (sf:move sprite '(0 5)))
		 ((eq (sf:code (sf:key event)) 'keyboard-key/down)
		  (sf:move sprite '(0 -5)))
		 ((eq (sf:code (sf:key event)) 'keyboard-key/left)
		  (sf:move sprite '(-5 0)))
		 ((eq (sf:code (sf:key event)) 'keyboard-key/right)
		  (sf:move sprite '(5 0))))
	       )))
       (sf:clear window '(0 0 0 255))
       (sf:draw window sprite)
       (sf:display window))
  (sf:stop music)
  )
