
;; this should be the path where the sfml libraries are located.
(setf *DEFAULT-PATHNAME-DEFAULTS* #P"~/dev/sfml-build/lib/")

;; this should be the path where the graphics and the music file
;; referenced below are located.
(ext:chdir #P"~/dev/clasp-stuff/resources")

(defun load-sfml ()
  "Load all available SFML libraries."
  (defmethod cleavir-environment::symbol-macro-expansion (sym (env core:environment))
    (cleavir-environment::symbol-macro-expansion sym nil))
  (defmethod cleavir-environment::macro-function (sym (env core:environment))
    (cleavir-environment::macro-function sym nil))
  (load "libsfml-window.so")
  (load "libsfml-graphics.so")
  (load "libsfml-audio.so"))

(load-sfml)

(let
    ((window (sf:make-render-window-vs '(600 400 32) "This is a clasp SFML Window"))
     (event (sf:make-event))
     (texture (sf:make-texture))
     (music (sf:make-music))
     (sprite nil)
     (event-type nil))
  (sf:load-from-file texture "clasp1.jpg" nil)
  (setf sprite (sf:make-sprite texture))
  (sf:open-from-file music "Medieval Rondo.ogg")
  (sf:play music)
  (loop while (sf:is-open window) do
       (loop while (sf:poll-event window event) do
	    (setf event-type (sf:get-type event))
	    (cond
	      ((eq event-type 'CLOSED) 
	       (sf:close window))))
       (sf:clear window '(0 0 0 255))
       (sf:draw window sprite)
       (sf:display window))
  (sf:stop music))
