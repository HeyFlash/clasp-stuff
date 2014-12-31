;;; This file defines a minor mode that can be used to quickly
;;; create clbind definitions from cpp header files.
;;; It is far from perfect and will likely break with many
;;; cpp libraries.
;;; Edit the keymap at the end of the file, evaluate the file
;;; and activate cpp2clbind-mode.

(defvar cpp2clb/current-namespace "sf"
  "Always used as the current namespace.")

(defvar cpp2clb/current-class nil
  "Used as the name of the current class.
If nil, the name of the class is tried to be autodetected.")

(defvar cpp2clb/static-class-delimiter "/"
  "The delimiter between class name and function name,
used when static member functions are translated.")

(defconst cpp2clb/string-literal-prefix "R"
  "The prefix for the string literal of the docstring.")

(defconst cpp2clb/string-literal-start-delimiter "\"**("
  "The start delimiter for the string literal of the docstring.")

(defconst cpp2clb/string-literal-end-delimiter ")**\""
  "The end delimiter for the string literal of the docstring.")

(defconst cpp2clb/no-comment-regexp "^[^/]*$"
  "The regexp that is used to distinguish lines that are
not comments")

;; (defconst cpp2clb/remove-line-from-comment-regexp "^\\s-*////+$"
;;   "The regexp that is used to remove entire lines from comments.")

(defconst cpp2clb/remove-from-comment-regexp "///*"
  "The regexp that is used to delete matches from comments.")

(defconst cpp2clb/function-name-regexp "\s+\\sw+("
  "The regexp that is used to detect the name of a function")

(defconst cpp2clb/class-regexp "\s*class\s+"
  "The regexp that is used to find the current class")

(defconst cpp2clb/api-def-regexp
  "\\(SFML_SYSTEM_API\\|SFML_WINDOW_API\\|SFML_GRAPHICS_API\\|SFML_AUDIO_API\\|SFML_NETWORK_API\\)"
  "The regexp that is used to detect stuff that comes between
the class keyword and the actual name of the class.")

(defun cpp2clb/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

(defun cpp2clb/get-text-between-regexps (first-regexp second-regexp)
  "Returns the text between the matches for first-regexp and 
second-regexp."
  (save-excursion
    (re-search-backward  first-regexp)
    (forward-char)
    (set-mark (point))
    (re-search-forward second-regexp)
    (buffer-substring-no-properties (mark) (point))))

(defun cpp2clb/get-function-string-at-point ()
  "Returns the whole string representing an SFML function,            
including the doxygen documentation"
  (cpp2clb/get-text-between-regexps "^$" "^$"))

(defun cpp2clb/get-function-definition-position (function-with-doxy)
  "Gets the position where the real function definition starts,
i.e. where the comment ends."
  (string-match cpp2clb/no-comment-regexp function-with-doxy 1))

(defun cpp2clb/get-function-definition (function-with-doxy)
  "Extracts the function definition from function-with-doxy"
  (string-match cpp2clb/no-comment-regexp function-with-doxy 1)
  (cpp2clb/chomp (match-string 0 function-with-doxy)))

(defun cpp2clb/get-function-definition-at-point ()
  "Returns the function definition at the current point,
even if point is inside the doxy string"
  (cpp2clb/get-function-definition
   (cpp2clb/get-function-string-at-point)))

(defun cpp2clb/get-doxystring (function-with-doxy)
  "Extracts the doxystring from function-with-doxy"
  (substring function-with-doxy
   0 (cpp2clb/get-function-definition-position function-with-doxy)))

(defun cpp2clb/get-doxystring-at-point ()
  "Returns the doxystring belonging to the function at point,
even if point inside the function definition"
  (cpp2clb/get-doxystring (cpp2clb/get-function-string-at-point)))

(defun cpp2clb/get-documentation-string-at-point ()
  "Returns a documentation string for the function at point, 
i.e. a string where all the documentation syntax (slashes)
has been removed."
  (cpp2clb/uncomment-string (cpp2clb/get-doxystring-at-point)))

(defun cpp2clb/uncomment-string (string)
  "Removes the everything that matches cpp2clb/remove-from-comment-regexp
 from a string. Starting and trailing whitespaces are removed as well."
  (cpp2clb/chomp
   (replace-regexp-in-string
    cpp2clb/remove-from-comment-regexp "" string)))

(defun cpp2clb/get-function-name (function-definition-string)
  "Extracts the name of the function from the 
function-definition-string"
  (string-match cpp2clb/function-name-regexp
		function-definition-string)
  (cpp2clb/chomp
   (replace-regexp-in-string
    "(" "" 
    (match-string 0 function-definition-string))))

(defun cpp2clb/get-function-name-at-point ()
  "Get the name of the function at point,
even if point is in the doxystring."
  (cpp2clb/get-function-name
   (cpp2clb/get-function-definition-at-point)))

(defun cpp2clb/lispify-helper (string)
  "Downcases string and prepends a -"
  (concat "-" (downcase string)))

(defun cpp2clb/lispify-name (function-name)
  "Lispifies the name of a function, 
i.e. replaces camel casing with dashes."
  (let ((case-fold-search nil)
	(fn-dc
	 (concat (downcase (substring function-name 0 1))
		 (substring function-name 1))))
    (replace-regexp-in-string
     "[A-Z]" 'cpp2clb/lispify-helper fn-dc t nil nil 0)))

(defun cpp2clb/get-lispified-function-name-at-point ()
  "Returns the lispified name of the function found at point,
even if point is in the doxystring."
  (cpp2clb/lispify-name
   (cpp2clb/get-function-name
    (cpp2clb/get-function-definition-at-point))))

(defun cpp2clb/find-current-class-name ()
  "Return the name of the current class"
  (save-excursion
    (re-search-backward
     (concat cpp2clb/class-regexp cpp2clb/api-def-regexp))
    (re-search-forward
     (concat cpp2clb/class-regexp cpp2clb/api-def-regexp))
    (set-mark (point))
    (forward-word)
    (cpp2clb/chomp
     (buffer-substring-no-properties (mark) (point)))))

(defun cpp2clb/get-current-class-name ()
  "Returns the current classname. If variable cpp2clb/current-class
is nil, tries to autodetect, otherwise returns the value 
of that variable."
  (if cpp2clb/current-class
      cpp2clb/current-class
    (cpp2clb/find-current-class-name)))

(defun cpp2clb/create-clbind-member-def-from-point ()
  "Returns the .def that suits the function definition found at point"
  (concat
   ".def(\""
   (cpp2clb/get-lispified-function-name-at-point)
   "\", &"
   cpp2clb/current-namespace
   "::"
   (cpp2clb/get-current-class-name)
   "::"
   (cpp2clb/get-function-name-at-point)
   ",\npolicies<>(), \"\", \"\",\n"
   cpp2clb/string-literal-prefix
   cpp2clb/string-literal-start-delimiter
   (cpp2clb/get-documentation-string-at-point)
   cpp2clb/string-literal-end-delimiter
   ")"))

(defun cpp2clb/create-clbind-static-def-from-point ()
  "Returns the .def that suits the function definition found at point"
  (concat
   ",def(\""
   (cpp2clb/lispify-name (cpp2clb/get-current-class-name))
   cpp2clb/static-class-delimiter
   (cpp2clb/get-lispified-function-name-at-point)
   "\", &"
   cpp2clb/current-namespace
   "::"
   (cpp2clb/get-current-class-name)
   "::"
   (cpp2clb/get-function-name-at-point)
   ",\npolicies<>(), \"\", \"\",\n"
   cpp2clb/string-literal-prefix
   cpp2clb/string-literal-start-delimiter
   (cpp2clb/get-documentation-string-at-point)
   cpp2clb/string-literal-end-delimiter
   ")"))

(defun cpp2clb/execute-function-previous-window (function)
  "Executes a given function in the previous window at its point,
  returning the point to the current buffer."
  (let ((otbuf (other-buffer (current-buffer) t))
	(curbuf (current-buffer)))
    (pop-to-buffer otbuf)
    (funcall function)
    (pop-to-buffer curbuf)))

(defun cpp2clb/insert-clbind-member-def-previous-window ()
  "Inserts the clbind def created from the current function 
into the previous window, at its point."
  (interactive)
  (let ((clbind-def (cpp2clb/create-clbind-member-def-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (+ 5 (current-column)))
	      (start (point)))
	  (insert clbind-def)
	  (indent-region start (point) indcolumn)
	  (newline)
	  (newline)
	  (c-indent-line-or-region))))))


(defun cpp2clb/insert-clbind-static-def-previous-window ()
  "Inserts the clbind def created from the current function 
into the previous window, at its point."
  (interactive)
  (let ((clbind-def (cpp2clb/create-clbind-static-def-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (+ 5 (current-column)))
	      (start (point)))
	  (insert clbind-def)
	  (indent-region start (point) indcolumn)
	  (newline)
	  (newline)
	  (c-indent-line-or-region))))))


(define-minor-mode cpp2clbind-mode
  "A minor mode for quickly creating clbind definitions
from cpp header file definitions."
  nil " CPP2CLB" 
  (let ((cpp2clbind-mode-map (make-sparse-keymap)))
    (define-key
      cpp2clbind-mode-map
      (kbd "s-b")
      'cpp2clb/insert-clbind-member-def-previous-window)
    (define-key
      cpp2clbind-mode-map
      (kbd "s-n")
      'cpp2clb/insert-clbind-static-def-previous-window)
    cpp2clbind-mode-map))

(add-hook 'c++-mode-hook 'cpp2clbind-mode)
