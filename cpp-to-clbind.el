;;; This file defines a minor mode that can be used to quickly
;;; create clbind definitions from cpp header files.
;;; It is far from perfect and will likely break with many
;;; cpp libraries.
;;; Edit the keymap at the end of the file, evaluate the file
;;; and activate cpp2clbind-mode.

(defconst cpp2clb/current-namespace "sf"
  "Always used as the current namespace.")

(defconst cpp2clb/current-class nil
  "Used as the name of the current class.
If nil, the name of the class is tried to be autodetected.")

(defconst cpp2clb/current-package "SFML"
  "Used as the CL package in which to intern things.")

(defconst cpp2clb/static-class-delimiter "/"
  "The delimiter between class name and function name,
used when static member functions are translated.")

(defconst cpp2clb/enum-class-delimiter "/"
  "The delimiter between class name and enum name, 
used when enums are translated to symbols.")

(defconst cpp2clb/enum-value-delimiter "/"
  "The delimiter between enum name and enum value, 
used when enums are translated to symbols.")

(defconst cpp2clb/string-literal-prefix "R"
  "The prefix for the string literal of the docstring.")

(defconst cpp2clb/string-literal-start-delimiter "\"**("
  "The start delimiter for the string literal of the docstring.")

(defconst cpp2clb/string-literal-end-delimiter ")**\""
  "The end delimiter for the string literal of the docstring.")

(defconst cpp2clb/no-comment-regexp "^[^/]*$"
  "The regexp that is used to distinguish lines that are
not comments.")

;; (defconst cpp2clb/remove-line-from-comment-regexp "^\\s-*////+$"
;;   "The regexp that is used to remove entire lines from comments.")

(defconst cpp2clb/remove-from-comment-regexp "///*"
  "The regexp that is used to delete matches from comments.")

(defconst cpp2clb/function-name-regexp "\\sw+("
  "The regexp that is used to detect the name of a function.")

(defconst cpp2clb/class-regexp "\s*\\(class\\|struct\\)\s+"
  "The regexp that is used to find the current class.")

(defconst cpp2clb/enum-regexp "\s*enum\s+"
  "The regexp that is used to find the current enum.")

(defconst cpp2clb/api-def-regexp
  "\\(SFML_SYSTEM_API\\|SFML_WINDOW_API\\|SFML_GRAPHICS_API\\|SFML_AUDIO_API\\|SFML_NETWORK_API\\)"
  "The regexp that is used to detect stuff that comes between
the class keyword and the actual name of the class.")

(defconst cpp2clb/api-def-regexp-complete
  "\\(^SFML_SYSTEM_API$\\|^SFML_WINDOW_API$\\|^SFML_GRAPHICS_API$\\|^SFML_AUDIO_API$\\|^SFML_NETWORK_API$\\)"
  "The regexp that is used to check whether a string is a api def.")

(defconst cpp2clb/function-keywords
  "\\(^virtual$\\|^static$\\|^public$\\|^private$\\)")

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

(defun cpp2clb/get-complete-definition-at-point ()
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
   (cpp2clb/get-complete-definition-at-point)))

(defun cpp2clb/get-brace-content (string open-brace close-brace)
  "Get the content between them matches of open-brace and close-brace
in string."
  (substring string
	     (+ (string-match open-brace string) (length open-brace))
	     (string-match close-brace string)))

(defun cpp2clb/get-lines-as-list (string)
  "Get the lines of the string as a list"
  (split-string (cpp2clb/chomp string) "$" t))

(defun cpp2clb/get-enum-member-from-line (line)
  "Get the enum member from a single line, if it is at start of line."
  (car (split-string (cpp2clb/chomp line)
		     "[,\|\s-]")))

(defun cpp2clb/get-doxystring (function-with-doxy)
  "Extracts the doxystring from function-with-doxy"
  (substring function-with-doxy
	     0 (cpp2clb/get-function-definition-position function-with-doxy)))

(defun cpp2clb/get-non-doxystring (thing-with-doxy)
  "Extracts everything beyond the doxystring from thing-with-doxy"
  (substring thing-with-doxy
	     (cpp2clb/get-function-definition-position thing-with-doxy)))

(defun cpp2clb/get-doxystring-at-point ()
  "Returns the doxystring belonging to the function at point,
even if point inside the function definition"
  (cpp2clb/get-doxystring (cpp2clb/get-complete-definition-at-point)))

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

(defun cpp2clb/find-current-environment-name
    (environment-regexp api-regexp)
  "Return the name of the current environment, environment
being e.g. class, struct, enum,..."
  (save-excursion
    (re-search-backward
     (concat environment-regexp api-regexp))
    (re-search-forward
     (concat environment-regexp api-regexp))
    (set-mark (point))
    (forward-word)
    (cpp2clb/chomp
     (buffer-substring-no-properties (mark) (point)))))

(defun cpp2clb/find-current-class-name ()
  "Return the name of the current class"
  (cpp2clb/find-current-environment-name
   cpp2clb/class-regexp cpp2clb/api-def-regexp))

(defun cpp2clb/find-current-enum-name ()
  "Return the name of the current enum"
  (cpp2clb/find-current-environment-name
   cpp2clb/enum-regexp ""))


(defun cpp2clb/get-current-class-name ()
  "Returns the current classname. If variable cpp2clb/current-class
is nil, tries to autodetect, otherwise returns the value 
of that variable."
  (if cpp2clb/current-class
      cpp2clb/current-class
    (cpp2clb/find-current-class-name)))

(defun cpp2clb/get-parameter-list-from-point ()
  "Returns a list of all the parameters for the function at point.
The list contains types and names of the parameters as found in code."
  (mapcar
   'cpp2clb/chomp
   (split-string
    (cpp2clb/get-brace-content
     (cpp2clb/get-function-definition-at-point) "(" ")")
    "," t)))

(defun cpp2clb/allowed-name (parameter)
  (not
   (or (string-match cpp2clb/function-keywords parameter)
       (string-match cpp2clb/api-def-regexp-complete parameter))))

(defun cpp2clb/extract-parameter-type (parameter)
  "Extracts the parameter type if parameter is both type and name 
and might contain a default."
  (let* ((para-list (split-string
		     (substring  parameter 0
				 (string-match "=" parameter))))
	 (ret-string "")
	 (last (nth (- (length para-list) 1) para-list)))
    (dotimes (i (- (length para-list) 1))
      (when (cpp2clb/allowed-name (nth i para-list))
	(setq ret-string
	      (concat ret-string (nth i para-list) " "))))
    (when (char-equal (elt last 0) ?*)
      (setq ret-string (concat ret-string "*")))
    (when (char-equal (elt last 0) ?&)
      (setq ret-string (concat ret-string "&")))
    (cpp2clb/chomp ret-string)))

(defun cpp2clb/get-parameter-types-from-point ()
  "Returns a list of all the parameter types for the function at point."
  (mapcar
   'cpp2clb/extract-parameter-type
   (cpp2clb/get-parameter-list-from-point)))

(defun cpp2clb/get-return-type-from-point ()
  "Returns the return type of the function at point."
  (cpp2clb/extract-parameter-type
   (substring
    (cpp2clb/get-function-definition-at-point)
    0
    (string-match
     "("
     (cpp2clb/get-function-definition-at-point)))))

(defun cpp2clb/get-function-cast-from-point (non-static)
  "Returns a cast to the function at point."
  (concat
   "("
   (cpp2clb/get-return-type-from-point)
   " ("
   (when non-static
     (concat
      cpp2clb/current-namespace
      "::"
      (cpp2clb/get-current-class-name)
      "::"))
   "*)("
   (mapconcat 'identity
	      (cpp2clb/get-parameter-types-from-point) ", ")
   "))"))

(defun cpp2clb/create-clbind-member-def-from-point
    (&optional overloaded)
  "Returns the .def that suits the function definition found at point.
If overloaded is non-nil, a function-pointer cast fitting for 
overloaded member functions is included in the .def."
  (concat
   ".def(\""
   (cpp2clb/get-lispified-function-name-at-point)
   "\", "
   (when overloaded
     (cpp2clb/get-function-cast-from-point t))
   "&"
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

(defun cpp2clb/create-clbind-static-def-from-point
    (&optional overloaded)
  "Returns the .def that suits the function definition found at point"
  (concat
   ",def(\""
   (cpp2clb/lispify-name (cpp2clb/get-current-class-name))
   cpp2clb/static-class-delimiter
   (cpp2clb/get-lispified-function-name-at-point)
   "\", "
   (when overloaded
     (cpp2clb/get-function-cast-from-point nil))
   "&"
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

(defun cpp2clb/create-clbind-constructor-def-from-point ()
  "Returns the .defconstructor that suits the constructor definition
found at point."
  (concat
   ".def_constructor(\""
   (cpp2clb/get-lispified-function-name-at-point)
   "\", constructor<"
   (mapconcat 'identity 
	      (cpp2clb/get-parameter-types-from-point) ", ")
   ">()"
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

(defun cpp2clb/insert-clbind-member-def-previous-window (overloaded)
  "Inserts the clbind def created from the current function 
into the previous window, at its point.
If the prefix arg overloaded is non-nil, a version including
the function pointer cast for an overloaded method is inserted."
  (interactive "P")
  (let ((clbind-def
	 (cpp2clb/create-clbind-member-def-from-point overloaded)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (+ 5 (current-column)))
	      (start (point)))
	  (insert clbind-def)
	  (indent-region start (point) indcolumn)
	  (newline)
	  (newline)
	  (c-indent-line-or-region))))))


(defun cpp2clb/insert-clbind-static-def-previous-window (overloaded)
  "Inserts the clbind def created from the current function 
into the previous window, at its point.
If the prefix arg overloaded is non-nil, a version including
the function pointer cast for an overloaded method is inserted."
  (interactive "P")
  (let ((clbind-def
	 (cpp2clb/create-clbind-static-def-from-point overloaded)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (+ 5 (current-column)))
	      (start (point)))
	  (insert clbind-def)
	  (indent-region start (point) indcolumn)
	  (newline)
	  (newline)
	  (c-indent-line-or-region))))))


(defun cpp2clb/insert-clbind-constructor-def-previous-window ()
  "Inserts the clbind def created from the current function 
into the previous window, at its point."
  (interactive)
  (let ((clbind-def (cpp2clb/create-clbind-constructor-def-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (+ 5 (current-column)))
	      (start (point)))
	  (insert clbind-def)
	  (indent-region start (point) indcolumn)
	  (newline)
	  (newline)
	  (c-indent-line-or-region))))))

(defun cpp2clb/get-current-enum-mapper-string ()
    "Returns a string that is the enum mapper for the current enum"
  (concat
   "*"
   (upcase (cpp2clb/find-current-class-name)) "-"
   (upcase (cpp2clb/find-current-enum-name))
   "-ENUM-MAPPER*"))

(defun cpp2clb/get-full-enum-designator ()
  "Returns the full enum designator found at point"
  (concat 
   cpp2clb/current-namespace
   "::"
   (cpp2clb/find-current-class-name)
   "::"
   (cpp2clb/find-current-enum-name)))

(defun cpp2clb/create-clbind-enum-def-from-point ()
  "Returns the enum definition that suits the enum definition found at point"
  (let ((return-string 
	 (concat
	  ".enum_<"
	  (cpp2clb/get-full-enum-designator)
	  ">(core::lisp_intern(\""
	  cpp2clb/current-package
	  "\", \""
	  (cpp2clb/get-current-enum-mapper-string)
	  "\"))\n[\n")))
    (dolist
	(enum-def
	 (cpp2clb/get-lines-as-list
	  (cpp2clb/get-brace-content
	   (cpp2clb/get-non-doxystring
	    (cpp2clb/get-complete-definition-at-point))
	   "{" "}")))
      (setq
       return-string
       (concat
	return-string
	"  value(\""
	(cpp2clb/lispify-name (cpp2clb/find-current-class-name))
	cpp2clb/enum-class-delimiter
	(cpp2clb/lispify-name (cpp2clb/find-current-enum-name))
	cpp2clb/enum-value-delimiter
	(cpp2clb/lispify-name
	 (cpp2clb/get-enum-member-from-line enum-def))
	"\", "
	(cpp2clb/get-full-enum-designator)
	"::"
	(cpp2clb/get-enum-member-from-line enum-def)
	"),\n")))
    (setq
     return-string
     (concat
      (substring return-string 0 (- (length return-string) 2))
      "\n]\n"))
    return-string))

(defun cpp2clb/insert-clbind-enum-def-previous-window ()
  "Inserts the clbind def created from the current function 
into the previous window, at its point."
  (interactive)
  (let ((clbind-def (cpp2clb/create-clbind-enum-def-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
  	(let ((indcolumn (current-column))
  	      (start (point)))
  	  (insert clbind-def)
  	  (indent-region start (point) indcolumn)
  	  (newline)
  	  (c-indent-line-or-region))))))


(defun cpp2clb/create-clbind-enum-translators-from-point ()
  "Create enum translators fitting for the enum at point."
  (concat
   "template <>
  struct to_object<"
   (cpp2clb/get-full-enum-designator)
   ">
  {
    static core::T_sp convert("
   (cpp2clb/get-full-enum-designator)
   " v)
    {
      core::Symbol_sp converterSym = 
        core::lisp_intern(\""
   cpp2clb/current-package
   "\",\""
   (cpp2clb/get-current-enum-mapper-string)
   "\");
      core::SymbolToEnumConverter_sp converter = 
        converterSym->symbolValue().as<core::SymbolToEnumConverter_O>();
      return converter->symbolForEnum<"
   (cpp2clb/get-full-enum-designator)
   ">(v);
    }
  };

  template <>
  struct from_object<"
   (cpp2clb/get-full-enum-designator)
   ">
  {
    typedef "
   (cpp2clb/get-full-enum-designator)
   " DeclareType;
    DeclareType _v;
    from_object(core::T_sp object)
    {
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) 
      {
	if (sym.notnilp()) 
        {
	  core::Symbol_sp converterSym = core::lisp_intern(\""
   cpp2clb/current-package
   "\",\""
   (cpp2clb/get-current-enum-mapper-string)
   "\");
	  core::SymbolToEnumConverter_sp converter = 
            converterSym->symbolValue().as<core::SymbolToEnumConverter_O>();
	  this->_v = converter->enumForSymbol<"
   (cpp2clb/get-full-enum-designator)
   ">(sym);
	  return;
	}
      }
      SIMPLE_ERROR(BF(\"Cannot convert object %s to "
   (cpp2clb/get-full-enum-designator)
   "\") % _rep_(object) );
    }
  };

  "))

(defun cpp2clb/insert-clbind-enum-translators-previous-window ()
  "Insert enum translators fitting for the enum at point
into the previous window."
  (interactive)
  (let ((clbind-def
	 (cpp2clb/create-clbind-enum-translators-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(insert clbind-def)))))


(defun cpp2clb/create-clbind-class-def-from-point ()
  "Creates a clbind class_ from the current class."
  (concat
   ",class_<"
   cpp2clb/current-namespace
   "::"
   (cpp2clb/find-current-class-name)
   ">(\""
   (cpp2clb/lispify-name (cpp2clb/find-current-class-name))
   "\")\n\n"))


(defun cpp2clb/insert-clbind-class-def-previous-window ()
  "Inserts the clbind class_ def created from the current class
into the previous window, at its point."
  (interactive)
  (let ((clbind-def (cpp2clb/create-clbind-class-def-from-point)))
    (cpp2clb/execute-function-previous-window
     '(lambda ()
	(let ((indcolumn (current-column)))
	  (insert clbind-def)
	  (indent-line-to indcolumn))))))


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
    (define-key
      cpp2clbind-mode-map
      (kbd "s-m")
      'cpp2clb/insert-clbind-enum-def-previous-window)
    (define-key
      cpp2clbind-mode-map
      (kbd "s-v")
      'cpp2clb/insert-clbind-enum-translators-previous-window)
    (define-key
      cpp2clbind-mode-map
      (kbd "s-c")
      'cpp2clb/insert-clbind-class-def-previous-window)
    cpp2clbind-mode-map))

(add-hook 'c++-mode-hook 'cpp2clbind-mode)
