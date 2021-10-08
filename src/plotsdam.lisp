(in-package #:plotsdam)

(deftype plotting-mode ()
  '(member :immediate :http))

(defparameter *plotting-mode* ':http
  "The current plotting mode.")

(defun plistp (obj &key keywords-only)
  "Check if OBJ is a property list."
  (and (listp obj)
       (loop :for (key val) :on obj :by #'cddr
	     :unless (if keywords-only
			 (keywordp key)
			 (symbolp key))
	       :do (return nil)
	     :finally (return t))))

(defun json-keyword-p (obj)
  "Return true if obj is a symbol with print name false, true, or null
   and NOT in the Lisp keyword package. Otherwise, return false."
  (and (symbolp obj)
       (not (keywordp obj))
       (member (symbol-name obj) 
               '(false true null)
               :test #'string-equal)))

(defun lisp-keywordify (symbol)
  "Return the result of taking the print name of symbol and interning
   it in the Lisp keyword package."
  (intern (symbol-name symbol) :keyword))
               

(defun translate (obj)
  "Translate OBJ to a format suitable for JSON serialization."
  ;; this is mainly so that we can handle plists so that
  ;; they generate objects, and also we turn false/true/null
  ;; (as symbols in any package but keyword) into keyword
  ;; equivalents, handled by our custom cl-json:encode-json
  ;; methods (below)
  (cond ((stringp obj) obj)
        ((json-keyword-p obj)
         (lisp-keywordify obj))
	((keywordp obj)
	 (if (every #'upper-case-p (symbol-name obj))
	     (string-downcase (symbol-name obj))
	     (symbol-name obj)))
	((and (listp obj)
	      (eq 'val (first obj)))
	 (unless (endp (third obj))
	   (error "VAL expected only a single expression."))
	 (second obj))
	((plistp obj :keywords-only t)
	 `(alexandria:plist-hash-table
	   (list .
	    ,(loop :for (key val) :on obj :by #'cddr
		   :append (list (translate key) (translate val))))))
	((or (listp obj) (vectorp obj))
	 `(list . ,(map 'list #'translate obj)))
	(t obj)))


(defmethod cl-json:encode-json ((obj (eql ':false)) &optional stream)
  "Encode :FALSE as false."
  (princ "false" stream)
  nil)

(defmethod cl-json:encode-json ((obj (eql ':true)) &optional stream)
  "Encode :TRUE as true."
  (princ "true" stream)
  nil)

(defmethod cl-json:encode-json ((obj (eql ':null)) &optional stream)
  "Encode :NULL as null."
  (princ "null" stream)
  nil)

(defmacro vega-lite (data &body body)
  "Generate the serialized JSON for a Vega Lite plot of DATA."
  (unless (plistp body :keywords-only t)
    (error "Expected keyword indexed property list in body of VEGA-LITE macro."))
  (let ((out (gensym)))
    `(with-output-to-string (,out)    
       (cl-json:encode-json
	,(translate
	  (append (list :data (list :values data))
		  body))
	,out))))


;;; HTTP plotting

(defparameter *cached-plots*
  (make-hash-table :test 'equalp)
  "A table of cached plots, indexed by their IDs.")

(defun gen-id ()
  "Generate an unused ID for a plot."
  (loop :with id := (symbol-name (gensym "plot"))
	:while (gethash id *cached-plots*)
	:do (setf id (symbol-name (gensym "plot")))
	:finally (return id)))

(defun add-plot (json)
  "Add a plot to *CACHED-PLOTS*."
  (let ((id (gen-id)))
    (setf (gethash id *cached-plots*) json)
    id))

(defun get-plot (id)
  "Get a plot from *CACHED-PLOTS*."
  (gethash id *cached-plots*))

(defun reset-cache ()
  "Reset *CACHED-PLOTS*."
  (setf *cached-plots*
	(make-hash-table :test 'equalp)))


(defun make-javascript-literal (plot-string)
  "Create a Lisp string that represents PLOT-STRING as a Javascript
   string literal. Specifically, the resulting string starts and ends
   with single-quote (') surrounding the characters of PLOT-STRING
   such that any backslash (\\) or single-quote character is escaped
   by the backslash character. For example, if PLOT-STRING contains
   the sequence

     Abc\\def 'ghi'

   this returns a Lisp string containing the sequence

     'Abc\\\\def \\'ghi\\''"
  (with-output-to-string (s)
    (write-char #\' s)
    (loop :for c :across plot-string
          :do (case c
                ((#\' #\\) (write-char #\\ s)))
              (write-char c s))
    (write-char #\' s)))

(defun html-template (plot-string &optional stream)
  (let ((plot-string-as-javascript-string-literal
          (make-javascript-literal plot-string)))
    (format stream "<!DOCTYPE html>
<html>
  <head>
    <title>Vega-Lite Plot</title>
    <script src=\"https://cdn.jsdelivr.net/npm/vega@5.15.0\"></script>
    <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@4.15.0\"></script>
    <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6.11.1\"></script>
  </head>
  <body>
    <div id=\"vis\"></div>

    <script type=\"text/javascript\">
      var rawSpec = ~A;
      var spec = JSON.parse(rawSpec);
      vegaEmbed('#vis', spec);
    </script>
  </body>
</html>" 
            plot-string-as-javascript-string-literal)))



(defvar *acceptor* nil
  "The active Hunchentoot acceptor.")

(defun start-http-plotter (&key (port 4242))
  "Start the HTTP plotter on the provided PORT."
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (reset-cache)
  (setf *acceptor*
	(make-instance 'hunchentoot:easy-acceptor
		       :port port
		       :access-log-destination nil
		       :message-log-destination nil))
  (hunchentoot:start *acceptor*)

  (hunchentoot:define-easy-handler (http-show-plot :uri "/plot")
    (id)
  (let ((plot-json (get-plot id)))
    (unless plot-json
      (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
      (return-from http-show-plot nil))
    (with-output-to-string (out)
      (html-template plot-json out))))

  t)


(defvar *display-command-name* "open"
  "The command used to open an HTTP plot.")


(defmacro plot ((data &key mode) &body body)
  "Plot DATA according to the Vega Lite directives indicated in BODY."
  (let ((id-var (gensym))
	(plot-op `(vega-lite ,data ,@body)))
    (when mode
      (check-type mode plotting-mode))
    `(ecase (or ,mode *plotting-mode*)
       (:immediate ,plot-op)
       (:http
	(progn
	  (when (null *acceptor*)
	    (start-http-plotter))
	  (unless *acceptor*
	    (error "Unable to plot in HTTP mode: plot server not started. Try START-HTTP-PLOTTER."))
	  (let ((,id-var (add-plot ,plot-op)))
	    (uiop:run-program
	     (list *display-command-name*
		   (format nil "http://localhost:~D/plot?id=~A"
			   (hunchentoot:acceptor-port *acceptor*)
			   ,id-var)))
	    ,id-var))))))

