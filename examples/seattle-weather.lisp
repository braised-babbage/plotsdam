(in-package #:plotsdam-examples)

;;; from https://vega.github.io/vega-lite/tutorials/explore.html

(defvar seattle-weather-data
  (rest
   (cl-csv:read-csv
    (asdf:system-relative-pathname :plotsdam-examples #P"examples/seattle-weather.csv")
    :map-fn (lambda (row)
	      (loop :for field :in '(:date :precipitation :temp_max :temp_min :wind :weather)
		    :for elt :in row
		    :collect (cons field elt))))))

(defun precipitation-plot ()
  "A plot with tick marks to show the distribution of precipitation."
  (plot (seattle-weather-data)
    :mark :tick
    :encoding (:x (:field :precipitation :type :quantitative))))


(defun precipitation-hist ()
  "A histogram of precipitation amounts."
  (plot (seattle-weather-data)
    :mark :bar
    :encoding (:x (:bin :true :field :precipitation)
	       :y (:aggregate :count))))

(defun monthly-change ()
  "A line plot showing the monthly precipitation."
  (plot (seattle-weather-data)
    :mark :line
    ;; note: vega is case sensitive
    :encoding (:x (:|timeUnit| :month :field :date)
	       :y (:aggregate :mean :field :precipitation))))


(defun stacked-bar ()
  "A stacked bar plot showing the kind of weather by month."
  (plot (seattle-weather-data)
    :mark :bar
    :encoding (:x (:|timeUnit| :month :field :date :type :ordinal)
	       :y (:aggregate :count :type :quantitative)
	       :color (:field :weather :type :nominal))))
