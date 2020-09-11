(in-package :plotsdam-examples)

(defun bar-plot ()
  "Example of bar plot from https://vega.github.io/vega-lite/tutorials/getting_started.html"
  (let ((data '(((:a . "C") (:b . 2))
		((:a . "C") (:b . 7))
		((:a . "C") (:b . 4))
		((:a . "D") (:b . 1))
		((:a . "D") (:b . 2))
		((:a . "D") (:b . 6))
		((:a . "E") (:b . 8))
		((:a . "E") (:b . 4))
		((:a . "E") (:b . 7)))))
    (plot (data)
      :mark :bar
      :encoding (:y (:field :a :type :nominal)
		 :x (:aggregate :average :field :b :type :quantitative
		     :title "Mean of b")))))

