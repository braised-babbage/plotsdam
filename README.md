# Plotsdam: Interactive plotting with Vega Lite

Plotsdam is a thin veneer over [Vega Lite](https://vega.github.io/vega-lite/). 

## Getting Started

The primary interface is the `plot` macro. There are two plotting modes, controlled by `*plotting-mode*`:

- `:http` mode (default) runs a hunchentoot server on port 4242 and plots to this
- `:immediate` mode just results in plots translating to json strings

There are a few examples in `examples/`. To load them all, try `(ql:quickload :plotsdam-examples)`. 

One colorful one is

```
(plot (plotsdam-examples:seattle-weather-data)
    :mark :bar
    :encoding (:x (:|timeUnit| :month :field :date :type :ordinal)
	       :y (:aggregate :count :type :quantitative)
	       :color (:field :weather :type :nominal)))
```

which renders as

![example image](https://github.com/kilimanjaro/plotsdam/blob/master/stacked-bar.svg?sanitize=true)
