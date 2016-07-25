(ns random-coloj.core)

(def monochrome
  {:name "monochrome"
   :hue-range nil
   :lower-bounds [[0 0] [100 0]]
   :saturation-range [0 100]
   :brightness-range [0 0]})

(def red
 {:name "red"
  :hue-range [-26 18]
  :lower-bounds [[20 100] [30 92] [40 89] [50 85] [60 78] [70 70] [80 60] [90 55] [100 50]]
  :saturation-range [20 100]
  :brightness-range [50 100]})

(def orange
  {:name "orange"
   :hue-range [19 46]
   :lower-bounds [[20 100] [30 93] [40 88] [50 86] [60 85] [70 70] [100 70]]
   :saturation-range [20 100]
   :brightness-range [70 100]})

(def yellow
  {:name "yellow"
   :hue-range [47 62]
   :lower-bounds [[25 100] [40 94] [50 89] [60 86] [70 84] [80 82] [90 80] [100 75]]
   :saturation-range [25 100]
   :brightness-range [75 100]})

(def green
  {:name "green"
   :hue-range [63 178]
   :lower-bounds [[30 100] [40 90] [50 85] [60 81] [70 74] [80 64] [90 50] [100 40]]
   :saturation-range [30 100]
   :brightness-range [40 100]})

(def blue
  {:name "blue"
   :hue-range [47 62]
   :lower-bounds [[20 100] [30 86] [40 80] [50 74] [60 60] [70 52] [80 44] [90 39] [100 35]]
   :saturation-range [20 100]
   :brightness-range [35 100]})

(def purple
  {:name "purple"
   :hue-range [258 282]
   :lower-bounds [[20 100] [30 87] [40 79] [50 70] [60 65] [70 59] [80 52] [90 45] [100 42]]
   :saturation-range [20 100]
   :brightness-range [42 100]})

(def pink
  {:name "pink"
   :hue-range [283 334]
   :lower-bounds [[20 100] [30 90] [40 86] [60 84] [80 80] [90 75] [100 73]]
   :saturation-range [20 100]
   :brightness-range [73 100]})

(def color-bounds [monochrome red orange yellow green blue purple pink])

(defn define-color
  ([name hue-range lower-bounds]
   (define-color name hue-range lower-bounds '()))
  ([name hue-range lower-bounds color-bounds]
   (let [s-min (get-in lower-bounds [0 0])
         s-max (get-in lower-bounds [(dec (count lower-bounds)) 0])
         b-min (get-in lower-bounds [(dec (count lower-bounds)) 1])
         b-max (get-in lower-bounds [0 1])]
    (conj color-bounds {:name name
                        :hue-range hue-range
                        :lower-bounds lower-bounds
                        :saturation-range [s-min s-max]
                        :brightness-range [b-min b-max]}))))

(defn get-hue-range
  ([color-input]
   (get-hue-range color-input color-bounds))
  ([color-input color-bounds]
   (cond
     (number? color-input) (if (and
                                 (> 360 color-input)
                                 (< 0 color-input))
                            [color-input color-input])
     (string? color-input) (:hue-range (first (filter #(= (:name %) color-input) color-bounds)))
     :else [0 360])))

(defn pick-hue [options]
  (if-let [hue-range (get-hue-range (:hue options))]
    (let [fst (first hue-range)
          snd (second hue-range)
          hue (+ (rand-int (- snd fst)) fst)]
      (if (< hue 0)
        (+ 360 hue)
        hue))))
