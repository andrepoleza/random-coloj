(ns random-coloj.core)

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
