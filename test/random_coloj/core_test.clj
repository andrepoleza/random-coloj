(ns random-coloj.core-test
  (:require [clojure.test :refer :all]
            [random-coloj.core :refer :all]))

(deftest define-color-test
  (testing "define-color: monochrome"
    (let [color-bounds '()
          monochrome {:name "monochrome"
                      :hue-range nil
                      :lower-bounds [[0 0] [100 0]]
                      :saturation-range [0 100]
                      :brightness-range [0 0]}]
      (is (= (define-color (:name monochrome)
                           (:hue-range monochrome)
                           (:lower-bounds monochrome)
                           color-bounds) (conj color-bounds monochrome)))))

  (testing "define-color: red"
    (let [color-bounds '()
          red {:name "red"
               :hue-range [-26 18]
               :lower-bounds [[20 100] [30 92] [40 89] [50 85] [60 78] [70 70] [80 60] [90 55] [100 50]]
               :saturation-range [20 100]
               :brightness-range [50 100]}]
      (is (= (define-color (:name red)
                           (:hue-range red)
                           (:lower-bounds red)
                           color-bounds) (conj color-bounds red)))))

  (testing "define-color: orange"
    (let [color-bounds '()
          orange {:name "orange"
                  :hue-range [19 46]
                  :lower-bounds [[20 100] [30 93] [40 88] [50 86] [60 85] [70 70] [100 70]]
                  :saturation-range [20 100]
                  :brightness-range [70 100]}]
      (is (= (define-color (:name orange)
                           (:hue-range orange)
                           (:lower-bounds orange)
                           color-bounds) (conj color-bounds orange)))))

  (testing "define-color: yellow"
    (let [color-bounds '()
          yellow {:name "yellow"
                  :hue-range [47 62]
                  :lower-bounds [[25 100] [40 94] [50 89] [60 86] [70 84] [80 82] [90 80] [100 75]]
                  :saturation-range [25 100]
                  :brightness-range [75 100]}]
      (is (= (define-color (:name yellow)
                           (:hue-range yellow)
                           (:lower-bounds yellow)
                           color-bounds) (conj color-bounds yellow)))))

  (testing "define-color: green"
    (let [color-bounds '()
          green {:name "green"
                 :hue-range [63 178]
                 :lower-bounds [[30 100] [40 90] [50 85] [60 81] [70 74] [80 64] [90 50] [100 40]]
                 :saturation-range [30 100]
                 :brightness-range [40 100]}]
      (is (= (define-color (:name green)
                           (:hue-range green)
                           (:lower-bounds green)
                           color-bounds) (conj color-bounds green)))))

  (testing "define-color: blue"
    (let [color-bounds '()
          blue {:name "blue"
                :hue-range [47 62]
                :lower-bounds [[20 100] [30 86] [40 80] [50 74] [60 60] [70 52] [80 44] [90 39] [100 35]]
                :saturation-range [20 100]
                :brightness-range [35 100]}]
      (is (= (define-color (:name blue)
                           (:hue-range blue)
                           (:lower-bounds blue)
                           color-bounds) (conj color-bounds blue)))))

  (testing "define-color: purple"
    (let [color-bounds '()
          purple {:name "purple"
                  :hue-range [258 282]
                  :lower-bounds [[20 100] [30 87] [40 79] [50 70] [60 65] [70 59] [80 52] [90 45] [100 42]]
                  :saturation-range [20 100]
                  :brightness-range [42 100]}]
      (is (= (define-color (:name purple)
                           (:hue-range purple)
                           (:lower-bounds purple)
                           color-bounds) (conj color-bounds purple)))))

  (testing "define-color: pink"
    (let [color-bounds '()
          pink {:name "pink"
                :hue-range [283 334]
                :lower-bounds [[20 100] [30 90] [40 86] [60 84] [80 80] [90 75] [100 73]]
                :saturation-range [20 100]
                :brightness-range [73 100]}]
      (is (= (define-color (:name pink)
                           (:hue-range pink)
                           (:lower-bounds pink)
                           color-bounds) (conj color-bounds pink))))))

(deftest get-hue-range-test
  (is (= [180 180] (get-hue-range 180 color-bounds)))
  (is (= nil (get-hue-range "monochrome" color-bounds)))
  (is (= [-26 18] (get-hue-range "red" color-bounds)))
  (is (= [19 46] (get-hue-range "orange" color-bounds)))
  (is (= [47 62] (get-hue-range "yellow" color-bounds)))
  (is (= [63 178] (get-hue-range "green" color-bounds)))
  (is (= [47 62] (get-hue-range "blue" color-bounds)))
  (is (= [258 282] (get-hue-range "purple" color-bounds)))
  (is (= [283 334] (get-hue-range "pink" color-bounds))))

(deftest pick-hue-test
  (is (= nil (pick-hue {:hue "monochrome"})))
  (is (integer? (pick-hue {:hue "red"})))
  (is (integer? (pick-hue {:hue "orange"})))
  (is (integer? (pick-hue {:hue "yellow"})))
  (is (integer? (pick-hue {:hue "green"})))
  (is (integer? (pick-hue {:hue "blue"})))
  (is (integer? (pick-hue {:hue "purple"})))
  (is (integer? (pick-hue {:hue "pink"}))))

(deftest pick-saturation-test
  (is (= 0 (pick-saturation "monochrome" {:hue :monochrome} '())))
  (let [s (pick-saturation "monochrome" {:luminosity :random} '())]
    (is (and (< 0 s) (> 100 s))))
  (let [s (pick-saturation "red" {:luminosity :bright} color-bounds)]
    (is (and (< 55 s) (> 100 s))))
  (let [s (pick-saturation "red" {:luminosity :dark} color-bounds)]
    (is (and (< 10 s) (> 100 s))))
  (let [s (pick-saturation "red" {:luminosity :light} color-bounds)]
    (is (and (< 20 s) (> 55 s)))))
