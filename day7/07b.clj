(ns day-7)

(require '[clojure.string :as str])

(defn fuelCost [x]
  (/ (* x (+ 1 x)) 2))

(defn totalDelta [xs dst]
  (reduce + (map (fn [x] (fuelCost (Math/abs ^int (- x dst)))) xs)))

(defn calcIdealPos [xs start]
  (def thisDelta (totalDelta xs start))
  (def nextDelta (totalDelta xs (+ 1 start)))
  (cond (> nextDelta thisDelta) start
        :else (calcIdealDst xs (+ 1 start))))

(def input (as-> (slurp "./input") x
             (str/split x #",")
             (map str/trim x)
             (map #(. Integer parseInt %) x)))

(totalDelta input (calcIdealPos input (apply min input)))
