(ns day-6)

(require '[clojure.string :as str])

(defn add-fish [h days count]
  (assoc h days (+ count (or (h days) 0))))

(defn simulate-fish [h [days count]]
  (if (= 0 days) (-> (add-fish h 6 count)
                  (add-fish 8 count))
      (add-fish h (- days 1) count)))

(defn simulate-fishes [h]
  (reduce simulate-fish {} h))

(def input (as-> (slurp "./input") x
             (str/split x #",")
             (map str/trim x)
             (map #(. Integer parseInt %) x)))

(def inputMap (reduce (fn [acc days] (add-fish acc days 1)) {} input))

(def resultA (reduce (fn [acc _] (simulate-fishes acc)) inputMap (range 80)))
(def resultB (reduce (fn [acc _] (simulate-fishes acc)) inputMap (range 256)))

(def countA (reduce (fn [acc [_ count]] (+ acc count)) 0 resultA))
(def countB (reduce (fn [acc [_ count]] (+ acc count)) 0 resultB))

(println countA)
(println countB)
