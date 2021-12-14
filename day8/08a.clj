(ns day-8)

(require '[clojure.string :as str])

(def input (as-> (slurp "./input") x
              (str/split x #"\n")
              (map (fn [l]
                     (map (fn [p] (str/split (str/trim p) #" ")) (str/split l #"\|"))) x)))

(def onlyOutputs (flatten (map second input)))
(def knownDigits (filter (fn [xs] (contains? (set '(2 4 3 7)) (count xs))) onlyOutputs))
(println (count knownDigits))
