(ns day-8)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def ALPHABET #{\a \b \c \d \e \f \g})
(defn decodeWith [signal mapping]
  (cond (every? (fn [x] (contains? mapping x)) signal) (set (map (fn [x] (get mapping x)) signal))
        :else false))

; characters that haven't been mapped (ie the mapping doesnt specify what they actually should be)
(defn unmappedChars [mapping]
  (set/difference ALPHABET (keys mapping)))

; characters that haven't been used (ie there is no character in the mapping that results in them)
(defn unusedChars [mapping]
  (set/difference ALPHABET (vals mapping)))

(defn validResult [x]
  (contains? #{#{\a \b \c \e \f \g} #{\c \f} #{\a \c \d \e \g} #{\a \c \d \f \g} #{\b \c \d \f} #{\a \b \d \f \g} #{\a \b \d \e \f \g}#{\a \c \f} #{\a \b \c \d \e \f \g} #{\a \b \c \d \f \g}} x))

(defn tryWith [signals mapping knownValues]
  (cond
    (not (every? (fn [[in expected]]
                   (def attemptedDecode (decodeWith in mapping))
                   (or (= false attemptedDecode) (= expected attemptedDecode)))
                 knownValues)) false ; stop considering path if it breaks any known values
    ;(not (every? (fn [in]
    ;               (def attemptedDecode (decodeWith in mapping))
    ;               (or (= false attemptedDecode) (validResult attemptedDecode)))
    ;              signals)) false ; stop when any of the fully resolved signals are invalid
    (every? validResult (map (fn [x] (decodeWith x mapping)) signals)) mapping ; base case - all resolved to valid values
    :else (do
            (def tryingToMap (first (unmappedChars mapping)))
            (first (filter (comp true? boolean) (for [candidate (unusedChars mapping)]
                                                  (do
                                                    (println (list tryingToMap candidate))
                                                    (tryWith signals (assoc mapping tryingToMap candidate) knownValues))))))))

(def KNOWN_VALUES {2 #{\c \f} 4 #{\b \c \d \f} 3 #{\a \c \f} 7 #{\a \b \c \d \e \f \g}})

(defn extractKnownValues [signals]
  (reduce (fn [m s] (cond (contains? KNOWN_VALUES (count s)) (assoc m s (get KNOWN_VALUES (count s)))
                          :else m)) {} signals))

(def input (as-> (slurp "./input_test") x
             (str/split x #"\n")
             (map (fn [l]
                    (map (fn [p] (str/split (str/trim p) #" ")) (str/split l #"\|"))) x)))

(def testLine (map set (set (flatten (first input)))))
(def otherTest '(#{\a \b \c \e \f \g} #{\c \f} #{\a \c \d \e \g} #{\a \c \d \f \g} #{\b \c \d \f} #{\a \b \d \f \g} #{\a \b \d \f \e \g} #{\a \c \f} #{\a \b \c \d \e \f \g} #{\a \b \c \d \f \g}))
(println (tryWith testLine {\a \c} (extractKnownValues testLine)))
