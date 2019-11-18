(ns org.clojars.lynch.loads.fake)


(def ^:private min-range 5)
(def ^:private max-1-range 10)
(def ^:private max-2-range 20)
(def ^:private max-3-range 25)


(defn- get-power-2-5
  [value]
  (if (or (zero? (mod value 2))
          (zero? (mod value 5)))
    value
    (recur (inc value))))


(defn- range-value
  [value min-value max-value]
  (let [min-rand (rand-int min-value)
        max-rand (rand-int max-value)]
    (if (neg? value)
      (+ min-rand (- value max-rand))
      (+ max-rand (- value min-rand)))))


(defn recalc-load
  [value]
  (let [abs-value (Math/abs value)]
    (cond
      (>= abs-value 100) (get-power-2-5 (range-value value min-range max-3-range))
      (>= abs-value 50)  (get-power-2-5 (range-value value min-range max-2-range))
      (>= abs-value 15)  (get-power-2-5 (range-value value min-range max-1-range))
      (> abs-value 5)    (get-power-2-5 (range-value value 0 min-range))
      :else          value)))
