(ns org.clojars.lynch.loads.calc
  (:require 
    [org.clojars.lynch.loads.types #?@(:cljs [:include-macros true
                                              :refer [Line Section Station]])])
  #?(:clj 
     (:import [org.clojars.lynch.loads.types Line Section Station])))


;;--------------------------------------------------------------------------
;; Utils 
;;--------------------------------------------------------------------------

(defn- out-sample?
  [sample]
  (neg? sample))

(defn- in-sample?
  [sample]
  (pos? sample))

(defn- transpose [matrix]
  (apply map list matrix))

(defn- update-lines
  [section fun]
  (update section :lines fun))

(defn- map-update-sections [fun station]
  (update
    station
    :sections
    (partial map fun)))

(defprotocol IRecalcSamples
  (recalc-samples [this fun])
  (recalc-out-samples [this fun])
  (recalc-in-samples [this fun]))


;;--------------------------------------------------------------------------
;; Line funcs
;;--------------------------------------------------------------------------

(defn- recalc-out-samples-on-line
  [line fun]
  (update
    line
    :samples
    (partial map (fn [sample]
                   (if (out-sample? sample)
                     (fun sample)
                     sample)))))

(defn- recalc-in-samples-on-line
  [line fun values]
  (update
    line
    :samples 
    (fn [samples]
      (map (fn [sample value]
            (if (in-sample? sample)
              ;; Проверка что есть небаланс нагрузок
              (if (and
                    (zero? value)
                    (not (zero? sample)))
                (fun sample)
                value)
              sample))
          samples
          values))))

(extend-type Line
  IRecalcSamples
  (recalc-out-samples [this fun]
    (recalc-out-samples-on-line this fun)))


;;--------------------------------------------------------------------------
;; Section funcs
;;--------------------------------------------------------------------------

(defn- transpose-samples-on-section
  [section]
  (->> section
       :lines
       (map :samples)
       (transpose)))

(defn- sum-out-samples-on-section
  [section]
  (->> section
       (transpose-samples-on-section)
       (map (partial filter out-sample?))
       (map (partial apply +))
       (map #(Math/abs %))))

(defn- count-in-samples-on-section
  [section]
  (->> section
       (transpose-samples-on-section)
       (map (partial filter in-sample?))
       (map count)))

;; FIX Can emit zero division error
(defn- calc-loads-on-section
  [section]
  (let [load-on-sec  (sum-out-samples-on-section section)
        in-lines-num (count-in-samples-on-section section)]
    (map #(Math/round (float (/ %1 %2))) load-on-sec in-lines-num)))

(defn- recalc-out-samples-on-section
  [section fun]
  (update-lines section (fn [lines]
                          (map #(recalc-out-samples-on-line % fun) lines))))

(defn- recalc-in-samples-on-section
  [section fun]
  (let [loads-on-sec (calc-loads-on-section section)]
    (update-lines section (fn [lines]
                            (map #(recalc-in-samples-on-line % fun loads-on-sec)
                                 lines)))))

(defn- recalc-samples-on-section
  [section fun]
  (-> section
      (recalc-out-samples-on-section fun)
      (recalc-in-samples-on-section fun)))

(extend-type Section
  IRecalcSamples
  (recalc-samples [this fun]
    (recalc-samples-on-section this fun))
  (recalc-out-samples [this fun]
    (recalc-out-samples-on-section this fun))
  (recalc-in-samples [this fun]
    (recalc-in-samples-on-section this fun)))


;;--------------------------------------------------------------------------
;; Station funcs
;;--------------------------------------------------------------------------

(defn- recalc-out-samples-on-station
  [station fun]
  (map-update-sections #(recalc-out-samples-on-section % fun) station))

(defn- recalc-in-samples-on-station
  [station fun]
  (map-update-sections #(recalc-in-samples-on-section % fun) station))

(defn- recalc-samples-on-station
  [station fun]
  (map-update-sections #(recalc-samples-on-section % fun) station))

(extend-type Station
  IRecalcSamples
  (recalc-samples [this fun]
    (recalc-samples-on-station this fun))
  (recalc-out-samples [this fun]
    (recalc-out-samples-on-station this fun))
  (recalc-in-samples [this fun]
    (recalc-in-samples-on-station this fun)))

;;--------------------------------------------------------------------------
;; Error funcs
;;--------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Naive tests
;;------------------------------------------------------------------------------

(comment
  (use '[loads.types :as t])
  (use '[loads.fake :refer [recalc-load]])
  (use '[clojure.pprint :refer [pprint]]))

(comment
  (recalc-out-samples-on-line
    (t/mk-line "A" -30 -30) recalc-load)
  (recalc-in-samples-on-line
    (t/mk-line "A" -30 -30) recalc-load [40 40])
  (recalc-out-samples
    (t/mk-line "A" -30 -30) recalc-load)
  (recalc-in-samples
    (t/mk-line "A" 30 30) recalc-load))

(comment 
  (transpose-samples-on-section
    (t/mk-section 
      (t/mk-line "A" 1 2 3)
      (t/mk-line "B" 4 5 6)))
  (sum-out-samples-on-section
    (t/mk-section
      (t/mk-line "A" -10 -10)
      (t/mk-line "B" -20 -20)))
  (recalc-samples-on-section
    (t/mk-section
      (t/mk-line "A" -10 -10)
      (t/mk-line "B" -20 -20))
    recalc-load)
  (recalc-out-samples-on-section
    (t/mk-section
      (t/mk-line "F"  30  30)
      (t/mk-line "A" -10 -10)
      (t/mk-line "B" -20 -20))
    recalc-load)
  (recalc-in-samples-on-section
    (t/mk-section
      (t/mk-line "F"  20  20)
      (t/mk-line "A" -10 -10)
      (t/mk-line "B" -20 -20))
    recalc-load)
  )

(comment 
  (-> (t/mk-station "3600"
        (t/mk-section
          (t/mk-line "ф.20-210"  50 70)
          (t/mk-line "ф.20-207"  50 70)
          (t/mk-line "3643А"     -30 -45)
          (t/mk-line "3661А"     -10 -10)
          (t/mk-line "3674Б"     -20 -25)
          (t/mk-line "3673А"     -40 -45)
          (t/mk-line "3749"      -18 -15)) 
        (t/mk-section
          (t/mk-line "ф.20-506"  74 70)
          (t/mk-line "ф.20-513"  74 70)
          (t/mk-line "3643Б"     -40 -25)
          (t/mk-line "3661Б"     -30 -20)
          (t/mk-line "3674А"     -30 -50)
          (t/mk-line "3673Б"     -40 -25)
          (t/mk-line "8630"      0 0)
          (t/mk-line "Т"         -8 -12)))
      (recalc-samples recalc-load)
      (pprint))
  ;; :sections
  ;; (map recalc-samples-on-section))
  )


