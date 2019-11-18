(ns org.clojars.lynch.loads.types
  (:require
    [clojure.spec.alpha :as s]
    ))


;;--------------------------------------------------------------------------------
;; Simple types
;;--------------------------------------------------------------------------------

(s/def ::ne-string 
  (every-pred string? not-empty))

;;--------------------------------------------------------------------------------
;; Samples 
;;--------------------------------------------------------------------------------

(s/def ::sample int?)
(s/def ::samples (s/coll-of ::sample))

;;--------------------------------------------------------------------------------
;; Lines 
;;--------------------------------------------------------------------------------

(s/def :line/name ::ne-string)
(s/def :line/samples ::samples)

(s/def ::line
  (s/keys :req-un [:line/name :line/samples]))

(defrecord Line [name samples])

(s/def ::->Line
  (s/and 
    ::line
    (s/conformer 
      (fn [value]
        (->Line (:name value) (:samples value))))))

(defn mk-line [name & samples]
  (let [line (->Line name samples)]
    (if-let [explain (s/explain-data ::line line)]
      (throw (ex-info "Validation error" {:msg     (str "on line " name)
                                          :explain explain}))
      line)))

;;--------------------------------------------------------------------------------
;; Sections 
;;--------------------------------------------------------------------------------

(s/def :section/lines (s/coll-of ::->Line))

(s/def ::section
  (s/keys :req-un [:section/lines]))

(defrecord Section [lines])

(s/def ::->Section
  (s/and
    ::section
    (s/conformer
      (fn [value]
        (->Section (:lines value))))))

(s/def ::sections (s/coll-of ::->Section))

(defn mk-section [& lines]
  (let [section (->Section lines)]
    (if-let [explain (s/explain-data ::section section)]
      (throw (ex-info "Validation error" {:msg     "on section"
                                          :explain explain}))
      section)))

;;--------------------------------------------------------------------------------
;; Stations 
;;--------------------------------------------------------------------------------

(s/def :station/name ::ne-string)
(s/def :station/sections ::sections)

(s/def ::station 
  (s/keys :req-un [:station/name :station/sections]))

(defrecord Station [name sections])

(s/def ::->Station
  (s/and
    ::station
    (s/conformer
      (fn [value]
        (->Station (:name value) (:sections value))))))

(s/def ::->Stations
  (s/coll-of ::->Station))

(defn mk-station [name & sections]
  (let [station (->Station name sections)]
    (if-let [explain (s/explain-data ::station station)]
      (throw (ex-info "Validation error" {:msg     (format "on station %s" name)
                                          :explain explain}))
      station)))

