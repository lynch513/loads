(ns org.clojars.lynch.loads.core
  (:require 
    [clojure.java.io :as io]
    [org.clojars.lynch.loads.io :as i]
    [org.clojars.lynch.loads.json :as j]
    [org.clojars.lynch.loads.types :as t]
    ))


(comment
  (->> "stations2.json"
      io/resource
      slurp
      (j/from-json-str ::t/->Stations))
  (i/from-map ::t/->Line {:name "" :samples [1 2]})
  (-> (t/mk-line "3477" 10 10)
      (clojure.data.json/write-str)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
