(ns org.clojars.lynch.loads.io
  (:require
    [clojure.spec.alpha :as s]
    [org.clojars.lynch.loads.errors :as errors]
    ))


(defn from-map [spec-conform data]
  (let [idata (s/conform spec-conform data)]
    (if (= idata ::s/invalid)
     (->> data
          (s/explain-data spec-conform)
          errors/get-error-messages
          (map #(errors/->SError (last %) (first %))))
     idata)))

