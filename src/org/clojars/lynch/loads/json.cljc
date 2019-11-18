(ns org.clojars.lynch.loads.json
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [org.clojars.lynch.loads.io :as io]
    #?(:clj [clojure.data.json :as json])
    [org.clojars.lynch.loads.errors :as errors]
    ))


(defn from-json-str [spec-conform str]
  (let [json #?(:clj (json/read-str str :key-fn #(-> % string/lower-case keyword))
                :cljs (js->clj (.parse js/JSON str) :keywordize-keys true))]
    (io/from-map spec-conform json)))

#?(:clj
   (defn to-json-str [data]
     (json/write-str data)))

#?(:cljs
   (defn to-json-str [data]
     (.stringify js/JSON (clj->js data))))
