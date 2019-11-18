(ns org.clojars.lynch.loads.errors
  (:require [clojure.spec.alpha :as s]))


;;--------------------------------------------------------------------------------
;; Error messages 
;;--------------------------------------------------------------------------------

(def spec-errors
  {::ne-string      :require-empty-string
   ::sample         :require-integer-value
   ::samples        :require-array-of-integers
   ::line           :error-in-line-data
   ::section        :error-in-section-data
   ::station        :error-in-station-data
   :default-message :error-in-input-data})

(defn problem->error-message
  [problem]
  (when problem
    (let [{:keys [via]} problem
          spec (last via)]
      (or
        (get spec-errors spec)
        (get spec-errors
             (-> spec name keyword))
        (get spec-errors :default-message)))))

(defn get-names [data in-keys]
  (loop [res (list)
         in-keys in-keys]
    (if (empty? in-keys)
      (filter identity (conj res (:name data)))
      (recur 
        (conj res (:name (get-in data in-keys)))
        (drop-last in-keys)))))

(defn problem->name-path
  [data problem]
  (when-let [keys-in (:in problem)]
    (get-names data keys-in)))
 
(defn get-error-messages
  [explain]
  (when explain
    (let [problems (::s/problems explain)
          value    (::s/value explain)] 
      (map
        list
        (map (partial problem->name-path value) problems)
        (map problem->error-message problems)))))

(defrecord SError [key path])
