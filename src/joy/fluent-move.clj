(ns joy.fluent-move
  (:gen-class))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;; Super simple implementation
{:from "e7", :to "e8", :castle? false, :promotion \Q}

(defn build-move [& pieces]
  (apply hash-map pieces))

(build-move :from "e7" :to "e8" :promotion \Q)
;=> {:from "e7", :to "e8", :promotion \Q}



;; Implemntation using records
(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castle"
             (if-let [p (:promotion this)]
               (str " promote to " p)
               "")))))

(.println System/out (Move. "e7" "e8" nil \Q))



;; Implementation that seperates concerns (i.e. Move record does not need to worry about what is a legal set of params)
(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

(str (build-move :from "e2" :to "e4"))
;=> "Move e2 to e4"

(true? [true true])
