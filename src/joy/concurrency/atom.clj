(ns joy.concurrency.atom
  (:require [joy.concurrency.common :refer :all]))


(def ^:dynamic *time* (atom 0))

(defn tick [] (swap! *time* inc))

(dothreads! tick :threads 1000 :times 100)
@*time*



(defn manipulable-memoize [function]
  (let [cache (atom {})] ;; store cache in Atom
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))

;(time [(slowly 9) (slowly 9)])
;; => "Elapsed time: 6000.63 msecs"

(def sometimes-slowly (manipulable-memoize slowly))
(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; => "Elapsed time: 3000.01 msecs"

(meta sometimes-slowly)
