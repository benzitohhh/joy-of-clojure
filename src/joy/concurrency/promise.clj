(ns joy.concurrency.promise
  (:require [joy.concurrency.common :refer :all]
            [joy.concurrency.xml :refer [rss-children]]))

(def x (promise))
(def y (promise))
(def z (promise))

(dothreads! #(deliver z (+ @x @y)))

(dothreads!
 #(do (Thread/sleep 2000) (deliver x 52)))

(dothreads!
 #(do (Thread/sleep 4000) (deliver y 82)))

(time @z)




(defn tweet-items [k feed]
  (k
    (for [item (filter (comp #{:item} :tag) (rss-children feed))]
      (-> item :content first :content))))

(def feed "http://127.0.0.1/~Marisa/fogus.rss")

;; Here callback "count" is called asynchronously.
(tweet-items count feed)

;; We can rewrite this as a promise, to get "blocking" behaviour
(let [p (promise)]
  (tweet-items #(deliver p (count %))
               feed)
  @p)

(defmacro cps->fn [f k]
  `(fn [& args#]
     (let [p# (promise)]
       (apply ~f (fn [x#] (deliver p# (~k x#))) args#)
       @p#)))

(def count-items (cps->fn tweet-items count))

(count-items feed)

(pvalues 1 2 (+ 1 2))

(defn sleeper [s thing] (Thread/sleep (* 1000 s)) thing)

(defn pvs [] (pvalues
              (sleeper 2 :1st)
              (sleeper 3 :2nd)
              (keyword "3rd")))

(-> (pvs) first time)

(-> (pvs) last time)


(->> [1 2 3]
     (pmap (comp inc (partial sleeper 2)))
     doall
     time)

(with-precision 4
  (doall (map (fn [x] (/ x 3)) (range 1M 4M))))
