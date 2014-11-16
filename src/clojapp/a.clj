(ns clojapp.a
  (:gen-class))

;; A* search implementation
(def world
  [[  1   1   1   1   1]
   [999 999 999 999   1]
   [  1   1   1   1   1]
   [  1 999 999 999 999]
   [  1   1   1   1   1]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (filter (fn [new-yx] (every? #(< -1 % size)
                                 new-yx))
             (map #(vec (map + yx %)) deltas)   ;; each item is a Vector
             ;(map #(map + yx %) deltas)        ;; each item would be a LazySequence
             )))

(defn estimate-cost [step-cost-est size y x] ;; [y x] is the destination
  (* step-cost-est
     (- (+ size size) y x 2))) ;; why subtract 2 here???

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

;(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])
;;;=> {:cost 9}

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (repeat size (vec (repeat size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo) ;; check done
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                          (fn [w] ;; expects w = [y x] is a Vector
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))


(astar [0 0]
       900
       world)



;; ============ NOTE on comparable ======================

(into (sorted-set [0 [5 5]])
      [[0 [5 6]]]) ;; ok

(into (sorted-set [0 [5 5]])
      [[0 (lazy-seq [5 6])]]) ;; NO!!! clojure.lang.Lazy cannot be cast to java.lang.Comparable

(into (sorted-set [0 [5 5]])
      [[0 ['(5 6)]]]) ;; ok

(into (sorted-set [0 [5 5]])
      [[0 '((5 6))]]) ;; NOO!  clojure.lang.PersistentList cannot be cast to java.lang.Comparable


(instance? Comparable (first (lazy-seq [5 6])))

(every? #(instance? Comparable %) (lazy-seq [5 6])) ;; true
(every? #(instance? Comparable %) [[5 6]])          ;; true
(every? #(instance? Comparable %) ['(5 6)])         ;; false

(type '(5 6)) ;; clojure.lang.PersistentList  - NOT comparable!
(type [[5 6]]) ;; clojure.lang.PersistentVector - is comparable
(type (vec '(5 6))) ;; clojure.langPersistentVector - comparable

;; Moral of the story:
;; Lists are NOT comparable. Vectors are
;; Often you need to convert lists to Vectors to make them comparable

(def x [{:foo 2 :bar 11}
        {:bar 99 :foo 1}
        {:bar 55 :foo 2}
        {:foo 1 :bar 77}])


(sort-by #(vec (map % [:foo :bar])) x)
