(ns joy.core
  (:gen-class))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

;;; A tree
(defn xconj [t v]
  (cond
    (nil? t)       {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t),
                    :L (xconj (:L t) v),
                    :R (:R t)}
    :else          {:val (:val t),
                    :L (:L t),
                    :R (xconj (:R t) v)}))

;;; Display the tree, sorted order (lowest first)
(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))


;;; Non lazy way to create a nested structure.
; (rec-step [ 1 2 3 4])
; => [1 [2 [3 [4 []]]]]
(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

;;; Lazy implementation
(defn lz-rec-step [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lz-rec-step (rest s))]
      [])))

;;; And another lazy thing
(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

;;; Returns an infinite lazy sequence
;;(iterate inc 1)

;;; Calculate the nth triangle number
(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(def tri-nums (map triangle (iterate inc 1)))

(take 10 tri-nums)

(take 10 (filter even? tri-nums))

(nth tri-nums 99)

;;; Get the first 2 triangle numbers greater than 10,000
(take 2 (drop-while #(< % 10000) tri-nums))




(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive)))
;=> :cheap

(defer-expensive (delay false)
  (delay (do (Thread/sleep 5000) :expensive)))
;=> :expensive



(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head  [l]   (:head l))

(defn tail  [l]   (force (:tail l)))

(def tri-nums (inf-triangles 1))

(head tri-nums)

(head (tail (tail (tail tri-nums))))

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(taker 10 tri-nums)
;=> [1 3 6 10 15 21 28 36 45 55]

(nthr tri-nums 99)
;=> 5050

(resolve 'taker)



(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts
  "Lazy, tail-recursive, incremental quicksort.  Works against
   and creates partitions based on the pivot, defined as 'work'."
  [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list*
                 (filter smaller? xs)
                 pivot
                 (remove smaller? xs)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

;(qsort (nom 10))

(def fifth  (comp first rest rest rest rest))

(fifth [0 2 3 4 5 6 7])
(seq [2 3])

(def plays [{:band "Burial",     :plays 979,  :loved 9}
            {:band "Eno",        :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979,  :loved 9}
            {:band "Magma",      :plays 2665, :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(conj {:foo 1 :bar 2} (apply hash-map [:yay 3 :woop 9]))

(def bearings [{:x  0, :y  1}   ; north
               {:x  1, :y  0}   ; east
               {:x  0, :y -1}   ; south
               {:x -1, :y  0}]) ; west

(defn bot [x y bearing-num]
  {:coords  [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                       (+ y (:y (bearings bearing-num)))
                       bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(defn mirror-bot [x y bearing-num]
  {:coords     [x y]
   :bearing    ([:north :east :south :west] bearing-num)
   :forward    (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                                 (- y (:y (bearings bearing-num)))
                                 bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left  (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})


;; Power function (without tail call recursion - large values will cause a StackOverflow exception)
(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

;; Power function with tail call recursion
(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

;; Finite state machine for an elevator
;; Implemented using mutually recursive functions and "tramploline"
(defn elevator [commands]
  (letfn
      [(ff-open [[cmd & r]]
         "When the elevator is open on the 1st floor
          it can either close or be done."
         #(case cmd
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[cmd & r]]
         "When the elevator is closed on the 1st floor
          it can either open or go up."
         #(case cmd
            :open (ff-open r)
            :up   (sf-closed r)
            false))
       (sf-closed [[cmd & r]]
         "When the elevator is closed on the 2nd floor
          it can either open or go down"
         #(case cmd
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[cmd & r]]
         "When the elevator is open on the 2nd floor
          it can either close or be done"
         #(case cmd
            :close (sf-closed r)
            :done true
            false))]
    (trampoline ff-open commands)))

;; factorial function (using continuation passing style)
(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

;; and a generalized cps generator
(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (kont v n))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
     n kend)))

(def fac (mk-cps zero? 1 identity #(* %1 %2)))


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
     (filter (fn [new-yx]
               (every? #(< -1 % size) new-yx))
             (map #(map + yx %) deltas))))

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
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
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
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))


(astar [0 0]
       900
       world)

