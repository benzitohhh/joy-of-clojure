(ns joy.concurrency.chess
  (:import (java.util.concurrent Executors ExecutorService)))

(def ^:dynamic *pool* (Executors/newFixedThreadPool
                       (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads! [f & {thread-count :threads
                       exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

;;(dothreads! #(println "hello, thread here") :threads 2 :times 4) ;; prints to standard out

;(.shutdown *pool*)

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(map + yx %) deltas))))

(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map [f bd]
  (vec (map #(vec (for [s %] (f s))) bd)))

(defn reset-bd!
  "Resets the board state. Generally these types of functions are a
  bad idea, but matters of page count force our hand."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves (partial neighbors
                         [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move? [to enemy-sq]
  (when (not= to enemy-sq) to))

(defn choose-move [[[mover mpos][_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(reset-bd!)
;(take 5 (repeatedly #(choose-move @to-move)))

(defn place [from to] to)

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src ) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece move @to-move)
     (update-to-move move))))

;(make-move)
;=> [[:k [0 1]] [:K [2 0]]]
;(board-map deref board)
;=> [[:- :k :-] [:- :- :-] [:K :- :-]]
;@num-moves
;=> 1

;@num-moves  ;; the @ defrefs a ref

;; OK... let's throw a bunch of threads at this

(defn go [move-fn threads times]
  (dothreads! move-fn :threads threads :times times))

;(go make-move 100 100)

;(board-map #(dosync (deref %)) board)
;=> [[:k :- :-] [:- :- :-] [:K :- :-]]

;@to-move
;=> [[:k [0 0]] [:K [2 0]]]

;@num-moves
;=> 10001




