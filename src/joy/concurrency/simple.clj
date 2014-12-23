(ns joy.concurrency.simple
  (:import (java.util.concurrent Executors ExecutorService)))

(def ^:dynamic *pool* (Executors/newFixedThreadPool
             (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads! [f & {thread-count :threads
                       exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

(dothreads! (fn [] (println "yay")) :threads 5 :times 3)

(.shutdown *pool*)



















































