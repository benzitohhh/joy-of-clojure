(ns joy.concurrency.log
  (:require [joy.concurrency.common :refer :all]))

(def log-agent (agent 0))

(defn do-log [msg-id message]
  (println msg-id ": " message)
  (inc msg-id))

(defn do-step [channel message]
  (Thread/sleep 1000)
  (send-off log-agent do-log (str channel message))) ;; dispatch action to the agent. Action will be called, and new value of agent set. The agent has a queue of actions to handle, and has a thread that works through these.

(defn three-step [channel]
  (do-step channel " ready to begin (step 0)") ;; each of these calls happens synchronously
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting going now (step 2)")
  (do-step channel " done! (step 3)"))

(defn all-together-now []
  (dothreads! #(three-step "alpha")) ;; each of these calls happens asynchronously
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "omega")))

;(all-together-now)
;@log-agent ;; use @ to get value from the agent

(defn exercise-agents [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a)))) ;; force the current thread to wait for all the agent threads to finish what they're doing (so that we can time them)

(time (exercise-agents send-off))
;; => "Elapsed time: 1008.771296 msecs"

(time (exercise-agents send))
;; => "Elapsed time: 3020.13262 msecs"

;; So basically... use "send-off" for threads that do io (i.e. the log printting stuff above)
;; Conversely...   use "send" for threads where no io is needed.

(send log-agent (fn [] 2000))   ; incorrect (agent fn must take at least one arg... so now agent will be in a "fail" state)..

(agent-error log-agent) ;; returns a non-nil value now...

(send log-agent (fn [_] 3000)) ;; will throw an exception (as agent is in "fail" state)

(restart-agent log-agent 2500 :clear-actions true) ;; this will restart the agent (and delete any actions in the queue)

(send-off log-agent do-log "The agent, it lives!")

@log-agent
