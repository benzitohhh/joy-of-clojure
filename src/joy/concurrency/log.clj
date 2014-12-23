(ns joy.concurrency.log
  (:require [joy.concurrency.common :refer :all]))

(def log-agent (agent 0))
