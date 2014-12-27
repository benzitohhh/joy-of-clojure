(ns joy.concurrency.test
  (:require [clojure.test :as test]
            [joy.concurrency.xml :refer :all]))

(defn tweetless-rss-children [s]
  '({:tag :title, :attrs nil, :content ["Stub"]}))

(defn count-rss2-children [s]
  (count (rss-children s)))

;; Use "with-redefs" to map function calls to stubs

(test/deftest feed-tests
  (with-redefs [rss-children tweetless-rss-children]
    (test/testing "RSS2 Child Counting"
      (test/is (= 1 (count-rss2-children "dummy"))))
    (test/testing "Twitter Occurrence Counting"
      (test/is (= 0 (count-tweet-text-task "#clojure" ""))))))

(defn test-ns-hook []
  (feed-tests))

(test/run-tests 'joy.concurrency.test)

