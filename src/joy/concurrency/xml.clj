(ns joy.concurrency.xml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip])
  (:import [java.util.regex Pattern]))

(defmulti rss-children class)

(defmethod rss-children String [uri-str]
  (-> (xml/parse uri-str)
      zip/xml-zip
      zip/down
      zip/children))

(def feed "http://127.0.0.1/~Marisa/fogus.rss")
(def text "#Clojure")
(def items (rss-children feed))
(def re (Pattern/compile (Pattern/quote text)))

(defn count-tweet-text-task [txt feed]
  (let [items (rss-children feed)
        re    (Pattern/compile (Pattern/quote text))]
    (count
     (mapcat #(re-seq re (first %))
             (for [item (filter (comp #{:item} :tag) items)]
               (-> item :content first :content))))))

(defmacro as-futures [[a args] & body]
  (let [parts            (partition-by #{'=>} body)
        [acts _ [res]]   (partition-by #{:as} (first parts))
        [_ _ task]       parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))

(defn tweet-occurrences [tag & feeds]
  (as-futures [feed feeds]
    (count-tweet-text-task tag feed)
    :as results
    =>
    (reduce (fn [total res] (+ total @res))
            0
            results)))

(tweet-occurrences text feed feed)

(def tag "#Clojure")
(def feeds [feed])


