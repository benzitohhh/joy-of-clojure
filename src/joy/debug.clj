(ns joy.debug
  (:require [clojure.xml :as xml]))


(defn traverse [node f]
  (when node
    (f node)
    (doseq [child (:content node)]
      (traverse child f))))

(traverse {:tag :flower :attrs {:name "Tanpopo"} :content []}
          println)

(def DB
  (-> "<zoo>
         <pongo>
           <animal>orangutan</animal>
         </pongo>
         <panthera>
           <animal>Spot</animal>
           <animal>lion</animal>
           <animal>Lopshire</animal>
         </panthera>
       </zoo>"
      .getBytes
      (java.io.ByteArrayInputStream.)
      xml/parse))

(traverse DB println)

(defn ^{:dynamic true} handle-weird-animal
  [{[name] :content}]
  (throw (Exception. (str name " must be 'dealt with'"))))

(defmulti visit :tag)

(defmethod visit :animal [{[name] :content :as animal}]
  (case name
    "Spot"     (handle-weird-animal animal)
    "Lopshire" (handle-weird-animal animal)
    (println name)))

(defmethod visit :default [node] nil)

(traverse DB visit)

(defmulti handle-weird  (fn [{[name] :content}] name))

(defmethod handle-weird "Spot" [_]
  (println "Transporting Spot to the circus."))

(defmethod handle-weird "Lopshire" [_]
  (println "Signing Lopshire to a book deal."))

(binding [handle-weird-animal handle-weird]
  (traverse DB visit))
