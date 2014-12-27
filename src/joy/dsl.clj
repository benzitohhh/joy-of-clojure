(ns joy.dsl
  (:require [clojure.set :as ra]))

(def artists
  #{{:artist "Burial"  :genre-id 1}
    {:artist "Magma"   :genre-id 2}
    {:artist "Can"     :genre-id 3}
    {:artist "Faust"   :genre-id 3}
    {:artist "Ikonika" :genre-id 1}
    {:artist "Grouper"}})

(def genres
  #{{:genre-id 1 :genre-name "Dubstep"}
    {:genre-id 2 :genre-name "Zeuhl"}
    {:genre-id 3 :genre-name "Prog"}
    {:genre-id 4 :genre-name "Drone"}})

(def ALL identity)

(ra/select ALL genres)

(ra/select #(#{1 3} (:genre-id %)) genres)

(take 4 (ra/join artists genres))


(defn meters->feet [m] (* m 3.28083989501312))

(defn meters->miles [m] (* m 0.000621))

(defn relative-units [u units]
  (let [spec (u units)]
    (if (nil? spec)
      (throw (Exception. (str "Undefined unit " u)))
      (if (vector? spec)
        (let [[conv to] spec]
          (* conv
             (relative-units to units)))
        spec))))

(relative-units :m {:m 1 :cm 100 :mm [10 :cm]})
;=> 1

(relative-units :cm {:m 1 :cm 100 :mm [10 :cm]})
;=> 100

(relative-units :mm {:m 1 :cm 100 :mm [10 :cm]})
;=> 1000

(defmacro defunits-of [name base-unit & conversions]
  (let [magnitude (gensym)
        unit (gensym)
        units-map (into `{~base-unit 1}
                        (map vec (partition 2 conversions)))]
    `(defmacro ~(symbol (str "unit-of-" name))
       [~magnitude ~unit]
       `(* ~~magnitude
           ~(case ~unit
              ~@(mapcat
                 (fn [[u# & r#]]
                   `[~u# ~(relative-units u# units-map)])
                 units-map)))
       )
    ))

(defunits-of distance :m
  :km 1000
  :cm 1/100
  :mm [1/10 :cm]
  :ft 0.3048
  :mile [5280 :ft])

(unit-of-distance 1 :m)
(unit-of-distance 1 :mm)
(unit-of-distance 1 :ft)
(unit-of-distance 1 :mile)

