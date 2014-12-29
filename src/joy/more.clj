(ns joy.more
  (:gen-class))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(macroexpand '(-> 25 Math/sqrt int list))

(clojure.pprint/pprint (macroexpand '(time (print "timing"))))

(-> (/ 144 12) (/ 2 3) str keyword list)

(->> a (+ 5 ,,,) (let [a 5] ,,,))

(eval '(+ 1 2))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(contextual-eval {'a 2, 'b (* 2 6)} '(+ a b))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(defn from-end [s n]
  (let [delta (dec (- (count s) n))]
    (unless (neg? delta)
            (nth s delta))))


(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))


;; Humans vs monsters

(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)},
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
           (list? a) [:isa (str (second a))]
           (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                              {:tag :property,
                               :attrs {:name (str (first p))},
                               :content nil}))}))


;; Humans and monsters in action...

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elsuive creature"
                     [eats-goats?]))))

(import [java.io BufferedReader InputStreamReader]
        [java.net URL])

(defn joc-www []
  (-> "http://aistemos.com" URL.
      .openStream InputStreamReader. BufferedReader.))

;; (let [stream (joc-www)]
;;   (with-open [page stream]
;;     (println (.readLine page))
;;     (print "The stream will now close... ")))

(declare collect-bodies)

(defmacro contract
  "Returns function that wraps an input function with pre/post conditions (forms).
  Each form should have ([args] (:require xxx) (:ensure yyy))"
  [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) :require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) :ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception. (str "Unknown tag " (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (:require
             (pos? x))
            (:ensure
             (= (* 2 x) %))))

;; (doubler-contract #(* 2 %) 3)
;; => 6

(def times2 (partial doubler-contract #(* 2 %)))
(times2 3)


(def times3 (partial doubler-contract #(* 3 %)))
(times3 9)



