(ns joy.break
  (:require [clojure.main :refer [repl repl-read]]))

(defn readr [prompt exit-code]
  (let [input (repl-read prompt exit-code)]
    (if (= input ::t1)
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

;; (let [a 1, b 2, c 3]
;;   (let [b 200]
;;     (local-context)))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

;(contextual-eval {'a 2, 'b (* 2 6)} '(+ a b))

(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))

(defn div [n d] (break) (int (/ n d)))

(defn keys-apply [f ks m]
  (break)
  (let [only (select-keys m ks)]
    (break)
    (zipmap (keys only) (map f (vals only)))))

