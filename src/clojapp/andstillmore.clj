(ns clojapp.andstillmore
  (:gen-class))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(in-ns 'joy.ns)
(def authors ["Chouser"])

(in-ns 'your-ns)
(clojure.core/refer 'joy.ns)

(in-ns 'joy.ns)
(def authors ["Choser" "Fogus"])

(in-ns 'your-ns)

(def b (create-ns 'bonobo)) ;; only creates mappings for java.lang (not clojure.core)

(intern b 'x 9)

;; so need to intern the clojure.core functions
(intern b 'reduce clojure.core/reduce)


;; Steve Yegge's Universal Design Pattern (aka prototypical inheritance)
(ns joy.udp
  (:refer-clojure :exclude [get]))

(defn beget [o p] (assoc o ::prototype p))

(beget {:sub 0} {:super 1})
;=> {:joy.udp/prototype {:super 1}, :sub 0}

(def put assoc)

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(get (beget {:sub 0} {:super 1})
     :super)

(def cat {:likes-dogs true, :ocd-bathing true})

(def morris (beget {:likes-9lives true} cat))

(def post-traumatic-morris (beget {:likes-dogs nil} morris))

(get cat :likes-dogs)                    ;=> true
(get morris :likes-dogs)                 ;=> true
(get post-traumatic-morris :likes-dogs)  ;=> nil

(defmulti  compiler :os)

(defmethod compiler ::unix [m] (get m :c-compiler))

(defmethod compiler ::osx  [m] (get m :c-compiler))

(def clone (partial beget {}))

(def unix {:os ::unix, :c-compiler "cc", :home "/home", :dev "/dev"})

(def osx (-> (clone unix)
             (put :os ::osx)
             (put :c-compiler "gcc")
             (put :home "/Users")))

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))

(home unix)

(home osx) ;; exception

(derive ::osx ::unix) ;; establishes relationship "osx" is a "unix"

(home osx) ;; now this works

(parents ::osx)
(ancestors ::osx)
(descendants ::unix)
(isa? ::osx ::unix)

(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")

(home osx) ;; Throws exception becuase multiple methods for dispatch value osx

(prefer-method home ::unix ::bsd)

(home osx) ;; now this works ok

(remove-method home ::bsd)

(derive (make-hierarchy) ::osx ::unix)


(defmulti  compile-cmd  (juxt :os compiler))

(defmethod compile-cmd [::osx "gcc"] [m]
  (str "/usr/bin/" (get m :c-compiler)))

(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))




;; Records and protocols

(defrecord TreeNode [val l r])

(defn xconj [t v]
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else          (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)
                                        ;=> (2 3 4 5 6)



(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value)))

(xseq (fixo-push sample-tree 5/2))
                                        ;=> (2 5/2 3 4 5 6)

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value)))

;; Why would we want to extend the protocol to nil?...
(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))

;; .... so that we can do stuff like this:
(xseq (reduce fixo-push nil [3 5 2 4 6 0]))
;=> (0 2 3 4 5 6)


;; The full implementation

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value)) ;; Delegate to xconj
  (fixo-peek [node]
    (if (:l node) 
      (recur (:l node)) ;; Walk down left nodes to find smallest
      (:val node)))
  (fixo-pop [node] ;; Build new path down left to removed item
    (if (:l node)
      (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
      (:r node))))

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value] ;; fixo-push is vector's conj
    (conj vector value))
  (fixo-peek [vector] ;; peek is peek
    (peek vector))
  (fixo-pop [vector]  ;; pop is pop
    (pop vector)))

(xseq (fixo-pop (fixo-pop (fixo-pop (reduce fixo-push nil [3 5 2 4 6 0])))))

;; ok.... we're up to listing 9.4 (page 230)


;; An alternative to defrecord, is to use reify... (an re-use existing structure underneath - in this case a vector)
;; The below  implemenation has a size limit
(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
     (reify FIXO
       (fixo-push [this value]
         (if (< (count vector) limit)
           (fixed-fixo limit (conj vector value))
           this))
       (fixo-peek [_]
         (peek vector))
       (fixo-pop [_]
         (pop vector)))))

(-> (fixed-fixo 2)
    (fixo-push 1)
    (fixo-push 2)
    (fixo-push 3)
    (fixo-peek))
; -> 2
