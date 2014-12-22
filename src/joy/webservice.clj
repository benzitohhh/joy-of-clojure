(ns joy.webservice
  (:import (com.sun.net.httpserver HttpHandler HttpExchange HttpServer)
           (java.net InetSocketAddress HttpURLConnection)
           (java.io IOException FilterOutputStream)
           (java.util Arrays)))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn new-server [port path handler]
  (doto (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler [txt]
  (proxy [HttpHandler] [] ;; use proxy to implement a java interface (HttpHandler in this case)
                          ;; returns an instance of an anonmymous class
    (handle [exchange]
      (.sendResponseHeaders exchange HttpURLConnection/HTTP_OK 0)
      (doto (.getResponseBody exchange)
        (.write (.getBytes txt))
        (.close)))))

(def server (new-server 8123
                        "/joy/hello"
                        (default-handler "Hello there Cleveland")))

(.stop server 0)

;; Ok cool. But what if we want to change the message while the sevver is running?
(defn make-handler-fn [fltr txt]
  (fn [this exchange]
    (let [b (.getBytes txt)]
      (-> exchange
          .getResponseHeaders
          (.set "Content-Type" "text/html"))
      (.sendResponseHeaders exchange
                            HttpURLConnection/HTTP_OK
                            0)
      (doto (fltr (.getResponseBody exchange))
        (.write b)
        (.close)))))

(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
     (update-proxy p
                   {"handle" (make-handler-fn fltr txt)})))

(def p (default-handler
         "There's no problem that can't be solved
          with another level of indirection"))

(def server (new-server 8123 "/joy/hello" p))

(.stop server 0)

(change-message p "yayyy")

;; So... HttpServer -> Handler p -> handle()
;; Using update-proxy, we can modify the value of handle()

;; OK so we're up to page 209.....

(defn screaming-filter [o]
  (proxy [FilterOutputStream] [o] ;; Return instance of anon class that extends FilterOutputStream - with write() overridden
    (write [b]
      (proxy-super write (.getBytes (str "<strong>"
                                         (.toUpperCase (String. b))
                                         "</strong>"))))))

(def server (new-server 8123 "/joy/hello" p))

(change-message p screaming-filter "whisper")

(change-message p identity "whisper")

(.stop server 0)


;; Clojure’s proxy capabilities are truly dynamic, allowing you to create fully stubbed proxies using either construct-proxy, get-proxy-class, or init-proxy. In both cases, a partially to fully realized proxy will be constructed, allowing programmatic customization using update-proxy and arbitrary mixin maps.
;; There’s a universe of difference between the code outlined in this subsection and systems employing true code hot-loading, but it’s a reasonable facsimile. Using proxy is powerful, but doing so creates unnamed instances unavailable for later extension. If you instead wish to create named classes then you’ll need to use Clojure’s gen-class mechanism, which we’ll discuss next.


