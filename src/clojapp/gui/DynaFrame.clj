(ns clojapp.gui.DynaFrame
  (:gen-class
   :name         clojapp.gui.DynaFrame
   :extends      javax.swing.JFrame
   :implements   [clojure.lang.IMeta]
   :prefix       df-
   :state        state
   :init         init
   :constructors {[String] [String]}
   :methods      [[display [java.awt.Container] void]
                  ^{:static true} [version [] String]])
  (:import (javax.swing JFrame JPanel)
           (java.awt BorderLayout Container)))

;; Implement init
(defn df-init [title]
  [[title] (atom {::title title})])

;; Implement meta()
(defn df-meta [this] @(.state this))

;; Implement version()
(defn version [] "1.0")

;; Implement display()
(defn df-display [this pane]
  (doto this
    (-> .getContentPane .removeAll) ;; remove existing pane
    (.setContentPane (doto (JPanel.) ;; add new pane
                       (.add pane BorderLayout/CENTER)))
    (.pack)
    (.setVisible true)))

;; At the repl
;; (compile 'clojapp.gui.DynaFrame)
;; At this point, you should see the generate class files on the classpath (i.e. in lein, see /target/)

;; Now you can do..
;; (ns clojapp.gui.DynaFrame)

;; (meta (clojapp.gui.DynaFrame. "3rd"))
;; ->  {:clojapp.gui.DynaFrame/title "3rd"}
;;(clojapp.gui.DynaFrame/version)
;; -> "1.0"

;; And to see the gui call:

;; (def gui (clojapp.gui.DynaFrame. "4th"))

;; (.display gui (doto (javax.swing.JPanel.)
;;                 (.add (javax.swing.JLabel. "Charlemagne and Pippin"))))

;; And to modify the gui while its running:
;; (.display gui (doto (javax.swing.JPanel.)
;;                 (.add (javax.swing.JLabel. "Mater semper certa est." ))))



