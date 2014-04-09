(defproject org.clojars.mathias/clojure-mail "0.2-snapshot"
  :description "Clojure Email Library"
  :url "https://github.com/mathias/clojure-mail"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.jsoup/jsoup "1.7.3"] ;; for cleaning up messy html messages
                 [javax.mail/mail "1.4.4"]]
  :plugins [[lein-swank "1.4.4"]])
