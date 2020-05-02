(defproject ttr "0.1.0-SNAPSHOT"
  :description "Explorations with Ticket to Ride."
  :url "https://alphajuliet.com/ns/ttr"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot ttr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
