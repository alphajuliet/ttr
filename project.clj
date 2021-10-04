(defproject ttr "0.1.0-SNAPSHOT"
  :description "Explorations with Ticket to Ride."
  :url "https://alphajuliet.com/ns/ttr"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ubergraph "0.8.2"]
                 [csv-map "0.1.2"]
                 [funcool/cats "2.4.1"]
                 [random-seed "1.0.0"]
                 [org.clojure/test.check "1.1.0"]
                 [spec-dict "0.2.1"]]
  :main ^:skip-aot ttr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
