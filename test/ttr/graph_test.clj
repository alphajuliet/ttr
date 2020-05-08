(ns ttr.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [ubergraph.core :as uber]
            [ttr.graph :as gr]))

(deftest test-find
  (testing "Find edges"
    (let [g0 (gr/initial-map)]
      (is (= 202 (uber/count-edges g0)))
      (is (= 202 (count (gr/get-all-routes g0)))))))

(deftest test-claim
  (testing "Claim an edge for Player #1"
    (let [g0 (gr/initial-map)
          e {:src "Paris" :dest "Bruxelles" :colour :red}
          g1 (gr/claim-edge g0 e 1)]
      (is (nil? (gr/claimed? g0 e)))
      (is (= 1 (gr/claimed? g1 e))))))

;; The End