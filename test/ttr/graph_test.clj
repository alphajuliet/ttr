(ns ttr.graph-test
  (:require [clojure.test :refer :all]
            [ubergraph.core :as uber]
            [ttr.graph :refer :all]))

(deftest test-find
  (testing "Find edges"
    (let [g0 (initial-map)]
      (is (= 202 (uber/count-edges g0)))
      (is (= 202 (count (get-all-routes g0)))))))

(deftest test-claim
  (testing "Claim an edge for Player #1"
    (let [g0 (initial-map)
          e {:src "Paris" :dest "Bruxelles" :colour :red}
          g1 (claim-edge g0 e 1)]
      (is (nil? (claimed? g0 e)))
      (is (= 1 (claimed? g1 e))))))

;; The End