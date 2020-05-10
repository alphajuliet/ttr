(ns ttr.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [ubergraph.core :as uber]
            [ttr.graph :as gr]))

(deftest test-find
  (testing "Get routes"
    (let [g0 (gr/initial-map)]
      (is (= 202 (uber/count-edges g0)))
      (is (= 202 (count (gr/get-all-routes g0))))
      (is (= 16 (count (gr/get-routes g0 {:colour :red})))))))

(deftest test-update-attr
  (testing "Update an attribute"
    (let [g0 (gr/initial-map)
          e0 (first (gr/get-routes g0 {:src "Paris"}))
          g1 (gr/update-route g0 e0 :claimed-by 99)
          e1 (first (gr/get-routes g1 {:src "Paris"}))]
      (is (nil? (uber/attr g0 e0 :claimed-by)))
      (is (= 99 (uber/attr g1 e1 :claimed-by))))))

;; The End