(ns ttr.state-test
  (:require [clojure.test :refer [deftest testing is]]
            [ttr.state :as st]))

(deftest test-tickets
  (testing "Read tickets"
    (is (= 46 (count (st/read-tickets))))))

(deftest test-init-state
  (testing "Initialise state"
    (let [s0 (st/empty-state 3)]
      (is (= 3 (count (:player s0)))))))

;; The End