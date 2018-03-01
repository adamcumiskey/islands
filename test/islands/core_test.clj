(ns islands.core-test
  (:require [clojure.test :refer :all]
            [islands.core :refer :all]))

(def empty-ocean [[0 0 0 0 0]
                  [0 0 0 0 0]
                  [0 0 0 0 0]
                  [0 0 0 0 0]
                  [0 0 0 0 0]])

(def lone-island [[0 0 0 0 0]
                  [0 1 0 0 0]
                  [0 0 0 0 0]
                  [0 0 0 0 0]
                  [0 0 0 0 0]])

(def diagonal-islands [[1 0 0 0 0]
                       [0 1 0 0 0]
                       [0 0 0 1 0]
                       [0 0 1 0 0]
                       [0 1 0 0 0]])

(def normal [[1 0 0 0 1]
             [0 1 0 0 0]
             [0 1 0 0 0]
             [0 1 1 0 1]
             [0 1 0 0 1]])

(def all-land [[1 1 1 1 1]
               [1 1 1 1 1]
               [1 1 1 1 1]
               [1 1 1 1 1]
               [1 1 1 1 1]])

(deftest island-counts
  (testing "Empty ocean"
    (is (zero? (count-islands empty-ocean))))
  (testing "Lone island"
    (is (= 1 (count-islands lone-island))))
  (testing "Diagonal islands"
    (is (= 2 (count-islands diagonal-islands))))
  (testing "Normal case"
    (is (= 3 (count-islands normal))))
  (testing "Only Land"
    (is (= 1 (count-islands all-land)))))
