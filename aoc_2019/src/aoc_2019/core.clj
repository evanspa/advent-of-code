(ns aoc-2019.core
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]])
  (:import (java.io BufferedReader StringReader)))

(defn required-fuel
  [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn required-fuel-with-fuel
  [mass]
  (loop [sum 0 remaining-mass mass]
    (if (<= remaining-mass 0)
      sum
      (let [reqd-fuel-val (required-fuel remaining-mass)]
        (recur
         (if (> reqd-fuel-val 0)
           (+ sum reqd-fuel-val)
           sum)
         reqd-fuel-val)))))

(defn- sum-all
  [reqd-fuel-fn]
  (reduce (fn [sum val]
            (+ sum (reqd-fuel-fn (Integer/parseInt val))))
          0
          (line-seq (BufferedReader. (StringReader. (slurp (resource "input_1.txt")))))))

(defn sum-all-required-fuel
  []
  (sum-all required-fuel))

(defn sum-all-required-fuel-with-fuel
  []
  (sum-all required-fuel-with-fuel))

(defn -main [& args]
  (is (== 2 (required-fuel 12)))
  (is (== 2 (required-fuel 12)))
  (is (== 654 (required-fuel 1969)))
  (is (== 33583 (required-fuel 100756))))

(defn intcode
  [ints-csv]
  (loop [result (vec (map #(Integer/parseInt %) (str/split ints-csv #",")))
         i 0]
    (let [opcode (nth result i)
          op (case opcode
               1 +
               2 *
               99 nil
               "error")]
      (if (== opcode 99)
        (reduce #(str %1 "," %2) (first result) (rest result))
        (if (nil? op)
          (print "something went wrong")
          (recur (assoc result
                        (nth result (+ i 3))
                        (op (nth result (nth result (+ i 1)))
                            (nth result (nth result (+ i 2)))))
                 (+ i 4)))))))




