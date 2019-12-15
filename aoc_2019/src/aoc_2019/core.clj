(ns aoc-2019.core
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [clojure.string :as str])
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
  [program]  
  (loop [result program i 0]
    (let [opcode (nth result i)
          op (case opcode
               1 +
               2 *
               99 nil
               "error")]
      (if (== opcode 99)
        result        
        (if (nil? op)
          (print "something went wrong")
          (recur (assoc result
                        (nth result (+ i 3))
                        (op (nth result (nth result (+ i 1)))
                            (nth result (nth result (+ i 2)))))
                 (+ i 4)))))))

(defn intcode-csv
  [program-csv]
  (let [result (intcode (vec (map #(Integer/parseInt %) (str/split program-csv #","))))]    
    (reduce #(str %1 "," %2) (first result) (rest result))))

;; orig input 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0
(defn run-solution-2-p1
  []
  ;; after doing '...replace position 1 with the value 12 and replace position 2 with the value 2':
  (intcode-csv "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0"))

(defn run-solution-2-p2
  []  
  (let [input-template [1 '_ '_ 3 1 1 2 3 1 3 4 3 1 5 0 3 2 9 1 19 1 19 5 23 1 23 6 27 2 9 27 31 1 5 31 35 1 35 10 39 1 39 10 43 2 43 9 47 1 6 47 51 2 51 6 55 1 5 55 59 2 59 10 63 1 9 63 67 1 9 67 71 2 71 6 75 1 5 75 79 1 5 79 83 1 9 83 87 2 87 10 91 2 10 91 95 1 95 9 99 2 99 9 103 2 10 103 107 2 9 107 111 1 111 5 115 1 115 2 119 1 119 6 0 99 2 0 14 0]]
    (let [solution-inputs 
          (first (for [pos-1-val (range 91)
                       pos-2-val (range 91)
                       :let [inputs (-> input-template
                                        (assoc 1 pos-1-val)
                                        (assoc 2 pos-2-val))
                             result (first (intcode inputs))]
                       :when (== result 19690720)]
                   [pos-1-val pos-2-val]))]
      (when (not (empty? solution-inputs))
        (let [[noun verb] solution-inputs]          
          (+ verb (* 100 noun)))))))

(defn manhatten-distance
  [p1 p2]
  (let [p1x (first p1)
        p1y (second p1)
        p2x (first p2)
        p2y (second p2)]
    (+ (Math/abs (- p2x p1x)) (Math/abs (- p2y p1y)))))

(defn to-coordinates
  "Converts a path string into a sequence of coordinates."
  [path-str]
  (let [path (str/split path-str #",")]
    (reduce (fn [p1 p2]
              (let [coordinates (let [direction (first p2)
                                      value (Integer/parseInt (subs p2 1))
                                      prev-coord (first (last p1))
                                      prev-cum-sum (second (last p1))                                      
                                      prev-coord-x (first prev-coord)
                                      prev-coord-y (second prev-coord)]
                                  (if (= direction \U)
                                    (map (fn [step] [[prev-coord-x (+ prev-coord-y step)] (+ prev-cum-sum step)])
                                         (range 1 (inc value)))
                                    (if (= direction \D)
                                      (map (fn [step] [[prev-coord-x (+ prev-coord-y (* -1 step))] (+ prev-cum-sum step)])
                                           (range 1 (inc value)))                                      
                                      (if (= direction \L)
                                        (map (fn [step] [[(+ prev-coord-x (* -1 step)) prev-coord-y] (+ prev-cum-sum step)])
                                             (range 1 (inc value)))                                        
                                        (if (= direction \R)
                                          (map (fn [step] [[(+ prev-coord-x step) prev-coord-y] (+ prev-cum-sum step)])
                                               (range 1 (inc value)))                                          
                                          nil)))))]
                (if coordinates
                  (apply conj p1 coordinates)
                  p1)))
            [[[0 0] 0]]
            path)))

(defn coordinates->map
  [coordinates]
  (reduce (fn [coords-map coord]
            (let [pt (first coord)
                  cum-sum (second coord)                  
                  existing-coord (get coords-map pt)]
              (if existing-coord
                coords-map ; don't overwrite existing entry
                (assoc coords-map pt {:point pt :cum-sum cum-sum}))))
          {}
          coordinates))

(defn intersections
  [coords-map-1 coords-map-2]
  (let [map-1-keys (keys coords-map-1)]
    (for [key map-1-keys
          :let [match (get coords-map-2 key)]
          :when (not (nil? match))]
      (let [coord-1 (get coords-map-1 key)]
        {:point key :cum-sum (+ (:cum-sum coord-1) (:cum-sum match)) :manhatten-distance (manhatten-distance [0 0] key)}))))

(defn run-solution-3-p1-test-1
  []
  (-> (->> (intersections (-> "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "U62,R66,U55,R34,D71,R55,D58,R83"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :manhatten-distance))
      second ; because the first result is origin and we want to ignore that
      :manhatten-distance))

(defn run-solution-3-p1-test-2
  []
  (-> (->> (intersections (-> "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :manhatten-distance))
      second ; because the first result is origin and we want to ignore that
      :manhatten-distance))

(defn run-solution-3-p1
  []
  (-> (->> (intersections (-> "R1005,D32,R656,U228,L629,U59,L558,D366,L659,D504,R683,U230,R689,U489,R237,U986,L803,U288,R192,D473,L490,U934,L749,D631,L333,U848,L383,D363,L641,D499,R926,D945,L520,U311,R75,D414,L97,D338,L754,U171,R601,D215,R490,U164,R158,U499,L801,U27,L671,D552,R406,U168,R12,D321,L97,U27,R833,U503,R950,U432,L688,U977,R331,D736,R231,U301,L579,U17,R984,U399,L224,U100,L266,U184,R46,D989,L851,D739,R45,D231,R893,D372,L260,U26,L697,U423,L716,D573,L269,U867,R722,U193,R889,D322,L743,U371,L986,D835,R534,U170,R946,U271,L514,D521,L781,U390,L750,D134,L767,U599,L508,U683,L426,U433,L405,U10,L359,D527,R369,D365,L405,D812,L979,D122,L782,D460,R583,U765,R502,D2,L109,D69,L560,U76,R130,D794,R197,D113,L602,D123,L190,U246,L407,D957,L35,U41,L884,D591,R38,D911,L269,D204,R332,U632,L826,D202,L984,U153,L187,U472,R272,U232,L786,U932,L618,U104,R632,D469,L868,D451,R261,U647,L211,D781,R609,D549,L628,U963,L917,D716,L218,U71,L148,U638,R34,U133,R617,U312,L215,D41,L673,U643,R379,U486,L273,D539,L294,D598,L838,D60,L158,U817,R207,U825,L601,D786,R225,D89,L417,U481,L416,U133,R261,U405,R109,U962,R104,D676,R966,U138,L343,U14,L82,U564,R73,D361,R678,D868,L273,D879,R629,U164,R228,U949,R504,D254,L662,D726,R126,D437,R569,D23,R246,U840,R457,D429,R296,U110,L984,D106,L44,U264,R801,D350,R932,D334,L252,U714,L514,U261,R632,D926,R944,U924,R199,D181,L737,U408,R636,U57,L380,D949,R557,U28,L432,D83,R829,D865,L902,D351,R71,U704,R477,D501,L882,D75,R325,D53,L990,U460,R165,D82,R577,D788,R375,U264,L178,D193,R830,D343,L394"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "L1003,U125,L229,U421,R863,D640,L239,U580,R342,U341,R989,U732,R51,U140,L179,U60,R483,D575,R49,U220,L284,U336,L905,U540,L392,U581,L570,U446,L817,U694,R923,U779,R624,D387,R495,D124,R862,D173,R425,D301,L550,D605,R963,U503,R571,U953,L878,D198,L256,D77,R409,D752,R921,D196,R977,U86,L842,U155,R987,D39,L224,U433,L829,D99,R558,U736,R645,D335,L52,D998,L613,D239,R470,U79,R839,D71,L753,U127,R135,D429,R729,U71,L151,U875,R668,D220,L501,D822,R306,D557,R461,U942,R59,U14,R353,D546,R409,D261,R204,U873,L847,U936,R611,U487,R474,U406,R818,U838,L301,D684,R861,D738,L265,D214,R272,D702,L145,U872,R345,D623,R200,D186,R407,U988,L608,U533,L185,D287,L549,U498,L630,U295,L425,U517,L263,D27,R697,U177,L615,U960,L553,U974,L856,U716,R126,D819,L329,D233,L212,U232,L164,D712,R316,D682,L641,U676,L535,U783,R39,U953,R39,U511,R837,U325,R391,U401,L642,U435,R626,U801,R876,D849,R448,D8,R74,U238,L186,D558,L648,D258,R262,U7,L510,U178,L183,U415,L631,D162,L521,D910,R462,U789,R885,D822,R908,D879,R614,D119,L570,U831,R993,U603,L118,U764,L414,U39,R14,U189,L415,D744,R897,U714,R326,U348,R822,U98,L357,D478,L464,D851,L545,D241,L672,U197,R156,D916,L246,U578,R4,U195,R82,D402,R327,D429,R119,U661,L184,D122,R891,D499,L808,U519,L36,U323,L259,U479,L647,D354,R891,D320,R653,U772,L158,U608,R149,U564,L164,D998,L485,U107,L145,U834,R846,D462,L391,D661,R841,U742,L597,D937,L92,U877,L350,D130,R684,U914,R400,D910,L739,U789,L188,U256,R10,U258,L965,U942,R234,D106,R852,U108,R732,U339,L955,U271,L340,U23,R373,D100,R137,U648,L130"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :manhatten-distance))
      second ; because the first result is origin and we want to ignore that
      :manhatten-distance))

(defn run-solution-3-p2-test-1
  []
  (-> (->> (intersections (-> "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "U62,R66,U55,R34,D71,R55,D58,R83"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :cum-sum))
      second ; because the first result is origin and we want to ignore that
      :cum-sum))

(defn run-solution-3-p2-test-2
  []
  (-> (->> (intersections (-> "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :cum-sum))
      second ; because the first result is origin and we want to ignore that
      :cum-sum))

(defn run-solution-3-p2
  []
  (-> (->> (intersections (-> "R1005,D32,R656,U228,L629,U59,L558,D366,L659,D504,R683,U230,R689,U489,R237,U986,L803,U288,R192,D473,L490,U934,L749,D631,L333,U848,L383,D363,L641,D499,R926,D945,L520,U311,R75,D414,L97,D338,L754,U171,R601,D215,R490,U164,R158,U499,L801,U27,L671,D552,R406,U168,R12,D321,L97,U27,R833,U503,R950,U432,L688,U977,R331,D736,R231,U301,L579,U17,R984,U399,L224,U100,L266,U184,R46,D989,L851,D739,R45,D231,R893,D372,L260,U26,L697,U423,L716,D573,L269,U867,R722,U193,R889,D322,L743,U371,L986,D835,R534,U170,R946,U271,L514,D521,L781,U390,L750,D134,L767,U599,L508,U683,L426,U433,L405,U10,L359,D527,R369,D365,L405,D812,L979,D122,L782,D460,R583,U765,R502,D2,L109,D69,L560,U76,R130,D794,R197,D113,L602,D123,L190,U246,L407,D957,L35,U41,L884,D591,R38,D911,L269,D204,R332,U632,L826,D202,L984,U153,L187,U472,R272,U232,L786,U932,L618,U104,R632,D469,L868,D451,R261,U647,L211,D781,R609,D549,L628,U963,L917,D716,L218,U71,L148,U638,R34,U133,R617,U312,L215,D41,L673,U643,R379,U486,L273,D539,L294,D598,L838,D60,L158,U817,R207,U825,L601,D786,R225,D89,L417,U481,L416,U133,R261,U405,R109,U962,R104,D676,R966,U138,L343,U14,L82,U564,R73,D361,R678,D868,L273,D879,R629,U164,R228,U949,R504,D254,L662,D726,R126,D437,R569,D23,R246,U840,R457,D429,R296,U110,L984,D106,L44,U264,R801,D350,R932,D334,L252,U714,L514,U261,R632,D926,R944,U924,R199,D181,L737,U408,R636,U57,L380,D949,R557,U28,L432,D83,R829,D865,L902,D351,R71,U704,R477,D501,L882,D75,R325,D53,L990,U460,R165,D82,R577,D788,R375,U264,L178,D193,R830,D343,L394"
                              (to-coordinates)
                              (coordinates->map))
                          (-> "L1003,U125,L229,U421,R863,D640,L239,U580,R342,U341,R989,U732,R51,U140,L179,U60,R483,D575,R49,U220,L284,U336,L905,U540,L392,U581,L570,U446,L817,U694,R923,U779,R624,D387,R495,D124,R862,D173,R425,D301,L550,D605,R963,U503,R571,U953,L878,D198,L256,D77,R409,D752,R921,D196,R977,U86,L842,U155,R987,D39,L224,U433,L829,D99,R558,U736,R645,D335,L52,D998,L613,D239,R470,U79,R839,D71,L753,U127,R135,D429,R729,U71,L151,U875,R668,D220,L501,D822,R306,D557,R461,U942,R59,U14,R353,D546,R409,D261,R204,U873,L847,U936,R611,U487,R474,U406,R818,U838,L301,D684,R861,D738,L265,D214,R272,D702,L145,U872,R345,D623,R200,D186,R407,U988,L608,U533,L185,D287,L549,U498,L630,U295,L425,U517,L263,D27,R697,U177,L615,U960,L553,U974,L856,U716,R126,D819,L329,D233,L212,U232,L164,D712,R316,D682,L641,U676,L535,U783,R39,U953,R39,U511,R837,U325,R391,U401,L642,U435,R626,U801,R876,D849,R448,D8,R74,U238,L186,D558,L648,D258,R262,U7,L510,U178,L183,U415,L631,D162,L521,D910,R462,U789,R885,D822,R908,D879,R614,D119,L570,U831,R993,U603,L118,U764,L414,U39,R14,U189,L415,D744,R897,U714,R326,U348,R822,U98,L357,D478,L464,D851,L545,D241,L672,U197,R156,D916,L246,U578,R4,U195,R82,D402,R327,D429,R119,U661,L184,D122,R891,D499,L808,U519,L36,U323,L259,U479,L647,D354,R891,D320,R653,U772,L158,U608,R149,U564,L164,D998,L485,U107,L145,U834,R846,D462,L391,D661,R841,U742,L597,D937,L92,U877,L350,D130,R684,U914,R400,D910,L739,U789,L188,U256,R10,U258,L965,U942,R234,D106,R852,U108,R732,U339,L955,U271,L340,U23,R373,D100,R137,U648,L130"
                              (to-coordinates)
                              (coordinates->map)))
           (sort-by :cum-sum))
      second ; because the first result is origin and we want to ignore that
      :cum-sum))

(defn is-password-p1
  [x]
  (loop [chars (seq (str x))
         prev-char nil
         contains-double false
         increasing true]
    (let [current-char (first chars)]      
      (if (empty? chars)
        (and contains-double increasing)
        (if (nil? prev-char)
          (recur (rest chars) current-char contains-double increasing)
          (let [prev-val (Character/digit prev-char 10)
                current-val (Character/digit current-char 10)]
            (recur (rest chars)
                   current-char
                   (or contains-double (== prev-val current-val))
                   (and increasing (<= prev-val current-val)))))))))

(defn run-solution-4-p1
  []
  (count (filter is-password-p1 (range 236491 713787 1))))

(defn contains-double
  [m]
  (loop [keys (keys m)
         double-found false]
    (if (empty? keys)
      double-found
      (recur (rest keys) (or double-found
                             (== (get m (first keys)) 1))))))

(defn is-password-p2
  [x]
  (loop [chars (seq (str x))
         prev-char nil         
         increasing true
         repeats {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0}]
    (let [current-char (first chars)]      
      (if (empty? chars)
        (and increasing
             (contains-double repeats))        
        (if (nil? prev-char)
          (recur (rest chars) current-char increasing repeats)
          (let [prev-val (Character/digit prev-char 10)
                current-val (Character/digit current-char 10)]
            (let [matches-prev (== prev-val current-val)]
              (recur (rest chars)
                     current-char                     
                     (and increasing (<= prev-val current-val))
                     (if matches-prev
                       (update repeats current-val inc)
                       repeats)))))))))

(defn run-solution-4-p2
  []
  (count (filter is-password-p2 (range 236491 713787 1))))

(defn param-modes
  [opcode-chars]
  (case (count opcode-chars)
    5 [(Character/digit (nth opcode-chars 2) 10)
       (Character/digit (nth opcode-chars 1) 10)
       (Character/digit (nth opcode-chars 0) 10)]
    4 [(Character/digit (nth opcode-chars 1) 10)
       (Character/digit (nth opcode-chars 0) 10)
       0]
    3 [(Character/digit (nth opcode-chars 0) 10)
       0
       0]
    2 [0 0 0]
    1 [0 0 0]))

(defn param-val
  "Returns the value of a parameter based on its mode (position or immediate)."
  [inputs i param-mode offset]
  (if (== param-mode 0)               
    (nth inputs (nth inputs (+ i offset))) ; position mode
    (nth inputs (+ i offset))))            ; immediate mode

(defn perform-op
  [inputs i op param-1-mode param-2-mode]
  (assoc inputs
         (nth inputs (+ i 3))
         (op (param-val inputs i param-1-mode 1)
             (param-val inputs i param-2-mode 2))))

(defn intcode-5
  [program input]  
  (loop [program program
         i 0 ; instruction pointer
         terminate false]
    (when (not terminate)      
      (let [opcode-str (str (nth program i))
            opcode-chars (seq opcode-str)]        
        (cond
          (str/ends-with? opcode-str "1") ; add
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (recur (perform-op program i + param-1-mode param-2-mode)
                   (+ i 4)
                   false))
          
          (str/ends-with? opcode-str "2") ; mult
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (recur (perform-op program i * param-1-mode param-2-mode)
                   (+ i 4)
                   false))

          (str/ends-with? opcode-str "3") ; read an input val          
          (recur (assoc program (nth program (+ i 1)) input)
                 (+ i 2)
                 false)

          (str/ends-with? opcode-str "4") ; write
          (let [[param-1-mode] (param-modes opcode-chars)]            
            (print (param-val program i param-1-mode 1) " ")
            (recur program (+ i 2) false))

          (str/ends-with? opcode-str "5") ; jump-if-true
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (let [jump-test (param-val program i param-1-mode 1)]
              (if (not (== jump-test 0))
                (recur program (param-val program i param-2-mode 2) false)
                (recur program (+ i 3) false))))          

          (str/ends-with? opcode-str "6") ; jump-if-false
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (let [jump-test (param-val program i param-1-mode 1)]
              (if (== jump-test 0)
                (recur program (param-val program i param-2-mode 2) false)
                (recur program (+ i 3) false))))          

          (str/ends-with? opcode-str "7") ; less than
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]            
            (recur (assoc program
                          (nth program (+ i 3))
                          (if (< (param-val program i param-1-mode 1)
                                 (param-val program i param-2-mode 2))
                            1
                            0))
                   (+ i 4)
                   false))

          (str/ends-with? opcode-str "8") ; equals
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]            
            (recur (assoc program
                          (nth program (+ i 3))
                          (if (== (param-val program i param-1-mode 1)
                                  (param-val program i param-2-mode 2))
                            1
                            0))
                   (+ i 4)
                   false))

          (str/ends-with? opcode-str "99")
          (recur nil nil true))))))

(defn intcode-csv-5
  [program-csv input]  
  (intcode-5 (vec (map #(Integer/parseInt %) (str/split program-csv #","))) input))

(def solution-5-input "3,225,1,225,6,6,1100,1,238,225,104,0,1101,34,7,225,101,17,169,224,1001,224,-92,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1102,46,28,225,1102,66,83,225,2,174,143,224,1001,224,-3280,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,19,83,224,101,-102,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,114,17,224,1001,224,-63,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,60,46,225,1101,7,44,225,1002,40,64,224,1001,224,-1792,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,80,27,225,1,118,44,224,101,-127,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,75,82,225,1101,40,41,225,1102,22,61,224,1001,224,-1342,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,102,73,14,224,1001,224,-511,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,389,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1108,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,434,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,449,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,464,101,1,223,223,108,226,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,494,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,509,1001,223,1,223,7,677,226,224,1002,223,2,223,1006,224,524,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,569,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,7,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,629,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,644,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,659,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226")

(defn run-solution-5-p1
  []
  (intcode-csv-5 solution-5-input 1))

(defn run-solution-5-p2
  []
  (intcode-csv-5 solution-5-input 5))

(defn orbital-map-str->map
  [orbital-map-str]
  (reduce (fn [orbital-map orbital-relationship-str]
            (let [[parent child] (str/split orbital-relationship-str #"\)")]
              (assoc orbital-map child parent)))
          {}
          (line-seq (BufferedReader. (StringReader. orbital-map-str)))))

(defn count-orbits-to-parent
  [object parent-pred orbital-map]
  (loop [count 0
         parent (get orbital-map object)]
    (if (parent-pred parent) 
      count
      (recur (inc count) (get orbital-map parent)))))

(defn parents
  [obj orbital-map]
  (loop [parents []
         parent (get orbital-map obj)]
    (if (nil? parent)
      parents
      (recur (conj parents parent) (get orbital-map parent)))))

(defn common-parent
  [obj1 obj2 orbital-map]
  (first (for [parent1 (parents obj1 orbital-map)
               parent2 (parents obj2 orbital-map)
               :when (= parent1 parent2)]
           parent1)))

(defn count-all-orbits
  [orbital-map]
  (loop [count 0
         objects (keys orbital-map)]
    (if (empty? objects)
      count
      (recur (+ count (count-orbits-to-parent (first objects) nil? orbital-map)) (rest objects)))))

(defn run-solution-6-p1
  []
  (count-all-orbits (orbital-map-str->map (slurp (resource "orbital_map.txt")))))

(defn transfers
  [start end]
  )

(defn run-solution-6-p2
  []  
  (let [orbital-map (orbital-map-str->map (slurp (resource "orbital_map.txt")))
        common-parent (common-parent "SAN" "YOU" orbital-map)]
    (+ (count-orbits-to-parent "SAN" #(= % common-parent) orbital-map)
       (count-orbits-to-parent "YOU" #(= % common-parent) orbital-map))))

(defn phase-setting-permutations
  [range-start range-end]
  (for [a (range range-start range-end)
        b (range range-start range-end)
        c (range range-start range-end)
        d (range range-start range-end)
        e (range range-start range-end)
        :let [v [a b c d e]
              sv (set v)]
        :when (== (count sv) 5)]
    v))

(defn intcode-7
  [program phase-setting input instruction-pointer output-value]  
  (loop [program program
         terminate false
         halted false
         i instruction-pointer
         phase-setting phase-setting
         input input
         output-value output-value]
    (if terminate
      [program output-value halted i]
      (let [opcode-str (str (nth program i))
            opcode-chars (seq opcode-str)]        
        (cond
          (str/ends-with? opcode-str "1") ; add
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (recur (perform-op program i + param-1-mode param-2-mode)
                   false
                   false
                   (+ i 4)
                   phase-setting
                   input
                   output-value))
          
          (str/ends-with? opcode-str "2") ; mult
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (recur (perform-op program i * param-1-mode param-2-mode)
                   false
                   false
                   (+ i 4)
                   phase-setting
                   input
                   output-value))

          (str/ends-with? opcode-str "3") ; read an input val          
          (recur (assoc program
                        (nth program (+ i 1))
                        (if phase-setting phase-setting input))
                 false
                 false
                 (+ i 2)
                 nil
                 input
                 output-value)

          (str/ends-with? opcode-str "4") ; write
          (let [[param-1-mode] (param-modes opcode-chars)]
            (recur program
                   true
                   false
                   (+ i 2)
                   phase-setting
                   input
                   (param-val program i param-1-mode 1)))

          (str/ends-with? opcode-str "5") ; jump-if-true
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (let [jump-test (param-val program i param-1-mode 1)]
              (if (not (== jump-test 0))
                (recur program
                       false
                       false
                       (param-val program i param-2-mode 2)
                       phase-setting
                       input
                       output-value)
                (recur program
                       false
                       false
                       (+ i 3)
                       phase-setting
                       input
                       output-value))))          

          (str/ends-with? opcode-str "6") ; jump-if-false
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]
            (let [jump-test (param-val program i param-1-mode 1)]
              (if (== jump-test 0)
                (recur program
                       false
                       false
                       (param-val program i param-2-mode 2)
                       phase-setting
                       input
                       output-value)
                (recur program
                       false
                       false
                       (+ i 3)
                       phase-setting
                       input
                       output-value))))          

          (str/ends-with? opcode-str "7") ; less than
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]            
            (recur (assoc program                          
                          (nth program (+ i 3))
                          (if (< (param-val program i param-1-mode 1)
                                 (param-val program i param-2-mode 2))
                            1
                            0))
                   false
                   false
                   (+ i 4)                   
                   phase-setting
                   input
                   output-value))

          (str/ends-with? opcode-str "8") ; equals
          (let [[param-1-mode param-2-mode] (param-modes opcode-chars)]            
            (recur (assoc program                          
                          (nth program (+ i 3))
                          (if (== (param-val program i param-1-mode 1)
                                  (param-val program i param-2-mode 2))
                            1
                            0))
                   false
                   false
                   (+ i 4)                   
                   phase-setting
                   input
                   output-value))

          (str/ends-with? opcode-str "99")
          (recur program true true nil nil nil output-value))))))

(defn thruster-signal
  [amp-program phase-setting-sequence]
  (let [amp-a {:amp :a :prog amp-program :ptr 0 :phase-setting (nth phase-setting-sequence 0) :input 0   :halted false :output nil :next-amp :b}
        amp-b {:amp :b :prog amp-program :ptr 0 :phase-setting (nth phase-setting-sequence 1) :input nil :halted false :output nil :next-amp :c}
        amp-c {:amp :c :prog amp-program :ptr 0 :phase-setting (nth phase-setting-sequence 2) :input nil :halted false :output nil :next-amp :d}
        amp-d {:amp :d :prog amp-program :ptr 0 :phase-setting (nth phase-setting-sequence 3) :input nil :halted false :output nil :next-amp :e}
        amp-e {:amp :e :prog amp-program :ptr 0 :phase-setting (nth phase-setting-sequence 4) :input nil :halted false :output nil :next-amp :a}]
    (loop [amp-programs {:a amp-a, :b amp-b, :c amp-c, :d amp-d, :e amp-e}
           current-amp-key :a]      
      (let [current-amp (get amp-programs current-amp-key)]                                        
        (if (:halted current-amp)
          (if (= current-amp-key :e)
            (:output current-amp)
            (recur amp-programs (:next-amp current-amp)))
          (let [[result-program output halted inst-ptr] (intcode-7 (:prog current-amp)
                                                                   (:phase-setting current-amp)                                                                   
                                                                   (:input current-amp)
                                                                   (:ptr current-amp)
                                                                   (:output current-amp))                
                amp-programs (-> amp-programs
                                 (assoc-in [current-amp-key :output] output)
                                 (assoc-in [current-amp-key :prog] result-program)
                                 (assoc-in [current-amp-key :ptr] inst-ptr)
                                 (assoc-in [current-amp-key :halted] halted)
                                 (assoc-in [current-amp-key :phase-setting] nil)
                                 (assoc-in [(:next-amp current-amp) :input] output))]            
            (recur amp-programs (:next-amp current-amp))))))))

(defn max-thruster-signal
  [amp-program phase-setting-range-start phase-setting-range-end]
  (loop [phase-setting-permutations (phase-setting-permutations phase-setting-range-start phase-setting-range-end)         
         max-thruster-signal 0]
    (if (empty? phase-setting-permutations)
      max-thruster-signal
      (let [phase-setting (first phase-setting-permutations)
            thruster-signal (thruster-signal amp-program phase-setting)]
        (if (> thruster-signal max-thruster-signal)
          (recur (next phase-setting-permutations) thruster-signal)
          (recur (next phase-setting-permutations) max-thruster-signal))))))

(def day-7-program "3,8,1001,8,10,8,105,1,0,0,21,34,59,68,85,102,183,264,345,426,99999,3,9,101,3,9,9,102,3,9,9,4,9,99,3,9,1002,9,4,9,1001,9,2,9,1002,9,2,9,101,5,9,9,102,5,9,9,4,9,99,3,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,102,3,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99")

(defn run-solution-7-p1
  []
  (max-thruster-signal (vec (map #(Integer/parseInt %) (str/split day-7-program #","))) 0 5))

(defn run-solution-7-p2
  []
  (max-thruster-signal (vec (map #(Integer/parseInt %) (str/split day-7-program #","))) 5 10))

(def input-8 (slurp (resource "input_8.txt")))

(defn render-layer
  [digits]
  (partition 6 (partition 25 digits)))

(defn layers
  [input-str]
  (partition (* 25 6) (map #(Character/digit % 10) (seq input-str))))

(defn indexed-layers
  [input-str]
  (let [layers (layers input-str)]
    (map-indexed (fn [idx layer] [idx layer]) layers)))  

(defn count-digits-in-layer
  [[layer-idx layer] digit]  
  {:layer-idx layer-idx 
   :count (count (filter #(== %1 digit) layer))})

(defn run-solution-8-p1
  []
  (let [indexed-layers (indexed-layers input-8)
        m (first (sort-by :count (map #(count-digits-in-layer %1 0) indexed-layers)))
        indexed-layer (nth indexed-layers (:layer-idx m))]
    (* (:count (count-digits-in-layer indexed-layer 1))
       (:count (count-digits-in-layer indexed-layer 2)))))

(defn render-pixel
  [& pixels]
  (first (filter #(not= % 2) pixels)))

(defn run-solution-8-p2
  []
  (render-layer (apply map render-pixel (layers input-8))))


  
