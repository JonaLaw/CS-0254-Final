;; gorilla-repl.fileformat = 1

;; **
;;; # Unnecessarily Complex Symbolic Regression
;;;
;; **


;; @@
(ns worksheets.simple-symbolic-regression
  (:require [gorilla-plot.core :as plot]
            [clojure.zip :as zip]
            ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; This code defines and runs a genetic programming system on the problem
;;; of finding a function that fits a particular set of [x y] pairs.
;;; The evolve function will try to find the function for the data-set labled target-data.
;; **

;; @@
;equation: 2x^2
;solution: (* (+ x x) x)
(def target-data2
  [[1 2]
   [2 8]
   [3 18]
   [4 32]])

(def target-data3
  [[-10 64]
   [-6 16]
   [-4 4]
   [-2 0]
   [0 4]
   [1 9]
   [3 25]
   [4 36]
   [6 64]
   [8 100]])

; amherst temp data
(def target-data
[[0 -12.67]
[2 -8.6]
[3 -9.95]
[1 -8.61]
[2.6 -8.98]
[3.6 -9.97]
[1.8 -7.78]
[2.2 -8.63]
[3.2 -9.84]
[0.2 -11.61]
[0.4 -10.85]
[0.8 -9.17]
[1.6 -7.92]
[2.8 -8.85]
[3.8 -9.94]
[1.4 -7.53]
[2.4 -8.85]
[3.4 -9.81]
[0.6 -9.53]
[1.2 -7.8]])
;; @@

;; @@
(def hyperparameters {:population-size 400 ; How many inidivuals are in the population.
                      :error-threshold 0.1 ; When an individual's error reaches below this, it is considered a solution.
                      
                      ; The next 3 parameters should sum to 1.
                      ;:percent-mutation 1/4 ; What percentage of children should come from mutation each generation.
                      :percent-crossover 3/4 ; What percentage of children should come from crossover each generation.
                      :percent-cloned 1/4}) ; What percentage of children should be clones of their parents each generation.


;(def function-table (zipmap '(+ - * pd)
;                            '(2 2 2 2)))

(def function-table (zipmap '(+ - * pd sin cos)
                            '(2 2 2 2  1   1)))
                            
;(def function-table (zipmap '(+ - * pd log sqrt sin cos)
;                            '(2 2 2 2  1   1    1   1)))


(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

(defn sin [n] (Math/sin n))

(defn cos [n] (Math/cos n))

;(defn sqrt
;  "Square Root"
;  [x]
;  (if (neg? x)
;    0
;    (Math/sqrt x)))

;(defn log
;  "Natural logarithm"
;  [x]
;  (if (> 1 x)
;    0
;    (Math/log x)))

(defn random-function 
  "Returns a random function from function-table."
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  "Returns a random terminal. Either 'x' or a rounded value.
   numbers are rounded - 0.00"
  []
  (rand-nth (list 'x (read-string (format "%.2f"(- (rand 10) 5))))))

(defn random-code
  "Returns a random expression of lisp style code with at most `depth` number of nested subexpressions."
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))
;; @@

;; @@
(defn codesize [c]
  "Given a code expression, return its size"
  (if (seq? c)
    (count (flatten c))
    1))

(defn random-subtree 
  "Returns a random subtree of code expression."
  [i]
  (if (zero? (rand-int (codesize i)))
    i
    (random-subtree 
      (rand-nth (rest i)))))

(defn replace-random-subtree
  "replace random subtree with given replacement"
  [i replacement]
  (if (= 1 (codesize i));got rid of (rand-int (codesize i))
    replacement
    (let [position-to-change 
          (rand-nth 
            (apply concat
                   (map #(repeat (codesize %1) %2)
                        (rest i)
                        (iterate inc 1))))]
          (map #(if %1 (replace-random-subtree %2 replacement) %2)
               (for [n (iterate inc 0)] (= n position-to-change))
               i))))
;; @@

;; @@
(defn select
  "Uses tournament selection to pick a single parent."
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

(defn mutate
  [i]
  (replace-random-subtree i (random-code 2)))

(defn crossover
  [i j]
  (replace-random-subtree i (random-subtree j)))

(defn mutate2
  "mutates i for given amount of times
   - chance of removing code
   - chance of adding new code"
  [i times]
  (if (zero? times)
    i
    (if (zero? (rand-int 2))
      (recur (random-subtree i)
             (dec times))
      (recur (replace-random-subtree i (random-code 2))
             (dec times)))))

(defn get-parents
  "returns two different parents"
  [population]
  (let [p1 (select population 7)
        p2 (select population 7)]
    (if (not= p1 p2)
      [p1 p2]
      (recur population))))

(defn crossover2
  "takes two parents and inserts a part of one into the other"
  [parentss]
  (replace-random-subtree (nth parentss 0) (random-subtree (nth parentss 1))))
;; @@

;; @@
(defn round2
  "Round a double to the given precision (number of significant digits)
   do this to maintain similarity with other functions
   http://stackoverflow.com/a/25098576"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn error2
  "checks if function is static and returns end value if so
   amount of input values can be reduced to 2"
  [tree]
  (let [value-function (eval (list 'fn '[x] tree))
        output (map (fn [x] (value-function x))
                    [-3 0 4])]
    (if (apply = output)
      (round2 2 (first output))
      tree)))

(defn simplify
  "longer but faster? & less repetitive version
  
   takes a tree and returns a simplified version if possible
   1: sees if tree is an endpoint
   2: sees if tree can be simplified
   3: sees if the size of tree is equal to the number of parameters
   4: breaks up tree and recurs"
  [tree]
  (let [size (codesize tree)]
    (if (= 1 size)
      tree
      (let [simtree (error2 tree)]
        (if (= 1 (codesize simtree))
          simtree
          (let [num-of-params (get function-table (nth tree 0))]
            (if (= size num-of-params)
              tree
              (cons (nth tree 0) (for [x (range 1 (+ 1 num-of-params))]
                      (simplify (nth tree x)))))))))))

(defn simplify2
  "same output as above but shorter, slower? & more unnecessary calls
   error2 called even if codesize = 1
   recurs even if tree cannot be simplified"
  [i]
  (let [tree (error2 i)]
    (if (= 1 (codesize tree))
      tree
      (cons (nth i 0) (for [x (range 1 (+ 1 (get function-table (nth i 0))))]
              (simplify2 (nth i x)))))))
;; @@

;; @@
(defn error 
  "Returns the error of the program by doing the following:
  	1) Creates clojure function out of `individual`.
  	2) Run the program on each test-case, and record outputs.
  	3) Compare outputs to target outputs and compute error."
  [individual]
  (let [value-function (eval (list 'fn '[x] individual))]
    (reduce + (map (fn [[x y]] 
                     (Math/abs 
                       (float (- (value-function x) y))))
                   target-data))))

(defn sort-by-error
  "Sorts a population of individuals (programs) by their error. Lowest errors first."
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

(defn tree-compare
  "compares two trees by going down the subtrees and comparing them
   the higher the returned value the larger the difference
   each different endpoint and function is counted as one
   1: sees if trees are the same
   2: sees if trees are endpoints and if functions are the same
      - if not then return the size of the greatest tree
   3: pairs together subtrees that have the most in common
   *** the pairing is only set up for functions with 2 parameters ***
          there must be a method that works for all functions"
  [tree1 tree2]
  (if (= tree1 tree2)
    0
    (if (or (= 1 (codesize tree1))
            (= 1 (codesize tree2))
            (not= (nth tree1 0) (nth tree2 0)))
      (max (codesize tree1) (codesize tree2))
      (let [num-of-params (get function-table (nth tree1 0))]
        (if (= 1 num-of-params)
          (recur (nth tree1 1) (nth tree2 1))
          (min (+ (tree-compare (nth tree1 1) (nth tree2 1))
                  (tree-compare (nth tree1 2) (nth tree2 2)))
                 (+ (tree-compare (nth tree1 1) (nth tree2 2))
                  (tree-compare (nth tree1 2) (nth tree2 1)))))))))

(defn sort-by-novelty
  "Sorts a population of individuals (programs) by their novelty. Highest novelty second after current best.
   tree-compare returns how similar a tree is to the current best - smaller more similar"
  [best population]
  (println "***Rewarding Novelty***")
  (vec (conj (map second
                  (sort (fn [[nov1 ind1] [nov2 ind2]] (> nov1 nov2))
                        (map #(vector (tree-compare % best) %) (drop 1 population))))
             best)))
;; @@

;; @@
;(def pops (repeatedly 10 #(random-code 4)))
;(println pops)
;(println (map #(simplify %) pops))
;(println (sort-by-novelty (first pops) pops))
;; @@

;; @@
;(def bests (atom []))

(defn evolve
  "original with minor additions"
  [max-generations]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly (:population-size hyperparameters)
                                               #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ (:population-size hyperparameters)
                                                        2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      ;(swap! bests conj best)
      (if (or (< best-error 
                 (:error-threshold hyperparameters))
              (= generation max-generations))
        (do (println "Success:" best)
            (println "Simple :" (simplify best))
          (plot/compose
            (plot/list-plot target-data)
            (plot/plot (eval (list 'fn '[x] best)) [-50 50])))
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 (:population-size hyperparameters))
                          #(mutate (select population 7)))
              (repeatedly (* 1/4 (:population-size hyperparameters))
                          #(crossover (select population 7)
                                      (select population 7)))
              (repeatedly (* 1/4 (:population-size hyperparameters)) 
                          #(select population 7)))))))))
;; @@

;; @@
(defn evolve2
  "original with novelty"
  [max-generations]
  (println "Starting evolution...")
  (loop [generation 0
         errors '(nil nil)
         population (sort-by-error (repeatedly (:population-size hyperparameters)
                                               #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ (:population-size hyperparameters)
                                                        2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      ;(swap! bests conj best)
      (if (or (< best-error 
                 (:error-threshold hyperparameters))
              (= generation max-generations))
        (do (println "Success:" best)
            (println "Simple :" (simplify best))
          (plot/compose
            (plot/list-plot target-data)
            (plot/plot (eval (list 'fn '[x] best)) [-50 50])))
        (if (and (< 4 generation);(<= (count errors) generations)
                 (apply = (conj errors best-error)))
          (recur
            (inc generation)
            (conj (drop-last errors) 0)
            (sort-by-novelty (simplify best)
              (concat
                (repeatedly (* 1/2 (:population-size hyperparameters))
                            #(simplify (mutate (select population 7))))
                (repeatedly (* 1/4 (:population-size hyperparameters))
                            #(simplify (crossover (select population 7)
                                                  (select population 7))))
                (repeatedly (* 1/4 (:population-size hyperparameters))
                            #(simplify (select population 7))))))
          (recur
            (inc generation)
            (conj (drop-last errors) best-error)
            (sort-by-error
              (concat
                (repeatedly (* 1/2 (:population-size hyperparameters))
                            #(mutate (select population 7)))
                (repeatedly (* 1/4 (:population-size hyperparameters))
                            #(crossover (select population 7)
                                        (select population 7)))
                (repeatedly (* 1/4 (:population-size hyperparameters))
                            #(select population 7))))))))))
;; @@

;; @@
(defn evolve3
  "midterm version"
  [max-generations]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly (:population-size hyperparameters)
                                               #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ (:population-size hyperparameters)
                                                        2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      ;(swap! bests conj best)
      (if (or (< best-error 
                 (:error-threshold hyperparameters))
              (= generation max-generations))
        (do (println "Success:" best)
            (println "Simple :" (simplify best))
          (plot/compose
            (plot/list-plot target-data)
            (plot/plot (eval (list 'fn '[x] best)) [-50 50])))
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* (:percent-cloned hyperparameters)
                             (:population-size hyperparameters)) 
                          #(select population 7))
              (repeatedly (* (:percent-crossover hyperparameters)
                             (:population-size hyperparameters))
                          #(mutate2 (crossover2 (get-parents population))
                                   (rand-int 3))))))))))
;; @@

;; @@
(defn evolve4
  "midterm with novelty"
  [max-generations]
  (println "Starting evolution...")
  (loop [generation 0
         errors '(nil nil)
         population (sort-by-error (repeatedly (:population-size hyperparameters)
                                               #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ (:population-size hyperparameters)
                                                        2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      ;(swap! bests conj best)
      (if (or (< best-error 
                 (:error-threshold hyperparameters))
              (= generation max-generations))
        (do (println "Success:" best)
            (println "Simple :" (simplify best))
          (plot/compose
            (plot/list-plot target-data)
            (plot/plot (eval (list 'fn '[x] best)) [-50 50])))
        (if (and (< 4 generation);(< (count errors) generations)
                 (apply = (conj errors best-error)))
          (recur
            (inc generation)
            (conj (drop-last errors) 0)
            (sort-by-novelty (simplify best)
              (concat
                (repeatedly (* (:percent-cloned hyperparameters)
                               (:population-size hyperparameters))
                            #(simplify (select population 7)))
                (repeatedly (* (:percent-crossover hyperparameters)
                               (:population-size hyperparameters))
                            #(simplify (mutate2 (crossover2 (get-parents population))
                                                (rand-int 2)))))))
          (recur
            (inc generation)
            (conj (drop-last errors) best-error)
            (sort-by-error
              (concat
                (repeatedly (* (:percent-cloned hyperparameters)
                               (:population-size hyperparameters))
                            #(select population 7))
                (repeatedly (* (:percent-crossover hyperparameters)
                               (:population-size hyperparameters))
                            #(mutate2 (crossover2 (get-parents population))
                                      (rand-int 3)))))))))))
;; @@

