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
