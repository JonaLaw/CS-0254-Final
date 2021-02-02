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
