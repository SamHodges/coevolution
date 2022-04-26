(ns propeller.main-coevolution
  (:require [propeller.genome :as genome]
            [propeller.gp :as gp]
            [propeller.selection :as selection]
            [propeller.variation :as variation]
            [propeller.push.instructions :as instructions]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]))


; TEACHER
; ingredients- total test set, eg regression input/outputs

; mutation function- change which test sets are being chosen

; error function- look at how much students improved

; main loop for evolving- run every x loops of student evolving



; STUDENT
; ingredients- potentially ingredients to find linear regression?

; mutation function- change equation

; error function- how far off given teacher input/outputs answer key

; main loop for evolving:
; - different students given different test cases
; - every x loops call teacher so it can evolve


;BELOW IS NEW AS OF 4/17 REFER TO THAT


;method 1: weights--(20%, 30%, 5% ...)
;to evolve method 1: tick up or down percentages by some small amt when mutating
;method 2: list--(random, variant, hardest, ...)
;to evolve method 2: chance of swapping order (like tsp)

;syllabus:
;step 1:
;-test all students on all test cases (collecting data for test cases)

;step 2:
;-evaluate teachers from student performance (compare last semester to the test that was just run)

;step 3:
;from those evaluations, mutate + evolve to create new teachers

;step 4:
;randomly assign students to teachers

;step 5:
;run the semester



;functions we'll need:
; (defn take-n-random [all-test-cases, test-case-performance, n]) -> n test cases

; (defn take-n-hardest [all-test-cases, test-case-performance, n]) -> n test cases

; (def features {:random take-n-random
;                 :hardest take-n-hardest
;                  ...}


; (defn testing-subset-from-teacher [teacher-genome, test-cases]) -> test case subset

; (defn mutate-teacher [teacher]) -> teacher
; list: takes a teacher and (possibly) swaps some order around, look at TSP mutate function
; weights: takes a teacher, (possibly) adds/subtracts from each weight, then normalizes at end

; (defn create-teacher) -> teacher
; list: randomly makes order of list (random order of keys from the map "features")
; weights: randomly chooses weights (possible issue: implies that each feature is equally
; important. Maybe stack the distribution? caviar issue)

;IMPORTANT: test-case-performance WILL LOOK LIKE THIS:
; vector of tests, where each vector
;[
; this corresponds to a single test: [1 5 20 4 2 1 ...]
; this corresponds to another test: [5 214 3 12 42 5 3 ...]
;]


; make initial test cases
; change amount of cases by going to function in simple regression
(require '[propeller.problems.simple-regression :as regression])
(def all-train-cases (:train regression/train-and-test-data))


; Random test subset
(defn take-n-random [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (shuffle all-test-cases)))
  )

; Helper functions to order questions by difficulty
; add total error to each test case
(defn pair-total-error-and-test-case [test-case-performance, all-test-cases]
  (map #(concat [%1] [%2])
       (map #(reduce + %) test-case-performance) all-test-cases))

; separate total error from test case
(defn unpair-total-error-and-test-case [paired-performance-test-cases]
  (map #(second %) paired-performance-test-cases))

; Hardest
(defn take-n-hardest [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (reverse (unpair-total-error-and-test-case (sort-by #(first %)
                                                                (pair-total-error-and-test-case test-case-performance all-test-cases)))))
    ))

; Easiest
(defn take-n-easiest [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (unpair-total-error-and-test-case (sort-by #(first %)
                                                       (pair-total-error-and-test-case test-case-performance all-test-cases))))
    ))


; Helper functions to sort by variance
(defn pair-variance-and-test-case [test-case-performance, all-test-cases]
  (map #(concat [%1] [%2])
       (find-variance test-case-performance)
       all-test-cases))

(defn unpair-variance-and-test-case [paired-performance-test-cases]
  (map #(second %) paired-performance-test-cases))

(defn find-variance [test-case-performance]
  (map - (map find-largest test-case-performance) (map find-smallest test-case-performance)))

(defn find-smallest [single-test-case-performance]
  (reduce #(if (< %1 %2) %1 %2) single-test-case-performance))

(defn find-largest [single-test-case-performance]
  (reduce #(if (> %1 %2) %1 %2) single-test-case-performance))


; Most Variant
(defn take-n-most-variant [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (reverse (unpair-variance-and-test-case (sort-by #(first %)
                                                             (pair-variance-and-test-case test-case-performance all-test-cases)))))
    ))

;Least Variant
(defn take-n-least-variant [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (unpair-variance-and-test-case (sort-by #(first %)
                                                    (pair-variance-and-test-case test-case-performance all-test-cases))))
    ))





(defn main [student-population-size generations train-cases semester-length]
  (loop [student-population (repeatedly student-population-size
                                        #(new-student-individual))
         teacher-population (repeatedly teacher-population-size
                                        #(new-teacher-individual))
         generation 0]
    ; report here potentially?
    (if (> generation generations)
      ; Yes:
      ; evolve students

      ; run eval on all students
      (run-gp-loop all-train-cases all-test-cases student-population)
      ; evolve teachers

      ;No:
      ; return everything

      )))



[[2 5 6 7 ]
 [0 0 1 3]
 [1 2 3 4]]

[(testcase1) testcase2 testcase3]





(defn run-gp-loop [all-train-cases all-test-cases students] (gp/gp {:instructions            regression/instructions
          :error-function          regression/error-function
          :training-data           all-train-cases
          :testing-data            (:test regression/train-and-test-data)
          :max-generations         500
          :population-size         500
          :max-initial-plushy-size 100
          :step-limit              200
          :parent-selection        :tournament
          :tournament-size         5
          :umad-rate               0.01
          :variation               {:umad      1.0
                                    :crossover 0.0}
          :elitism                 false})
  )







