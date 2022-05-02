(ns propeller.main-coevolution
  ;libraries
  (:require [propeller.genome :as genome]
            [propeller.gp :as gp]
            [propeller.selection :as selection]
            [propeller.variation :as variation]
            [propeller.push.instructions :as instructions]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]))

; make initial test cases
; TODO: change amount of cases to whatever we end up waiting
; -> go to function in simple regression
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
(defn unpair-variance-and-test-case [paired-performance-test-cases]
  (map #(second %) paired-performance-test-cases))

(defn find-smallest [single-test-case-performance]
  (reduce #(if (< %1 %2) %1 %2) single-test-case-performance))

(defn find-largest [single-test-case-performance]
  (reduce #(if (> %1 %2) %1 %2) single-test-case-performance))

(defn find-variance [test-case-performance]
  (map - (map find-largest test-case-performance) (map find-smallest test-case-performance)))

(defn pair-variance-and-test-case [test-case-performance, all-test-cases]
  (map #(concat [%1] [%2])
       (find-variance test-case-performance)
       all-test-cases))


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



; gp loop- the evolving call
(defn run-gp-loop [all-train-cases all-test-cases students]
  (gp/gp {:instructions            regression/instructions
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

(defn run-gp-eval [all-train-cases all-test-cases students]
     (gp/gp {:instructions            regression/instructions
             :error-function          regression/error-function
             :training-data           all-train-cases
             :testing-data            (:test regression/train-and-test-data)
             :max-generations         1
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


; evolve students
(defn evolve-students [teacher-population student-population]
  (map #(run-gp-loop %2 %2 %1) (partition (count teacher-population) (shuffle student-population)) teacher-population)
  )

; evaluate students
(defn evaluate-students [all-test-cases student-population]
  (run-gp-eval all-test-cases all-test-cases student-population))

; main loop
; TODO: combine evolving stuff so that the output of 1 is passed to the next
(defn main [student-population-size generations train-cases semester-length]
  (loop [student-population (repeatedly student-population-size
                                        #(new-student-individual))
         teacher-population (repeatedly teacher-population-size
                                        #(new-teacher-individual))
         generation 0]
    ; report here potentially?
    (if (< generation generations)
      ; Yes
      (recur
        (let [new-student-population (evolve-students teacher-population student-population)]
        new-student-population
        (map #(evolve-teacher) teacher-population)
        (inc generation)))
      (evaluate-students all-test-cases  student-population)
      )))

; normalizing function
(defn normalize [v]
  ;normalizes vector so it sums to 1
  (let [total (reduce + v)]
    (map #(/ % total) v)))

(def base-teacher-vector
  ;shuffled to make random teachers
  ;IF ADDING ANOTHER FEATURE, PUT IT IN THIS VECTOR
  ;feature functions are of the form:
  ;(defn take-n-whatever [all-test-cases, test-case-performance, n]
  ;                      return n test cases chosen in some specific way)
  (vec [take-n-easiest, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-random]))

(def vector-of-weights
  ;weights, per index, for our teachers
  ;i.e. when we want to get n test cases from a given teacher,
  ;for each test case, we choose it with probability index1 % from
  ;the first feature in its genome, probability index2 % from the second
  ;feature in its genome, etc.
  (vec [100.0 0.0 0.0 0.0 0.0]))

(defn random-from-probabilities [prob-vector]
  ;prob: [0.42, 0.21, ...]
  ;https://stackoverflow.com/questions/14464011/idiomatic-clojure-for-picking-between-random-weighted-choices
  ;returns index randomly chosen by prob
  (let [total (reduce + prob-vector)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (prob-vector i) sum))
        i
        (recur (inc i) (+ (prob-vector i) sum))))))

(defn create-random-teacher-genome []
  ;returns a randomly created teacher genome
  (shuffle base-teacher-vector))

;inputs:
;teacher-genome: the genome for a single teacher
;all-test-cases: all the test cases (list of maps)
;test-case-performance: performance on those test cases for some set of students (list of lists, each one corresponding to a specific test case)
;num-test-cases: number of test cases to return. IMPORTANT: DO NOT MAKE THIS NUMBER BIGGER THAN SIZE OF ALL TEST CASES

;outputs:
;a list, num-test-cases long, of test cases.

;example usage: (teacher-to-cases all-easiest-genome example-test-cases example-test-case-performance 2)
;"give me 2 test cases, from the teacher genome 'all-easiest-genome', from the test case set 'example-test-cases' which
;has performance detailed in 'example-test-case-performance' ."
(defn teacher-to-cases [teacher-genome, all-test-cases, test-case-performance, num-test-cases]
  ;returns list of n test cases, chosen according to the teacher's genome
  (loop [remaining-test-cases all-test-cases ;test cases we have not selected already for returning
         remaining-test-case-performance test-case-performance ;test case performance of test cases we have not selected
         num-left-to-choose num-test-cases ;how many cases we still need to add to the return vector
         final-set-of-cases (vector) ;the return vector, ends up as a list of length num-test-cases of distinct test cases
         ]
    (let [chosen-feature-function (teacher-genome (random-from-probabilities vector-of-weights)) ;choose a feature function to use for this index
          chosen-test-case (first (chosen-feature-function remaining-test-cases, remaining-test-case-performance, 1)) ;use it to pick a test case
          chosen-case-index (.indexOf remaining-test-cases chosen-test-case) ;helper to remove test case from remaining
          new-set-of-cases (conj final-set-of-cases chosen-test-case) ;return vector with our new case added on
          ]
      (do
        (if (= 1 num-left-to-choose)
          ;if we're done, exit the loop and return our final vector
          new-set-of-cases
          ;otherwise, recur
          (recur
            ;set of test cases with newest chosen case removed becomes our new list of test cases we haven't selected already
            (vec (remove #(= % chosen-test-case) remaining-test-cases))
            ;set of test case performance with newest chosen case removed becomes our new list of test case performance for test cases we haven't selected already
            (vec (concat (subvec remaining-test-case-performance 0 chosen-case-index)
                         (subvec remaining-test-case-performance (+ chosen-case-index 1) (count remaining-test-case-performance))))
            (- num-left-to-choose 1)
            new-set-of-cases) ;return vector with our newest case included
          )
        )
      )
    )
  )

(def example-test-case-performance
  [
   [10 6 0 5 7] ;total error: 28
   [14 2 4 9 10]
   [0 5 3 8 11] ;total error: 27
   [8 9 14 20 17] ;total error: 68
   [4 10 19 5 3]
   [11 7 18 6 1]
   [15 16 2 13 12] ;total error: 58
   ])

(def example-test-cases
  [{:input1 [4] :output1 [3]}
   {:input1 [2] :output1 [-3]}
   {:input1 [1] :output1 [1]}
   {:input1 [20] :output1 [12]}
   {:input1 [16] :output1 [-6]}
   {:input1 [7] :output1 [16]}
   {:input1 [-5] :output1 [21]}
   ])

(def all-easiest-genome
  [take-n-easiest, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-random])

(def all-random-genome
  [take-n-random, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-easiest])

;_(teacher-to-cases all-easiest-genome example-test-cases example-test-case-performance 2)
