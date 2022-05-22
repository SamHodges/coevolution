; importing libraries
(ns propeller.main-coevolution
  (:require [propeller.genome :as genome]
  [propeller.gp :as gp]
  [propeller.selection :as selection]
  [propeller.variation :as variation]
  [propeller.push.instructions :as instructions]
  [propeller.push.interpreter :as interpreter]
  [propeller.push.state :as state]
  [propeller.tools.math :as math]
  [propeller.utils :as utils]
  [propeller.problems.simple-classification-ryan :as classification]))

; TODO list:
; TODO clean up reporting mechanisms
; TODO make teacher genome length a passed in var instead of hardcoded!
; class clojure.lang.LazySeq cannot be cast to class java.lang.Number
; (clojure.lang.LazySeq is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')

; Notes:
; gp returns average error of best student?


;##############################################################################

(def all-train-cases (:train propeller.problems.simple-classification-ryan/train-and-test-data))

;##############################################################################
; Helper Functions for Take N Functions

; inputs:
; - all-test-cases: the entire subset, eg:
#_(def example-test-cases
    [{:input1 [4] :output1 [3]}
     {:input1 [2] :output1 [-3]}
     {:input1 [1] :output1 [1]}
     ])

; - test-case-performance: how well students did on tests, eg:
#_(def example-test-case-performance
    [[10 6 0 5 7]
     [14 2 4 9 10]
     [0 5 3 8 11]])

; - paired-performance-test-cases: list with paired errors and test cases, eg:
#_(def example-paired-performance-test-cases
    '((28 {:input1 [4], :output1 [3]})
      (39 {:input1 [2], :output1 [-3]})
      (27 {:input1 [1], :output1 [1]})))

; - single-test-case-performance: the test case performance for only 1 test, eg:
#_(def example-single-test-case-performance
    [10 6 0 5 7])


; Hardest/Easiest Helper Functions

; Pair Error and Test Cases
; make a list with the errors and test cases combined
; output:
; - combined list of error/test cases, eg ((28 {:input1 [4], :output1 [3]}) (39 {:input1 [2], :output1 [-3]}))
(defn pair-total-error-and-test-case [test-case-performance, all-test-cases]
  (map #(concat [%1] [%2])
       (map #(reduce + %) test-case-performance) all-test-cases))
#_(pair-total-error-and-test-case example-test-case-performance example-test-cases)


; Unpair Error and Test Cases
; separate total error from test case, and return the test cases
; output:
; - list of test cases, eg => ({:input1 [4], :output1 [3]} {:input1 [1], :output1 [1]})
(defn unpair-total-error-and-test-case [paired-performance-test-cases]
  (map #(second %) paired-performance-test-cases))
#_(unpair-total-error-and-test-case example-paired-performance-test-cases)



; Variance Helper Functions

; Find Smallest
; find the smallest student error
; output:
; - smallest error, eg 0
(defn find-smallest [single-test-case-performance]
  (reduce #(if (< %1 %2) %1 %2) single-test-case-performance))
#_(find-smallest example-single-test-case-performance)

; Find Largest
; find the largest student error
; output:
; - largest error, eg 10
(defn find-largest [single-test-case-performance]
  (reduce #(if (> %1 %2) %1 %2) single-test-case-performance))
#_(find-largest example-single-test-case-performance)

; Find Variance
; find the difference between the smallest and largest error for each test case
; output:
; - list of variances, eg (10 12 11)
(defn find-variance [test-case-performance]
  (map - (map find-largest test-case-performance) (map find-smallest test-case-performance)))
#_(find-variance example-test-case-performance)

; Pair Variance and Test Cases
; make a list with the variances and test cases combined
; output:
; - combined list of variance/test cases, eg ((28 {:input1 [4], :output1 [3]}) (39 {:input1 [2], :output1 [-3]}))
(defn pair-variance-and-test-case [test-case-performance, all-test-cases]
  (map #(concat [%1] [%2])
       (find-variance test-case-performance)
       all-test-cases))
#_(pair-variance-and-test-case example-test-case-performance example-test-cases)

; Unpair Variance and Test Cases
; separate variance from test case, and return the test cases
; output:
; - list of test cases, eg => ({:input1 [4], :output1 [3]} {:input1 [1], :output1 [1]})
(defn unpair-variance-and-test-case [paired-performance-test-cases]
  (map #(second %) paired-performance-test-cases))
#_(unpair-variance-and-test-case example-paired-performance-test-cases)



;##############################################################################
; Take N Functions

; inputs:
; - all-test-cases: the entire subset, eg:
#_(def example-test-cases
  [{:input1 [4] :output1 [3]}
   {:input1 [2] :output1 [-3]}
   {:input1 [1] :output1 [1]}
   ])

; - test-case-performance: how well students did on tests, eg:
#_(def example-test-case-performance
  [[10 6 0 5 7]
   [14 2 4 9 10]
   [0 5 3 8 11]])

; - n: how many cases you want to return, eg:
#_(def example-n 2)

; outputs:
; - a subset of test cases, eg: ({:input1 [2], :output1 [-3]} {:input1 [4], :output1 [3]})

; Take n Random
; take a random subset from the main set
(defn take-n-random [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (shuffle all-test-cases)))
  )
#_(take-n-random example-test-cases example-test-case-performance example-n)

; Take n Hardest
; take a subset of the hardest test cases
(defn take-n-hardest [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (reverse
              (unpair-total-error-and-test-case
                (sort-by #(first %)
                         (pair-total-error-and-test-case
                           test-case-performance all-test-cases)))))))
#_(take-n-hardest example-test-cases example-test-case-performance example-n)

; Take n Easiest
; take a subset of the easiest cases
(defn take-n-easiest [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (unpair-total-error-and-test-case
              (sort-by #(first %)
                       (pair-total-error-and-test-case
                         test-case-performance all-test-cases))))))
#_(take-n-easiest example-test-cases example-test-case-performance example-n)

; Take n Most Variant
(defn take-n-most-variant [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (reverse
              (unpair-variance-and-test-case
                (sort-by #(first %)
                         (pair-variance-and-test-case
                           test-case-performance all-test-cases)))))))
#_(take-n-most-variant example-test-cases example-test-case-performance example-n)

; Take n Least Variant
(defn take-n-least-variant [all-test-cases, test-case-performance, n]
  (if (> n (count all-test-cases))
    all-test-cases
    (take n (unpair-variance-and-test-case
              (sort-by #(first %)
                       (pair-variance-and-test-case
                         test-case-performance all-test-cases))))))
#_(take-n-least-variant example-test-cases example-test-case-performance example-n)



;##############################################################################

; normalizing function
(defn normalize [v]
  ;normalizes vector so it sums to 1
  (let [total (reduce + v)]
    (map #(/ % total) v)))

(def list-base-teacher-vector
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

(defn list-create-random-teacher-genome []
  ;returns a randomly created teacher genome
  (shuffle list-base-teacher-vector))

;inputs:
;teacher-genome: the genome for a single teacher
;all-test-cases: all the test cases (list of maps)
;test-case-performance: performance on those test cases for some set of students (list of lists, each one corresponding to a specific test case)
;num-test-cases: number of test cases to return. IMPORTANT: DO NOT MAKE THIS NUMBER BIGGER THAN SIZE OF ALL TEST CASES

;outputs:
;a list, num-test-cases long, of test cases.

;example usage: (list-teacher-to-cases list-all-easiest-genome example-test-cases example-test-case-performance 2)
;"give me 2 test cases, from the teacher genome 'list-all-easiest-genome', from the test case set 'example-test-cases' which
;has performance detailed in 'example-test-case-performance' ."
(defn list-teacher-to-cases [teacher-genome, all-test-cases, test-case-performance, num-test-cases]
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

(def list-all-easiest-genome
  [take-n-easiest, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-random])

(def list-all-random-genome
  [take-n-random, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-easiest])

;_(list-teacher-to-cases list-all-easiest-genome example-test-cases example-test-case-performance 2)

;##############################################################################
;New teachers! Teachers are now a list of 5 weights that correspond to the 5 feature functions.
;mutation is done by shifting a percentage up or down (but not past 0) and then normalizing

(def feature-functions
  [take-n-easiest, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-random])

(def base-teacher-vector
  ;not really relevant the way it is in the list version of a teacher
  ;I can use this to talk about teacher genomes though
  ;the weights in each index correspond to:
  ;easiest, hardest, most variant, least variant, random
  (vec [0.2, 0.2, 0.2, 0.2, 0.2]))

(defn create-random-teacher-genome []
  (vec (normalize (take 5 (repeatedly #(rand 1.0))))))

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
    (let [chosen-feature-function (nth feature-functions (random-from-probabilities teacher-genome)) ;choose a feature function to use for this index
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

(def all-easiest-genome
  [1.0 0.0 0.0 0.0 0.0])

(def all-random-genome
  [0.0 0.0 0.0 0.0 1.0])

(def balanced-genome
  [0.2 0.2 0.2 0.2 0.2])

;_(teacher-to-cases all-easiest-genome example-test-cases example-test-case-performance 2)



;##############################################################################
; Evolving Students

; inputs:
; - students: a list of students for a single teacher, eg:
#_(def example-students
    '({:plushy (:exec_dup :integer_subtract 0 :integer_subtract)}
      {:plushy (:exec_if :exec_if)} {:plushy (:integer_add :exec_dup :in1 0 :exec_if)}
      {:plushy (:in1 1 :integer_eq 0 :integer_subtract)}
      {:plushy (:integer_subtract :integer_quot)}))

; - student-population: a list of students for all of the teachers, eg:
#_(def example-student-population
    '(({:plushy (false :boolean_from_integer true)}
       {:plushy (1 false :boolean_from_integer :integer_mod true)}
       {:plushy (false :boolean_from_integer :integer_mod true)}
       {:plushy (:boolean_invert_first_then_and true :integer_mod 3)}
       {:plushy (true true :integer_mod 3)})
      ({:plushy (true true :integer_mod 3)}
       {:plushy (true true :integer_mod 3)}
       {:plushy (false :boolean_from_integer :integer_mod true :boolean_not)}
       {:plushy (true true :integer_mod 3)}
       {:plushy (false :boolean_from_integer :integer_mod true)})))

; - teacher-cases, teacher-population: a list of teachers, eg:
#_(def example-teacher-cases
    '([0.005542920374019209 0.3299393224209624 0.2873773482352696 0.13283622678584261 0.24430418218390626]
      [0.21733978683274813 0.08560319324948945 0.3089665255126939 0.008750299970001355 0.3793401944350672]))

; - all-test-cases: a list of actual test cases, eg,
#_(def example-all-test-cases
    '[{:input1 [4], :output1 [3]} {:input1 [2], :output1 [-3]}
      {:input1 [1], :output1 [1]} {:input1 [20], :output1 [12]}
      {:input1 [16], :output1 [-6]} {:input1 [7], :output1 [16]}
      {:input1 [-5], :output1 [21]}])

; - student: a single student plushy, eg:
#_(def example-student '({:plushy (0)}))

; - test-case-performance: a list of how well students do on different tests, eg
#_(def example-test-case-performance
    '([[1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]
       [1000000 1000000 1000000 1000000 1000000]]
      [[0 0 0 0 0] [6 6 6 6 6]
       [2 2 2 2 2] [9 9 9 9 9]
       [9 9 9 9 9] [13 13 13 13 13] [18 18 18 18 18]])

    )

; student-subgroup

; teacher

; Run GP Loop
; Call GP to run a loop of student evolution
; output:
; - a list of evolved students, eg '({:plushy (:in1 1 :integer_eq 0 :integer_subtract)} {:plushy (:integer_subtract :integer_quot)})
(defn run-gp-loop [students teacher-cases]
  (do (print "running main gp loop... \n")
      (print "my students: " (count students) students "\n")
      (print "my teacher: " (count teacher-cases) teacher-cases "\n")

      ;(Thread/sleep 5000)
      (gp/gp {:instructions            propeller.problems.simple-classification-ryan/instructions
              :error-function          propeller.problems.simple-classification-ryan/error-function
              :training-data           (apply list teacher-cases)
              :testing-data            (:test propeller.problems.simple-classification-ryan/train-and-test-data)
              :max-generations         2
              :population-size         (count students)
              :population              students ;test-student
              :step-limit              200
              :parent-selection        :lexicase
              :tournament-size         5
              :umad-rate               0.01
              :variation               {:umad      1.0
                                        :crossover 0.0}
              :elitism                 false})))
#_(run-gp-loop example-students example-teacher-cases)

; Error Function
; error function from regression backend code.
; output:
; - errors for each student, eg
(defn error-function [all-test-cases student]
  (let [program (genome/plushy->push (:plushy student))
        inputs (map (fn [x] (first (:input1 x))) all-test-cases)
        correct-outputs (map (fn [x] (first (:output1 x))) all-test-cases)
        outputs (map (fn [input]
                       (state/peek-stack
                         (interpreter/interpret-program
                           program
                           (assoc state/empty-state :input {:in1 input})
                           200)
                         :integer))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (math/abs (- correct-output output))))
                    correct-outputs
                    outputs)]
    errors))
#_(error-function example-all-test-cases example-student)

; Evolve Students
; shuffle student splits and send them to evolve
; output:
; - evolved students, eg '({:plushy (:in1 1 :integer_eq 0 :integer_subtract)} {:plushy (:integer_subtract :integer_quot)})
(defn evolve-students [teacher-population student-population all-test-cases test-case-performance]
  (do
    (print "evolving students..." "\n")
    (let [combined-students (reduce concat student-population)
          split-students
          (partition (/ (count combined-students) (count teacher-population))
                     ;shuffle students so teachers get different ones
                     (shuffle combined-students))
          teacher-cases
          (map #(teacher-to-cases %1 all-test-cases
                                  (vec (map vec %2))
                                  (/ (count combined-students) 2))
               teacher-population test-case-performance)
          evolved-students (map run-gp-loop split-students teacher-cases)]
      (print "combined students: " (count combined-students) combined-students "\n")
      (print "split students: " (count split-students) split-students "\n")
      (print "teacher-cases: " (count teacher-cases) teacher-cases "\n")
      (print "evolved students: " (count evolved-students) evolved-students "\n")
      evolved-students)))
#_(evolve-students example-teacher-cases example-students example-all-test-cases example-test-case-performance)

(defn subgroup-error [all-test-cases student-subgroup]
  (do
    (apply (partial mapv vector) (vec (map #(vec (error-function all-test-cases %1)) student-subgroup)))
    ))

; evaluate students
(defn evaluate-students [all-test-cases student-population]
  (do (print "evaluating... \n")
      (print "current student pop:" student-population "\n")
      ;(print error-function all-test-cases (first student-population))
      (map #(subgroup-error all-test-cases %1)
           student-population)))

(defn split-students [combined-students teacher-population]
  (partition (/ (count combined-students) (count teacher-population))
             ; shuffle students so teachers get different ones
             (shuffle combined-students)))

; main loop
(defn main [teacher-population-size student-population-size student-size generations all-test-cases]
  ; loop until you hit generation limit
  (loop
    ; student pop made using gp code
    [; create as many teachers as needed
     teacher-population (repeatedly teacher-population-size
                                    #(create-random-teacher-genome))
     student-population
     (split-students (#?(:clj pmap :cljs map)
                       (fn [_] {:plushy
                                (genome/make-random-plushy propeller.problems.simple-classification-ryan/instructions student-size)})
                       (range student-population-size)) teacher-population)
     ; keep track of scores
     student-scores (evaluate-students all-test-cases student-population)
     ; start at gen 0
     generation 0]
    ; TODO: report here potentially?
    ; only continue if below gen count
    (if (< generation generations)
      (do
        (print (str "on gen: " generation "\n"))
        (print "initial students: " (count student-population) student-population "\n")
        ; evolve students and pass on to relevant places
        (let [
              new-student-population
              (evolve-students teacher-population student-population all-test-cases
                               student-scores)
              new-student-scores (evaluate-students all-test-cases student-population)]


          (recur
            ; evolved student population
            ; evolved teacher population
            teacher-population; (map #(evolve-teacher) teacher-population (partition (count teacher-population) new-student-population)))
            new-student-population
            ; re-calculate student scores
            (evaluate-students all-test-cases new-student-population)
            ; increase gen
            (inc generation))))
      ; loop is over, send back full eval
      student-population
      )))

(main 2 10 15 2 example-test-cases)