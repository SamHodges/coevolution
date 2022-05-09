(ns propeller.main-coevolution
  ;libraries
  (:require [propeller.genome :as genome]
            [propeller.gp :as gp]
            [propeller.selection :as selection]
            [propeller.variation :as variation]
            [propeller.push.instructions :as instructions]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.tools.math :as math]
            [propeller.utils :as utils]))

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
; calls gp loop when we want to evolve the students
(defn run-gp-loop [students teacher-cases]
  (do (print "running main gp loop... \n")
      (print "my students: " students "\n")
      (print "my teacher: " teacher-cases "\n")
      (gp/gp {:instructions            regression/instructions
          :error-function          regression/error-function
          :training-data           (apply list teacher-cases)
          :testing-data            (:test regression/train-and-test-data)
          :max-generations         1
          :population-size         (count students)
          :population              students
          :step-limit              200
          :parent-selection        :tournament
          :tournament-size         5
          :umad-rate               0.01
          :variation               {:umad      1.0
                                    :crossover 0.0}
          :elitism                 false}))
  )


; error function
(defn error-function [input-output-pairs student]
  (let [program (genome/plushy->push (:plushy student))
        inputs (map (fn [x] (first (:input1 x))) input-output-pairs)
        correct-outputs (map (fn [x] (first (:output1 x))) input-output-pairs)
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

; evolve subgroups of students
(defn evolve-subgroups [student-subgroup teacher]
  (let [new-students (run-gp-loop student-subgroup teacher)]
    (do
      (print "evolving subgroups method... \n")
      (print new-students "\n")
        new-students)))

; split students and send them to evolve
(defn evolve-students [teacher-population student-population all-test-cases test-case-performance]
  ; run gp on each student-teacher group
  (do
    (print "evolving students..." "\n")
    (let [split-students
          (partition (/ (count student-population) (count teacher-population))
                     ; shuffle students so teachers get different ones
                     (shuffle student-population))
          teacher-cases
          (map #(teacher-to-cases %1 all-test-cases
                                  (vec (map vec test-case-performance))
                                (/ (count test-case-performance) 2))
                             teacher-population)
          evolved-students (map evolve-subgroups split-students teacher-cases)]
      (print "split students: " split-students "\n")
      (print "teacher-cases: " teacher-cases "\n")
      (print "evolved students: " evolved-students "\n")
    evolved-students))
  )


(def teacher-population '([0.467830484743246 0.05742994503583208 0.24787083171760918 0.21481503394808674 0.012053704555225905] [0.20823573476733295 0.2087936143955382 0.28331310960040895 0.2554954728406314 0.04416206839608836]) )
(def all-test-cases
  '[{:input1 [4], :output1 [3]} {:input1 [2], :output1 [-3]}
    {:input1 [1], :output1 [1]} {:input1 [20], :output1 [12]}
    {:input1 [16], :output1 [-6]} {:input1 [7], :output1 [16]}
    {:input1 [-5], :output1 [21]}])
(def test-case-performance '((1 5 0 8 22 9 26) (1000000 1000000 1000000 1000000 1000000 1000000 1000000)
                             (1000000 1000000 1000000 1000000 1000000 1000000 1000000)
                             (1000000 1000000 1000000 1000000 1000000 1000000 1000000) (2 4 0 11 7 15 20)
                             (1000000 1000000 1000000 1000000 1000000 1000000 1000000) (3 3 1 12 6 16 21) (2 4 0 11 7 15 20)
                             (1000000 1000000 1000000 1000000 1000000 1000000 1000000) (2 4 0 11 7 15 20)) )

(vec (map vec test-case-performance))
(def teacher [0.467830484743246 0.05742994503583208 0.24787083171760918 0.21481503394808674 0.012053704555225905])
(map #(teacher-to-cases
        %1 all-test-cases (vec (map vec test-case-performance))
        (/ (count test-case-performance) 2)) teacher-population)
(def student-population (#?(:clj pmap :cljs map)
  (fn [_] {:plushy
           (genome/make-random-plushy regression/instructions 5)})
  (range 10)))

(def test-student '({:plushy (0)}
               {:plushy ()}
               {:plushy (close 1 :in1 :integer_subtract)}
               {:plushy (:integer_mult)}
               {:plushy (:integer_subtract close)}))

(def test-teacher [{:input1 [2], :output1 [-3]}
                   {:input1 [1], :output1 [1]}
                   {:input1 [4], :output1 [3]}
                   {:input1 [16], :output1 [-6]}
                   {:input1 [-5], :output1 [21]}])


; TODO: can't output all test cases for some reason? can only do 8/10 max?

; evaluate students
(defn evaluate-students [all-test-cases student-population]
  (do (print "evaluating... \n")
      (print "current student pop:" student-population "\n")
      ;(print error-function all-test-cases (first student-population))
    (map #(error-function all-test-cases %1) student-population)))


; main loop
(defn main [teacher-population-size student-population-size student-size generations all-test-cases]
  ; loop until you hit generation limit
  (loop
    ; student pop made using gp code
    [student-population
         (#?(:clj pmap :cljs map)
           (fn [_] {:plushy
                    (genome/make-random-plushy regression/instructions student-size)})
           (range student-population-size))
     ; create as many teachers as needed
     teacher-population (repeatedly teacher-population-size
                                    #(create-random-teacher-genome))
     ; keep track of scores
     student-scores (map #(error-function all-test-cases %1) student-population)
     ; start at gen 0
     generation 0]
    ; TODO: report here potentially?
    ; only continue if below gen count
    (if (< generation generations)
      (do
        (print (str "on gen: " generation "\n"))
      ; evolve students and pass on to relevant places
      (let [new-student-scores (evaluate-students all-test-cases student-population)
            new-student-population
            (evolve-students teacher-population student-population all-test-cases
                             new-student-scores)]
      (recur
        ; evolved student population
        new-student-population
        ; evolved teacher population
        teacher-population; (map #(evolve-teacher) teacher-population (partition (count teacher-population) new-student-population)))
        ; evaluate students compared to last performances
        (map - student-scores new-student-scores)
        ; increase gen
        (inc generation))))
      ; loop is over, send back full eval
      ; TODO: execution error when sending results - divide by 0 - gp/report needs substitution
      (evaluate-students all-test-cases student-population)
      )))

(main 2 10 10 5 example-test-cases)

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
