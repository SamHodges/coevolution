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

; evolve students
(defn evolve-students [teacher-population student-population]
  (map #(run-gp-loop %2 %2 %1) (partition (count teacher-population) (shuffle student-population)) teacher-population)
  )

; evaluate students
(defn evaluate-students [all-train-cases all-test-cases student-population]
  (run-gp-loop all-train-cases all-test-cases student-population))

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
        (evaluate-students all-train-cases all-test-cases (evolve-students teacher-population student-population))
        (evolve-teacher )
        (inc generation))
        (evaluate-students all-train-cases all-test-cases student-population)
      )))

; normalizing function
(defn normalize [v]
  ;normalizes vector so it sums to 1
  (let [total (reduce + v)]
    (map #(/ % total) v)))


(defn count-cases-from-weights [weights, n]
  ;splits n into integers among weights such that the total sum is
  ;n again, used to avoid issues where a weighting like [0.33, 0.33, 0.33]
  ;with n of 10 would return [3, 3, 3], which does not sum to 10.
  ;This function would return [4, 3, 3] instead
  (let [inital-guess]))


(def base-teacher-vector
  ;shuffled to make random teachers
  ;if adding another feature, put it in this vector
  ;feature functions are of the form:
  ;(defn take-n-whatever [all-test-cases, test-case-performance, n]
  ;                      return n test cases chosen in some specific way)
  (vec [take-n-easiest, take-n-hardest, take-n-most-variant, take-n-least-variant, take-n-random]))

(def vector-of-weights
  ;weights, per index, for our teachers
  ;i.e. when we want to get n test cases from a given teacher,
  ;we choose 40% of those cases from the first function in the list,
  ;30% from the second, etc.
  (vec [50.0 50.0 0.0 0.0 0.0]))

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
  (shuffle base-teacher-vector))

(defn teacher-to-cases [teacher-genome, all-test-cases, test-case-performance, num-test-cases]
  ;returns list of n test cases, chosen according to the teacher's genome
  (loop [remaining-test-cases all-test-cases
         remaining-test-case-performance test-case-performance
         num-left-to-choose num-test-cases
         final-set-of-cases (vector)]
    (let [chosen-feature-function (teacher-genome (random-from-probabilities vector-of-weights))
          chosen-test-case (first (chosen-feature-function remaining-test-cases, remaining-test-case-performance, 1))
          chosen-case-index (.indexOf remaining-test-cases chosen-test-case)
          new-set-of-cases (conj final-set-of-cases chosen-test-case)]
      (do
        ;(println "current test case, index, case population, performance")
        ;(println chosen-test-case)
        ;(println chosen-case-index)
        ;(println remaining-test-cases)
        ;(println remaining-test-case-performance)
        ;(println "next case population, performance")
        ;(println (vec (remove #(= % chosen-test-case) remaining-test-cases)))
        ;(println (vec (concat (subvec remaining-test-case-performance 0 chosen-case-index)
        ;                      (subvec remaining-test-case-performance (+ chosen-case-index 1) (count remaining-test-case-performance)))))

        (if (= 1 num-left-to-choose)
          ;exit the loop
          new-set-of-cases
          ;recur
          (recur
            ;set of test cases with chosen one removed
            (vec (remove #(= % chosen-test-case) remaining-test-cases))

            (vec (concat (subvec remaining-test-case-performance 0 chosen-case-index)
                         (subvec remaining-test-case-performance (+ chosen-case-index 1) (count remaining-test-case-performance))))

            (- num-left-to-choose 1)
            new-set-of-cases))


        ;pick random index in teacher-genome from probabilities
        ;use the function at that index to choose a case from remaining-test-cases
        ;if remaining-test-cases = 1:
        ;   final-set-of-cases conjed with new test case
        ;else recur with
        ;   (remove the chosen case from remaining-test-cases, (- num-left-to-choose 1), final-set-of-cases conjed with new test case)
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
  [take-n-hardest, take-n-easiest, take-n-most-variant, take-n-least-variant, take-n-random])

 ;_(teacher-to-cases all-easiest-genome example-test-cases example-test-case-performance 2)


