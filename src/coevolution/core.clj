(ns coevolution.core)

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







