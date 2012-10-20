(ns four-cljr.easy-test
  (:use clojure.test)
  (:require clojure.set))

;; 19
;; Easy
;; seqs core-functions
;; Write a function which returns the last element in a sequence.
;; Special Restrictions - last
(deftest test-19
  (testing "Last Element"
    (is (= ((fn get-last [sq] (first (reverse sq))) [1 2 3 4 5]) 5))
    (is (= ((fn get-last [sq] (first (reverse sq))) '(5 4 3)) 3))
    (is (= ((fn get-last [sq] (first (reverse sq))) ["b" "c" "d"]) "d"))))

;; 20
;; Easy
;; seqs
;; Write a function which returns the second to last element from a sequence.
(deftest test-20
  (testing "Penultimate Element"
    (is (= ((fn get-second-last [sq] (second (reverse sq)))
            (list 1 2 3 4 5)) 4))
    (is (= ((fn get-second-last [sq] (second (reverse sq)))
            ["a" "b" "c"]) "b"))
    (is (= ((fn get-second-last [sq] (second (reverse sq)))
            [[1 2] [3 4]]) [1 2]))))

;; 21
;; Easy
;; seqs core-functions
;; Write a function which returns the Nth element from a sequence.
;; Special Restrictions nth
(deftest test-21
  (testing "Nth Element"
    (is (= ((fn get-nth
              [coll index]
              (first (drop index coll))) '(4 5 6 7) 2) 6))
    (is (= ((fn get-nth
              [coll index]
              (first (drop index coll))) [:a :b :c] 0) :a))	
    (is (= ((fn get-nth
              [coll index]
              (first (drop index coll))) [1 2 3 4] 1) 2))
    (is (= ((fn get-nth
              [coll index]
              (first (drop index coll))) '([1 2] [3 4] [5 6]) 2) [5 6]))))

;; 22
;; Easy
;; seqs core-functions
;; Write a function which returns the total number of elements in a sequence.
(deftest test-22
  (testing "Count a Sequence"
    (is (= ((fn count-me [x] (alength (into-array x))) '(1 2 3 3 1)) 5))
    (is (= ((fn count-me [x] (alength (into-array x))) "Hello World") 11))
    (is (= ((fn count-me [x] (alength (into-array x))) [[1 2] [3 4] [5 6]]) 3))
    (is (= ((fn count-me [x] (alength (into-array x))) '(13)) 1))
    (is (= ((fn count-me [x] (alength (into-array x))) '(:a :b :c)) 3))))







;; 173
;; Easy
;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.)
;; (let [bindings* ] exprs*)
;; Complete the bindings so all let-parts evaluate to 3.
(deftest test-173
  (testing "Intro to Destructuring 2"
    (is (= 3
           (let [[f a] [+ (range 3)]] (apply f a))
           (let [[[f a] b] [[+ 1] 2]] (f a b))
           (let [[f a] [inc 2]] (f a)))))) 

