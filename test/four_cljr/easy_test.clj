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

;; 23
;; Easy
;; seqs core-functions
;; Write a function which reverses a sequence.
;; Special Restrictions: reverse rseq
(defn reverse-seq 
  [c1]
  (loop [lc1 c1 rc '()]
    (if (empty? lc1)
      rc
      (recur (rest lc1) (cons (first lc1) rc)))))

(deftest test-23
  (testing "Reverse a Sequence"
    (is (= (reverse-seq [1 2 3 4 5]) [5 4 3 2 1]))	
    (is (= (reverse-seq (sorted-set 5 7 2 7)) '(7 5 2)))
    (is (= (reverse-seq [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))))

;; 24
;; Easy
;; seqs
;; Write a function which returns the sum of a sequence of numbers.
(deftest test-24
  (testing "Sum It All Up"
    (is (= (#(reduce + %1) [1 2 3]) 6))
    (is (= (#(reduce + %1) (list 0 -2 5 5)) 8))
    (is (= (#(reduce + %1) #{4 2 1}) 7))
    (is (= (#(reduce + %1) '(0 0 -1)) -1))
    (is (= (#(reduce + %1) '(1 10 3)) 14))))

;; 25
;; Easy
;; seqs
;; Write a function which returns only the odd numbers from a sequence.
(deftest test-25
  (testing "Find the odd numbers"
    (is (= (#(filter odd? %) #{1 2 3 4 5}) '(1 3 5)))
    (is (= (#(filter odd? %) [4 2 1 6]) '(1)))
    (is (= (#(filter odd? %) [2 2 4 6]) '()))
    (is (= (#(filter odd? %) [1 1 1 3]) '(1 1 1 3)))))

;; 26
;; Easy
;; Fibonacci seqs
;; Write a function which returns the first x fibonacci numbers
(defn fib
  [a b]
  (lazy-seq (cons a (fib b (+ b a)))))

(deftest test-26
  (testing "Fibonacci Sequence"
    (is (= (#(take % (fib 1 1)) 3) '(1 1 2)))	
    (is (= (#(take % (fib 1 1)) 6) '(1 1 2 3 5 8)))	
    (is (= (#(take % (fib 1 1)) 8) '(1 1 2 3 5 8 13 21)))))

;; 27
;; Easy
;; seqs
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
(deftest test-27
  (testing "Palindome Detector"
    (is (false? (#(= (reverse %) (if (seq? %) % (seq %))) '(1 2 3 4 5))))
    (is (true? (#(= (reverse %) (if (seq? %) % (seq %))) "racecar")))
    (is (true? (#(= (reverse %) (if (seq? %) % (seq %))) [:foo :bar :foo])))
    (is (true? (#(= (reverse %) (if (seq? %) % (seq %))) '(1 1 3 3 1 1))))
    (is (false? (#(= (reverse %) (if (seq? %) % (seq %))) '(:a :b :c))))))

;; 28
;; Easy
;; seqs core-functions
;; Write a function which flattens a sequence.
;; Special Restrictions: flatten
(defn flatten-me
  [x]
  (filter (complement sequential?) (rest (tree-seq sequential? seq x))))
(deftest test-28
  (testing "Flatten a Sequence"
    (is (= (flatten-me '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))	
    (is (= (flatten-me ["a" ["b"] "c"]) '("a" "b" "c")))
    (is (= (flatten-me '((((:a))))) '(:a)))))

;; 29
;; Easy
;; strings
;; Write a function which takes a string and returns a new string containing only the capital letters.
(deftest test-29
  (testing "Get the Caps"
    (is (= (#(reduce str (re-seq #"[A-Z]" %)) "HeLlO, WoRlD!") "HLOWRD"))
    (is (empty? (#(reduce str (re-seq #"[A-Z]" %)) "nothing")))
    (is (= (#(reduce str (re-seq #"[A-Z]" %)) "$#A(*&987Zf") "AZ"))))

;; 30
;; Easy
;; seqs
;; Write a function which removes consecutive duplicates from a sequence .
(defn compress-seq
  ([coll] (compress-seq coll '()))
  ([coll rcoll]
     (if (empty? coll )
       (reverse rcoll)
       (recur (rest coll)
              (if (= (first coll)
                     (first rcoll))
                rcoll (conj rcoll (first coll)))))))
(deftest test-30
  (testing "Compress a Sequence"
    (is (= (apply str (#(compress-seq %) "Leeeeeerrroyyy")) "Leroy"))	
    (is (= (#(compress-seq %) [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
    (is (= (#(compress-seq %) [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

;; 31
;; Easy
;; seqs
;; Write a function which packs consecutive duplicates into sub-lists.
;;(deftest test-31
;;  (testing "Pack a Sequence"
;;    (is (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
;;    (is (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
;;    (is (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

;; 32
;; Easy
;; seqs
;; Write a function which duplicates each element of a sequence.
(defn duplicate-seq
  [x]
  (apply concat (map #(repeat 2 %) x)))
(deftest test-32
  (testing "Duplicate a Sequence"
    (is (= (duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
    (is (= (duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))	
    (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))	
    (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))))

;; 33
;; Easy
;; seqs
;; Write a function which replicates each element of a sequence of a sequence a variable number of times.
(defn replicate-seq
  [x n]
  (apply concat (map #(repeat n %) x)))
(deftest test-33
  (testing "Replicate a Sequence"
    (is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
    (is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
    (is (= (replicate-seq [4 5 6] 1) '(4 5 6)))
    (is (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
    (is (= (replicate-seq [44 33] 2) [44 44 33 33]))))

;; 34
;; Easy
;; seqs core-functions
;; Write a function which creates a list of all integers in a given range.
;; Special Restrictions: range
(deftest test-34
  (testing "Implement range"
    (is (= ((fn [x y] (take-while #(> y %) (iterate inc x))) 1 4) '(1 2 3)))
    (is (= ((fn [x y] (take-while #(> y %) (iterate inc x))) -2 2) '(-2 -1 0 1)))
    (is (= ((fn [x y] (take-while #(> y %) (iterate inc x))) 5 8) '(5 6 7)))))

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

