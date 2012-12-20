(ns four-cljr.medium-test
  (:use clojure.test)
  (:require clojure.set))

;; Medium Type of problems.

;; 43
;; Medium
;; seqs
;; Write a function which reverses the interleave process into x number of subsequences.
(defn reverse-interleave
  [coll n]
  (partition-all (/ (count coll) n)
                 (apply interleave (partition-all n coll))))
(deftest test-43
  (testing "Reverse Interleave"
    (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

;; 44
;; Medium
;; seqs
;; Write a function which can rotate a sequence in either direction.
(defn rotate-seq
  [n coll]
  (let [[a b] (split-at (mod n (count coll)) coll)]
    (concat b a)))
(deftest test-44
  (testing "Rotate Sequence"
    (is (= (rotate-seq 2 [1 2 3 4 5]) '(3 4 5 1 2)))
    (is (= (rotate-seq -2 [1 2 3 4 5]) '(4 5 1 2 3)))
    (is (= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1)))
    (is (= (rotate-seq 1 '(:a :b :c)) '(:b :c :a)))
    (is (= (rotate-seq -4 '(:a :b :c)) '(:c :a :b)))))



;; 46
;; Medium
;; higher-order-functions
;; Write a higher-order function which flips the order of the arguments of an input function
(defn flipping-out
  [f]
  (fn [index coll]
    (f coll index)))
(deftest test-46
  (testing "Flipping out"
    (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
    (is (= true ((flipping-out >) 7 8)))
    (is (= 4 ((flipping-out quot) 2 8)))
    (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3)))))

;; 50
;; Medium
;; seqs
;; Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
(defn split-by-type
  [s]
  (set (vals (group-by #(class %) s))))
(deftest test-50
  (testing "Split by Type"
    (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))))

;; 55
;; Medium
;; seqs core-functions
;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
;; Special Restrictions : frequencies
(defn count-occurence
  [sq]
  (loop [s sq r {}]
    (if (empty? s)
      r
      (recur (rest s) (if (nil? (get r (first s)))
                        (assoc r (first s) 1)
                        (update-in r [(first s)] inc))))))
(deftest test-55
  (testing "Count Occurrences"
    (is (= (count-occurence [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
    (is (= (count-occurence [:b :a :b :a :b]) {:a 2, :b 3}))
    (is (= (count-occurence '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

;; 56
;; Medium
;; seqs core-functions
;; Write a function which removes the duplicates from a sequence
(defn distinct-items
  [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen f) 
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen f))))))
                 xs seen)))]
    (step coll #{})))

(deftest test-56
  (testing "Find Distinct Items"
    (is (= (distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
    (is (= (distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
    (is (= (distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
    (is (= (distinct-items (range 50)) (range 50)))))
