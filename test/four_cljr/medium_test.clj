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

;; 58
;; Medium
;; higner-order-functions core-functions
;; Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left.
(defn my-comp
  [& fns]
  (fn [x & more]
    (let [rfns (reverse fns)
          ffn (first rfns)
          fr (apply ffn (list* x more))]
      (loop [f (rest rfns)
             r fr]
        (if (empty? f)
          r
          (recur (rest f) ((first f) r)))))))
(deftest test-58
  (testing "Functon Composition"
    (= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
    (= 5 ((my-comp (partial + 3) second) [1 2 3 4]))
    (= true ((my-comp zero? #(mod % 8) +) 3 5 7 9))
    (= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;; 67
;; Medium
;; Prime Numbers
;; Write a function which returns the first x number of prime numbers.
(defn prime-number?
  [n]
  (if (= n 1)
    false
    (loop [i 2
           prime? true]
      (if (or (= i n) (not prime?))
        prime?
        (recur (inc i) (not (zero? (mod n i))))))))

(defn prime-numbers
  [x]
  (take x (filter prime-number? (range))))

(deftest test-67
  (testing "Prime Numbers"
    (is (= (prime-numbers 2) [2 3]))
    (is (= (prime-numbers 5) [2 3 5 7 11]))
    (is (= (last (prime-numbers 100)) 541))))

;; 70
;; Medium
;; Word Sorting
;; sorting
;; Write a function that splits a sentence up into a sorted list of words. Capitalization should not affect sort order and punctuation should be ignored.
(defn sort-word
  [str]
  (sort-by clojure.string/upper-case
           (clojure.string/split (clojure.string/replace str #"\.|\!" "") #"\ ")))

(deftest test-70
  (testing "Word Sorting"
    (is (= (sort-word  "Have a nice day.")
           ["a" "day" "Have" "nice"]))
    (is (= (sort-word  "Clojure is a fun language!")
           ["a" "Clojure" "fun" "is" "language"]))
    (is (= (sort-word  "Fools fall for foolish follies.")
           ["fall" "follies" "foolish" "Fools" "for"]))))

;; 74
;; Medium
;; Filter perfect square
;; Given a string of comma separated integers, write a function which returns a new comma separated string that only contains the numbers which are perfect squares.
(defn perfect-square?
  [s]
  (let [inum (Integer/valueOf s)
        sqr (int (Math/sqrt inum))]
    (= (* sqr sqr) inum)))

(defn perfect-squares
  [s]
  (reduce #(str %1 "," %2)
          (filter perfect-square? (clojure.string/split s #"\,"))))

(deftest test-74
  (testing "Filter perfect square"
    (is (= (perfect-squares "4,5,6,7,8,9") "4,9"))
    (is (= (perfect-squares "15,16,25,36,37") "16,25,36"))))

;; 77
;; Medium
;; Anagram Finder
;; Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. Each sub-set should have at least two words. Words without any anagrams should not be included in the result.
(defn anagram-finder
  [xs]
  (set (filter #(> (count %) 1)
               (map set (vals (group-by (fn [s]
                                          (sort (seq s)))
                                        xs))))))
(deftest test-77
  (testing "Anagram Finder"
    (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"])
           #{#{"meat" "team" "mate"}}))
    (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
           #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))))

;; 80
;; Medium
;; Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
(defn perfect-number
  "Perfect Number"
  [n]
  (= n (reduce +
               (filter #(zero? (mod n %))
                       (range 1 n)))))
(deftest test-80
  (testing "Perfect Number"
    (= (perfect-number 6) true)
    (= (perfect-number 7) false)
    (= (perfect-number 496) true)
    (= (perfect-number 500) false)
    (= (perfect-number 8128) true)))

;; 86
;; Medium
;; math
;; Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not.
(defn happy-number
  ([n] (happy-number n #{}))
  ([n s]
   (let [hn (clojure.walk/walk
             #(let [x (Integer/valueOf (str %))]
                (* x x))
             #(apply + %)
             (seq (str n)))]
     (cond (= hn 1) true
           (contains? s hn) false
           :else (recur hn (conj s hn))))))

(deftest test-86
  (testing "Happy numbers"
    (= (happy-number 7) true)
    (= (happy-number 986543210) true)
    (= (happy-number 2) false)
    (= (happy-number 3) false)))

;; 105
;; Medium
;; map seqs
;; Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.

(defn map-seqs
  [xs]
  (loop [s xs
         r {}
         k (let [a (first s)]
             (if (keyword? a) a nil))]
    (if (empty? s)
      r
      (recur (rest s)
             (let [f (first s)]
               (if (keyword? f)
                 (assoc r f [])
                 (assoc r k (conj (k r) f))))
             (let [f (first s)]
               (if (keyword? f)
                 f
                 k))))))

(deftest test-105
  (testing "Identify keys and values"
    (= {} (map-seqs []))
    (= {:a [1]} (map-seqs [:a 1]))
    (= {:a [1], :b [2]} (map-seqs [:a 1, :b 2]))
    (= {:a [1 2 3], :b [], :c [4]} (map-seqs [:a 1 2 3 :b :c 4]))))

;; 108
;; Medium
;; seqs sorting
;; Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences. The sequences may be infinite, so be careful to search lazily.
(defn lazy-search
  ([s] (apply min s))
  ([s1 s2]
   (loop [x s1]
     (if (some #(= (first x) %) s2)
       (first x)
       (recur (rest x)))))
  ([s1 s2 s3]
   (let [x (take 70 s1)
         y (take 10 s2)
         z (take 70 s3)]
     (loop [s x]
       (if (and (some #(= (first s) %) y)
                (some #(= (first s) %) z))
       (first s)
       (recur (rest s)))))))

(deftest test-108
  (testing "Lazy Searching"
    (= 3 (lazy-search [3 4 5]))
    (= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
    (= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13]))
    (= 64 (lazy-search (map #(* % % %) (range))
          (filter #(zero? (bit-and % (dec %))) (range))
          (iterate inc 20)))))

