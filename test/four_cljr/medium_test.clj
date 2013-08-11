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

;; 54
;; Medium
;; seqs core-functions
;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
;; Special Restrictions - partition - partition-all

(defn partition-seq [n s]
  (if (>= (count s) n)
    (cons (take n s) (partition-seq n (drop n s)))))

(deftest test-54
  (testing "Partition a Sequence"
    (is (= (partition-seq 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
    (is (= (partition-seq 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
    (is (= (partition-seq 3 (range 8)) '((0 1 2) (3 4 5))))))

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

;; 59
;; Medium
;; higher-order-functions core-functions
;; Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
(defn juxta-position
  [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(deftest test-59
  (testing "Juxtaposition"
    (is (= [21 6 1] ((juxta-position + max min) 2 3 5 1 6 4)))
    (is (= ["HELLO" 5] ((juxta-position #(.toUpperCase %) count) "hello")))
    (is (= [2 6 4] ((juxta-position :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))))

;; 60
;; Medium
;; seqs core functions
;; Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy.
;; Special Restrictions - reductions

(defn my-reductions
  ([f coll]
   (lazy-seq
    (if-let [s (seq coll)]
      (my-reductions f (first s) (rest s))
      (list (f)))))
  ([f init coll]
   (cons init
         (lazy-seq
          (when-let [s (seq coll)]
            (my-reductions f (f init (first s)) (rest s)))))))
(deftest test-60
  (testing "Sequence Reductions"
    (is (= (take 5 (my-reductions + (range))) [0 1 3 6 10]))
    (is (= (my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
    (is (= (last (my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))))

;; 65
;; Medium
;; seqs testing
;; Clojure has many sequence types, which act in subtly different ways. The core functions typically convert them into a uniform "sequence" type and work with them that way, but it can be important to understand the behavioral and performance differences so that you know which kind is appropriate for your application.
;; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.
(defn seq-test
  [xs]
  (let [s (.toString xs)]
    (cond (.startsWith s "{") :map
          (.startsWith s "#{") :set
          (.startsWith s "[") :vector
          :else :list)))
(deftest test-65
  (testing "Black Box Testing"
    (= :map (seq-test {:a 1 :b 2}))
    (= :list (seq-test (range (rand-int 20))))
    (= :vector (seq-test [1 2 3 4 5 6]))
    (= :set (seq-test #{10 (rand-int 5)}))
    (= [:map :set :vector :list] (map seq-test [{} #{} [] ()]))))

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

;; 69
;; Medium
;; core-functions
;; Write a function which takes a function f and a variable number of maps. Your function should return a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)
;; Special Restrictions - merge-with

(defn my-merge-with
  [f m & maps]
  (if (empty? maps)
    m
    (recur f
           (let [m2 (first maps)]
             (loop [m1 m
                    ks (keys m2)]
               (if (empty? ks)
                 m1
                 (recur (let [k (first ks)
                              v (get m2 k)]
                          (if (contains? m1 (first ks))
                            (assoc m1 k (f (get m1 k) v))
                            (assoc m1 k v)))
                          (rest ks)))))
           (rest maps))))

(deftest test-69
  (testing "Merge with Function"
    (is (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
           {:a 4, :b 6, :c 20}))
    (is (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
           {1 7, 2 10, 3 15}))
    (is (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
           {:a [3 4 5], :b [6 7], :c [8 9]}))))

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

;; 76
;; Medium
;; recursion
;; The trampoline function takes a function f and a variable number of parameters. Trampoline calls f with any parameters that were supplied. If f returns a function, trampoline calls that function with no arguments. This is repeated, until the return value is not a function, and then trampoline returns that non-function value. This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.
(deftest test-76
  (testing "Into to Trampoline"
    (is (= [1 3 5 7 9 11]
           (letfn
             [(foo [x y] #(bar (conj x y) y))
              (bar [x y] (if (> (last x) 10)
                           x
                           #(foo x (+ 2 y))))]
             (trampoline foo [] 1))))))


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

;; 78
;; Medium
;; core-functions
;; Reimplement the function described in "Intro to Trampoline".
;; Special Restrictions - trampoline

(defn my-trampoline
  [f & args]
  (loop [res (apply f args)]
    (if (fn? res)
      (recur (res))
      res)))

(deftest test-78
  (testing "Reimplement Trampoline"
    (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                   (sub-two [x] #(stop?(- x 2)))
                   (stop? [x] (if (> x 50) x #(triple x)))]
             (my-trampoline triple 2))
           82))
    (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                   (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
             (map (partial my-trampoline my-even?) (range 6)))
           [true false true false true false]))))

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

;; 98
;; Medium
;; A function f defined on a domain D induces an equivalence relation on D, as follows: a is equivalent to b with respect to f if and only if (f a) is equal to (f b). Write a function with arguments f and D that computes the equivalence classes of D with respect to f.
(defn equi-classes
  [f s]
  (clojure.walk/walk #(into #{} %)
                     #(into #{} %)
                     (vals (group-by f s))))

(deftest test-98
  (testing "Equivalence Classes"
    (is (= (equi-classes #(* % %) #{-2 -1 0 1 2})
           #{#{0} #{1 -1} #{2 -2}}))
    (is (= (equi-classes #(rem % 3) #{0 1 2 3 4 5 })
           #{#{0 3} #{1 4} #{2 5}}))
    (is (= (equi-classes identity #{0 1 2 3 4})
           #{#{0} #{1} #{2} #{3} #{4}}))
    (is (= (equi-classes (constantly true) #{0 1 2 3 4})
           #{#{0 1 2 3 4}}))))

;; 102
;; Medium
;; strings
;; When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.

(defn intoCamelCase
  [s]
  (let [words (clojure.string/split s #"-")]
    (str (first words)
         (reduce str (map #(clojure.string/capitalize %) (rest words))))))

(deftest test-102
  (testing "Into Camel Case"
    (is (= (intoCamelCase "something") "something"))
    (is (= (intoCamelCase "multi-word-key") "multiWordKey"))
    (is (= (intoCamelCase "leaveMeAlone") "leaveMeAlone"))))

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

;; 114
;; Medium
;; seqs higher-order-functions
;; take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time. What if you need to keep track of some state as you go over the sequence?
;; Write a function which accepts an integer n, a predicate p, and a sequence. It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate.

(defn one-one-four
  [n p s]
  (lazy-seq
   (when-let [[c & r] (seq s)]
     (let [m (if (p c) (dec n) n)]
       (when (pos? m)
         (cons c (one-one-four m p r)))))))

(deftest test-114
  (testing "Global take-while"
    (= [2 3 5 7 11 13]
       (one-one-four 4 #(= 2 (mod % 3))
                     [2 3 5 7 11 13 17 19 23]))
    (= ["this" "is" "a" "sentence"]
       (one-one-four 3 #(some #{\i} %)
                     ["this" "is" "a" "sentence" "i" "wrote"]))
    (= ["this" "is"]
       (one-one-four 1 #{"a"}
                     ["this" "is" "a" "sentence" "i" "wrote"]))))

;; 115
;; Medium
;; math
;; A balanced number is one whose componenet digits have the same sum on the left and right halves of the number. Write a function which accepts an integer n and returns true iff n is balanced.
(defn is-balance?
  [n]
  (let [s (str n)
        l (.length s)
        q (quot l 2)]
    (cond (= l 1) true
          :else (let [xs (split-at q (into [] s))
                          ls (first xs)
                          rs (if (even? l)
                               (second xs)
                               (rest (second xs)))]
                      (= (reduce + (map #(Integer/valueOf (str %)) (into [] ls)))
                         (reduce + (map #(Integer/valueOf (str %)) (into [] rs))))))))

(deftest test-115
  (testing "The Balance Of N"
    (= true (is-balance? 11))
    (= true (is-balance? 121))
    (= false (is-balance? 123))
    (= true (is-balance? 0))
    (= false (is-balance? 88099))
    (= true (is-balance? 89098))
    (= true (is-balance? 89089))
    (= (take 20 (filter is-balance? (range)))
       [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])))


;; 116
;; Medium
;; math
;; A balanced prime is a prime number which is also the mean of the primes directly before and after it in the sequence of valid primes. Create a function which takes an integer n, and returns true if it is a balanced prime.

(defn prime-sandwich
  [pn]
  (cond (< pn 3) false
        (prime-number? pn)
        (let [pns (filter prime-number? (range))
              i (.indexOf pns pn)
              ppn (nth pns (dec i))
              npn (nth pns (inc i))]
          (= (/ (+ ppn npn) 2) pn))
        :else false))

(deftest test-116
  (testing "Prime Sandwich"
    (= false (prime-sandwich 4))
    (= true (prime-sandwich 563))
    (= 1103 (nth (filter prime-sandwich (range)) 15))))

;; 132
;; Medium
;; seqs core-functions
;; Write a function that takes a two-argument predicate, a value, and a collection; and returns a new collection where the value is inserted between every two items that satisfy the predicate.

(defn one-three-two
  [op k s]
  (if (empty? s)
    s
    (loop [xs s
           r []]
      (if (= 1 (count xs))
        (conj r (first xs))
        (recur (rest xs) (if (op (first xs) (second xs))
                           (conj (conj r (first xs)) k)
                           (conj r (first xs))))))))

(defn fn-132
  "Lazy-seq function"
  [f sym s]
  (letfn [(op [l s]
              (cond (empty? s) '()
                    (or (nil? l) (not (f l (first s))))
                    (cons (first s) (lazy-seq (op (first s) (rest s))))
                    true
                    (cons sym
                          (cons (first s) (lazy-seq
                                           (op (first s) (rest s)))))))]
    (op nil s)))

(deftest test-132
  (testing "Insert between two items"
    (= '(1 :less 6 :less 7 4 3) (one-three-two < :less [1 6 7 4 3]))
    (= '(2) (one-three-two > :more [2]))
    (= [0 1 :x 2 :x 3 :x 4]  (one-three-two #(and (pos? %) (< % %2)) :x (range 5)))
    (empty? (one-three-two > :more ()))))

;; 137
;; Medium
;; math
;; Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical system with an arbitrary base (second argument). Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.

(defn to-base
  [d b]
  (loop [id d
         res nil]
    (if (zero? id)
      (or res [0])
      (recur (quot id b)
             (cons (rem id b) res)))))

(deftest test-137
  (testing "Digits and bases"
    (is (= [1 2 3 4 5 0 1] (to-base 1234501 10)))
    (is (= [0] (to-base 0 11)))
    (is (= [1 0 0 1] (to-base 9 2)))
    (is (= [1 0] (let [n (rand-int 100000)](to-base n n))))
    (is (= [16 18 5 24 15 1] (to-base Integer/MAX_VALUE 42)))))