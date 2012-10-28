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
(defn pack-seq [xs]
  (loop [x xs r '()]
    (if (empty? x)
      (reverse r)
      (recur (rest x) (if (= (first x) (-> r first first))
                        (cons (cons (first x) (first r)) (rest r))
                        (cons (cons (first x) '()) r))))))
(deftest test-31
  (testing "Pack a Sequence"
    (is (= ((partial partition-by identity)
            [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
    (is (= ((partial partition-by identity)
            [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
    (is (= ((partial partition-by identity)
            [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

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

;; 38
;; Easy
;; core-functions
;; Write a function which takes a variable number of parameter and returns the maximum value.
;; Special Restrictions; max max-key
(defn find-max
  [& args] 
  (loop [col (rest args) mx (first args)] 
    (if (empty? col) 
      mx 
      (recur (rest col)
             (if (> (first col) mx) (first col) mx)))))
(deftest test-38
  (testing "Maximum value"
    (is (= (find-max 1 8 3 4) 8))
    (is (= (find-max 30 20) 30))
    (is (= (find-max 45 67 11) 67))))

;; 39
;; Easy
;; seqs core-functions
;; Write a function which takes two sequences and returns the first item from each , then the second item from each, then the third, etc.
;; Special Restrictions: interleave
(defn interleave-seq 
  [c1 c2]
  (loop [lc1 c1 lc2 c2 rc '()]
    (if (or (empty? lc1) (empty? lc2))
      rc
      (recur (rest lc1) (rest lc2) (concat rc (list (first lc1) (first lc2)))))))
(deftest test-39
  (testing "Interleave Two Seqs"
    (is (= (interleave-seq [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
    (is (= (interleave-seq [1 2] [3 4 5 6]) '(1 3 2 4)))
    (is (= (interleave-seq [1 2 3 4] [5]) [1 5]))
    (is (= (interleave-seq [30 20] [25 15]) [30 25 20 15]))))

;; 40
;; Easy
;; seqs core-functions
;; Write a function which separated the items of a sequence by an arbitrary value.
;; Special Restrictions: interpose
(defn interpose-seq
  [sp sq]
  (loop [isq sq rsq []]
    (if (empty? isq)
      (take (+ (count sq) (dec (count sq))) rsq)
      (recur (rest isq) (conj (conj rsq (first isq)) sp)))))
(deftest test-40
  (testing "Interpose a Seq"
    (is (= (interpose-seq 0 [1 2 3]) [1 0 2 0 3]))
    (is (= (apply str (interpose-seq ", " ["one" "two" "three"])) "one, two, three"))
    (is (= (interpose-seq :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))))

;; 41
;; Easy
;; seqs
;; Write a function which drops every Nth item from q sequence.
(defn drop-every-nth
  [x i]
  (loop [xs x n 1 rs [] index i]
    (if (empty? xs)
      rs
      (recur (rest xs)
             (if (= n index) 1 (inc n))
             (if (= n index) rs (conj rs (first xs))) index))))
(deftest test-41
  (testing "Drop Every Nth Item"
    (is (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
    (is (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
    (is (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

;; 42
;; Easy
;; math
;; Write a function which calculates factorials.
(deftest test-42
  (testing "Factorial Fun"
    (is (= (#(reduce * (range 1 (inc %))) 1) 1))
    (is (= (#(reduce * (range 1 (inc %))) 3) 6))
    (is (= (#(reduce * (range 1 (inc %))) 5) 120))
    (is (= (#(reduce * (range 1 (inc %))) 8) 40320))))

;; 45
;; Easy
;; seqs
;; The iterate function can be used to produce an infinite lazy sequence.
(deftest test-45
  (testing "Intro to Iterate"
    (is '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))))

;; 47
;; Easy
;; The contains? function checks if a KEY is present in a given collection.This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
(deftest test-47
  (testing "Contain Yourself"
    (is (contains? #{4 5 6} 4))
    (is (contains? [1 1 1 1 1] 4))
    (is (contains? {4 :a 2 :b} 4))
    (is (not (contains? '(1 2 4) 4)))))

;; 48
;; Easy
;; The some function takes a predicate function and a collection. It returns the first logical true value of (predicate x) where x is an item in the collection.
(deftest test-48
  (testing "Intro to some"
    (is (= 6 (some #{2 7 6} [5 6 7 8])))
    (is (= 6 (some #(when (even? %) %) [5 6 7 8])))))

;; 49
;; Easy
;; seqs core-functions
;; Special Restrictions: split-at
;; Write a function which will split a sequence into two parts.
(defn split-seq 
  [n xs] 
  [(take n xs) (drop n xs)])
(deftest test-49
  (testing "Split a sequence"
    (is (= (split-seq 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
    (is (= (split-seq 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
    (is (= (split-seq 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

;; 51
;; Easy
;; destructuring
;; Here is an example of some more sophisticated destructuring.
(deftest test-51
  (testing "Advanced Destructuring"
    (is (= [1 2 [3 4 5] [1 2 3 4 5]]
           (let [[a b & c :as d] (list 1 2 3 4 5)] [a b c d])))))

;; 52
;; Easy
;; destructuring
;; Let bindings and function parameter lists support destructuring
(deftest test-52
  (testing "Intro to Destructuring"
    (is (= [2 4] (let [[a b c d e f g] (range)] [c e])))))

;; 61
;; Easy
;; core-functions
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
(defn map-const
  [s1 s2]
  (loop [xs1 s1 xs2 s2 rmp {}]
    (if (or (empty? xs1) (empty? xs2))
      rmp
      (recur (rest xs1) (rest xs2) (assoc rmp (first xs1) (first xs2))))))
(deftest test-61
  (testing "Map Construction"
    (is (= (map-const [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
    (is (= (map-const [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
    (is (= (map-const [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

;; 62
;; Easy
;; seqs core-functions
;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;; Special Restrictions: iterate
(defn iter-ate
  [f x]
  (cons x (lazy-seq (iter-ate f (f x)))))
(deftest test-62
  (testing "Re-implement Iterate"
    (is (= (take 5 (iter-ate #(* 2 %) 1)) [1 2 4 8 16]))
    (is (= (take 100 (iter-ate inc 0)) (take 100 (range))))
    (is (= (take 9 (iter-ate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))))

;; 63
;; Easy
;; core-functions
;; Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s.
;; Special Restrictions: group-by
(defn group-seq
  [s1 s2]
  (loop [ixs s2 rs {}]
    (if (empty? ixs)
      rs
      (recur (rest ixs)
             (if (contains? rs (s1 (first ixs)))
               (assoc rs (s1 (first ixs))
                      (conj (get rs (s1 (first ixs))) (first ixs)))
               (assoc rs (s1 (first ixs)) [(first ixs)]))))))
(deftest test-63
  (testing "Group a Sequence"
    (is (= (group-seq
            #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
    (is (= (group-seq
            #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
           {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
    (is (= (group-seq
            count [[1] [1 2] [3] [1 2 3] [2 3]])
           {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

;; 66
;; Easy
;; Given two integers, write a function which returns the greatest common divisor.
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
(deftest test-66
  (testing "Greatest Common Divisor"
    (is (= (gcd 2 4) 2))
    (is (= (gcd 10 5) 5))
    (is (= (gcd 5 7) 1))
    (is (= (gcd 1023 858) 33))))

;; 81
;; Easy
;; set-theory
;; Write a function which returns the interection of two sets. The intersection is the sub-set of items that each set has in common.
;; Special Restrictions: intersection
(defn inter-section [s1 s2]
  (apply disj s1 (clojure.set/difference s1 s2)))
(deftest test-81
  (testing "Set Intersection"
    (is (= (inter-section #{0 1 2 3} #{2 3 4 5}) #{2 3}))
    (is (= (inter-section #{0 1 2} #{3 4 5}) #{}))
    (is (= (inter-section #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

;; 83
;; Easy
;; Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.
(defn half-truth
  [& xs]
  (and
   (not (every? identity xs))
   (boolean (some identity xs))))
(deftest test-83
  (testing "A Half-Truth"
    (is (= false (half-truth false false)))
    (is (= true (half-truth true false)))
    (is (= false (half-truth true)))
    (is (= true (half-truth false true false)))
    (is (= false (half-truth true true true)))
    (is (= true (half-truth true true true false)))))

;; 90
;; Easy
;; set-theory
;; Write a function which calculates the Cartesian product of two sets.
(defn cart-prod
  [x1 x2]
  (set (for [x x1 y x2] [x y])))
(deftest test-90
  (testing "Cartesian Product"
    (is (= (cart-prod #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
           #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
             ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
             ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
    (is (= (cart-prod #{1 2 3} #{4 5})
           #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    (is (= 300 (count (cart-prod (into #{} (range 10))
                                 (into #{} (range 30))))))))

;; 99
;; Easy
;; math seqs
;; Write a function which multiplies two numbers and return the result as a sequence of its digits
(defn product-digits
  [& more]
  (map #(Integer/valueOf (.toString %))
       (seq (.toString (apply * more)))))
(deftest test-99
  (testing "Product digits"
    (is (= (product-digits 1 1) [1]))
    (is (= (product-digits 99 9) [8 9 1]))
    (is (= (product-digits 999 99) [9 8 9 0 1]))))

;; 100
;; Easy
;; math
;; Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios.
(defn lcm
  [& xs]
  (reduce (fn [a b]
            (/ (* a b) ((fn [x y]
                          (if (zero? y)
                            x
                            (recur y (mod x y)))) a b))) xs))
(deftest test-100
  (testing "Least common multiple"
    (is (== (lcm 2 3) 6))
    (is (== (lcm 5 3 7) 105))
    (is (== (lcm 1/3 2/5) 2))
    (is (== (lcm 3/4 1/6) 3/2))
    (is (== (lcm 7 5/7 2 3/5) 210))))

;; 107
;; Easy
;; higher-order-functions math
;; Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called lexical closures. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.

;; It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n, return a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined.
(defn simple-closure
  [n]
  (fn [x]
    (reduce *(take n (repeat x)))))
(deftest test-107
  (testing "Simple closures"
    (is (= 256 ((simple-closure 2) 16) ((simple-closure 8) 2)))
    (is (= [1 8 27 64] (map (simple-closure 3) [1 2 3 4])))      
    (is (= [1 2 4 8 16] (map #((simple-closure %) 2) [0 1 2 3 4])))))

;; 120
;; Easy
;; math
;; Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
(defn sum-sq-digits
  [iseq]
  (loop [numrs (take 100 iseq) cnt 0]
    (if (empty? numrs)
      cnt
      (recur (rest numrs)
             (if (< (first numrs)
                    (apply + (map #(* % %) (map #(Integer/valueOf (.toString %))
                                                (seq (.toString (first numrs)))))))
               (inc cnt)
               cnt)))))
(deftest test-120
  (testing "Sum of square of digits"
    (is (= 8 (sum-sq-digits (range 10))))
    (is (= 19 (sum-sq-digits (range 30))))
    (is (= 50 (sum-sq-digits (range 100))))
    (is (= 50 (sum-sq-digits (range 1000))))))

;; 135
;; Easy
;; higher-order-functions math
;; Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation. Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right.
(defn infix-cal
  [x op & xs]
  (if (empty? xs)
    x
    (recur (op x (first xs)) (second xs) (-> xs rest rest))))
(deftest test-135
  (testing "Infix Calculator"
    (is (= 7  (infix-cal 2 + 5)))
    (is (= 42 (infix-cal 38 + 48 - 2 / 2)))
    (is (= 8  (infix-cal 10 / 2 - 1 * 2)))
    (is (= 72 (infix-cal 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))))

;; 143
;; Easy
;; seqs math
;; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
(deftest test-143
  (testing "dot product"
    (is (= 0 (#(apply + (map * %1 %2)) [0 1 0] [1 0 0])))
    (is (= 3 (#(apply + (map * %1 %2)) [1 1 1] [1 1 1])))
    (is (= 32 (#(apply + (map * %1 %2)) [1 2 3] [4 5 6])))
    (is (= 256 (#(apply + (map * %1 %2)) [2 5 6] [100 10 1])))))

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

