(ns four-cljr.elementary-test
  (:use clojure.test)
  (:require clojure.set))

;; 1
;; Elementory
;; This is a clojure form. Enter a value which will make the form to evaluate to true. Dont over think it! If you are confused, see the getting started page.
;; Hint: true is equal to true.
(deftest test-1
  (testing "Nothing but the truth"
    (is (= true true))))

;; 2
;; Elementary
;; If you are not familiar with polish notation, simple arithmetic might seem confusing.
;; Note: Enter only enough to fill in the blank (in this case, a single number) - do not retype the while problem.
(deftest test-2
  (testing "Simple Math"
    (is (= (- 10 (* 2 3)) 4))))

;; 3
;; Elementary
;; Clojure strings are Java strings. This means that you can use any of the Java string methods on Clojure strings.
(deftest test-3
  (testing "Intro to Strings"
    (is (= "HELLO WORLD" (.toUpperCase "hello world")))))

;; 4
;; Elementary
;; List can be constructed with either a function or a quoted form.
(deftest test-4
  (testing "Intro to Lists"
    (is (= (list :a :b :c) '(:a :b :c)))))

;; 5
;; Elementory
;; When operating on a list, the conj function will return a new list with one or more items "added to the font.
(deftest test-5
  (testing "Lists: conj"
    (is (= '(1 2 3 4) (conj '(2 3 4) 1)))
    (is (= '(1 2 3 4) (conj '(3 4) 2 1)))))

;; 6
;; Elementary
;; Vectos can be constructed several ways. You can compare them with lists.
(deftest test-6
  (testing "Intro to Vectos"
    (is (= [:a :b :c]
           (list :a :b :c)
           (vec '(:a :b :c))
           (vector :a :b :c)))))

;; 7
;; Elementary
;; When operating on a Vector, the conj function will return a new vector with one or more items "added" to the end.
(deftest test-7
  (testing "Vectors. conj"
    (is (= [1 2 3 4] (conj [1 2 3] 4)))
    (is (= [1 2 3 4] (conj [1 2] 3 4)))))

;; 8
;; Elementary
;; Sets are collections of unique values.
(deftest test-8
  (testing "Intro to sets"
    (is (= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d))))
    (is (= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d})))))
           
;; 9
;; Elementary
;; When operating on a set, the conj function returns a new set with one or more keys "added".
(deftest test-9
  (testing "Sets: conj"
    (is (= #{1 2 3 4} (conj #{1 4 3} 2)))))

;; 10
;; Elementary
;; Maps store key-value pairs. Both maps and keywords can be used as lookup functions. Commas can be used to make maps more readable, but they are not required.
(deftest test-10
  (testing "Intro to Maps"
    (is (= 20 ((hash-map :a 10, :b 20, :c 30) :b)))
    (is (= 20 (:b {:a 10, :b 20, :c 30})))))

;; 11
;; Elementary
;; When operating on a map, the conj function returns a new map with one or more key-value pairs "added".
(deftest test-11
  (testing "Maps: conj"
    (is (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))))

;; 12
;; Elementary
;; All Clojure collections support sequencing. You can operate on sequences with functions like first, second, and last.
(deftest test-12
  (testing "Intro to Sequences"
    (is (= 3 (first '(3 2 1))))
    (is (= 3 (second [2 3 4])))
    (is (= 3 (last (list 1 2 3))))))

;; 13
;; The rest function will return all the items of a sequence expect the first.
(deftest test-13
  (testing "Sequences: rest"
    (is (= (list 20 30 40) (rest [10 20 30 40])))))

;; 14
;; Elementary
;; Clojure has manu different ways to create functions.
(deftest test-14
  (testing "Intro to Functions"
    (is (= 8 ((fn add-five [x] (+ x 5)) 3)))
    (is (= 8 ((fn [x] (+ x 5)) 3)))
    (is (= 8 (#(+ % 5) 3)))
    (is (= 8 ((partial + 5) 3)))))

;; 15
;; Elementary
;; Write a function which doubles a number.
(deftest test-15
  (testing "Double Down"
    (is (= ((fn do-double [x] (* x 2)) 2) 4))
    (is (= ((fn do-double [x] (* x 2)) 3) 6))
    (is (= ((fn do-double [x] (* x 2)) 11) 22))
    (is (= ((fn do-double [x] (* x 2)) 7) 14))))

;; 16
;; Elementary
;; Write a function which returns a personalized greeting.
(deftest test-16
  (testing "Hello World"
    (is (= ((fn greeting [name] (str "Hello, " name "!")) "Dave")
           "Hello, Dave!"))
    (is (= ((fn greeting [name] (str "Hello, " name "!")) "Jenn")
               "Hello, Jenn!"))
    (is (= ((fn greeting [name] (str "Hello, " name "!")) "Rhea")
           "Hello, Rhea!"))))

;; 17
;; Elementary
;; The map function takes two arguments: a function (f) and a sequence (s). Map returns a new sequence consisting of the result of applying f to each item of s. Do not confuse the map function with the map data structure.
(deftest test-17
  (testing "Sequences: map"
    (is (= (list 6 7 8) (map #(+ % 5) '(1 2 3))))))

;; 18
;; Elementary
;; The filter function takes two arguments: a predicate function (f) and a sequence (s). Filter returns a new sequence consisting of all the items of s for which (f item) returns true.
(deftest test-18
  (testing "Sequences: filter"
    (is (= '(6 7) (filter #(> % 5) '(3 4 5 6 7))))))

;; 35
;; Elementary
;; syntax
;; Clojure lets you give local names to values using the special let-form.
(deftest test-35
  (testing "Local bindings"
    (is (= 7 (let [x 5] (+ 2 x))))
    (is (= 7 (let [x 3, y 10] (- y x))))
    (is (= 7 (let [x 21] (let [y 3] (/ x y)))))))

;; 36
;; Elementary
;; math syntax
;; Can you bind x, y, and z so that these are all true?
(deftest test-36
  (testing "Let it Be"
    (is (= 10 (let [x 7 y 3 z 1] (+ x y))))
    (is (= 4 (let [x 7 y 3 z 1] (+ y z))))
    (is (= 1 (let [x 7 y 3 z 1] z)))))

;; 37
;; Elementary
;; regex syntax
;; Regex patterns are supported with a speial reader macro.
(deftest test-37
  (testing "Regular Expressions"
    (is (= (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))))

;; 57
;; Elementary
;; recursion
;; A recursion function which calls itself. This is one of the fundamental techniques used in functional programming.
(deftest test-57
  (testing "Simple Recursion"
    (is (= '(5 4 3 2 1)
           ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))))

;; 64
;; Elementary
;; seqs
;; Reduce takes a 2 argument function and an optional starting value. It then applies the function to the first 2 items in the sequence (or the starting value and the first element of the sequence). In the next iteration the function will be called on the previous return value and the next item from the sequence, thus reducing the entire collection to one value. Don't worry, it's not as complicated as it sounds.
(deftest test-64
  (testing "Intro to Reduce"
    (is (= 15 (reduce + [1 2 3 4 5])))
    (is (=  0 (reduce + [])))
    (is (=  6 (reduce + 1 [2 3])))))

;; 68
;; Elementary
;; recursion
;; Clojure only has one non-stack-consuming looping construct: recur. Either a function or a loop can be used as the recursion point. Either way, recur rebinds the bindings of the recursion point to the values it is passed. Recur must be called from the tail-position, and calling it elsewhere will result in an error.
(deftest test-68
  (testing "Recurring Theme"
    (is (= [7 6 5 4 3]
           (loop [x 5
                  result []]
             (if (> x 0)
               (recur (dec x) (conj result (+ 2 x)))
               result))))))

;; 71
;; Elementary
;; The -> macro threads an expression x through a variable number of forms. First, x is inserted as the second item in the first form, making a list of it if it is not a list already. Then the first form is inserted as the second item in the second form, making a list of that form if necessary. This process continues for all the forms. Using -> can sometimes make your code more readable.
(deftest test-71
  (testing "Rearranging Code: ->"
    (is (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
           (-> [2 5 4 1 3 6] reverse rest sort last) 5))))

;; 72
;; Elementary
;; The ->> macro threads an expression x through a variable number of forms. First, x is inserted as the last item in the first form, making a list of it if it is not a list already. Then the first form is inserted as the last item in the second form, making a list of that form if necessary. This process continues for all the forms. Using ->> can sometimes make your code more readable.
(deftest test-72
  (testing "Rearranging Code: ->>"
    (is (= (#(reduce + %) (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (#(reduce + %)))
           11))))

;; 134
;; Elementary
;; maps
;; Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil.
(deftest test-134
  (testing "A nil key"
    (is (true?  (#(and (contains? %2 %1) (nil? (%1 %2))) :a {:a nil :b 2})))
    (is (false? (#(and (contains? %2 %1) (nil? (%1 %2))) :b {:a nil :b 2})))
    (is (false? (#(and (contains? %2 %1) (nil? (%1 %2))) :c {:a nil :b 2})))))

;; 145
;; Elementary
;; core-functions seqs
;; Clojure's for macro is a tremendously versatile mechanism for producing a sequence based on some other sequence(s). It can take some time to understand how to use it properly, but that investment will be paid back with clear, concise sequence-wrangling later. With that in mind, read over these for expressions and try to see how each of them produces the same result.
(deftest test-145
  (testing "For the win"
    (is (= '(1 5 9 13 17 21 25 29 33 37) (for [x (range 40)
                                               :when (= 1 (rem x 4))]
                                           x)))
    (is (= '(1 5 9 13 17 21 25 29 33 37) (for [x (iterate #(+ 4 %) 0)
                                               :let [z (inc x)]
                                               :while (< z 40)]
                                           z)))
    (is (= '(1 5 9 13 17 21 25 29 33 37) (for [[x y] (partition 2 (range 20))]
                                           (+ x y))))))

;; 156
;; Elementary
;; seqs
;; When retrieving values from a map, you can specify default values in case the key is not found:
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map.
(defn map-defaults
  [dv arr]
  (loop [rm {} keys arr]
    (if (empty? keys)
      rm
      (recur (assoc rm (first keys) dv) (rest keys)))))
(deftest test-156
  (testing "Map Defaults"
    (is (= (map-defaults 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (map-defaults "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (map-defaults [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

;; 161
;; Elementary
;; set-theory
;; Set A is a subset of set B, or equivalently B is a superset of A, if A is "contained" inside B. A and B may coincide.
(deftest test-161
  (testing "Subset and Superset"
    (is (clojure.set/superset? #{1 2} #{2}))
    (is (clojure.set/subset? #{1} #{1 2}))
    (is (clojure.set/superset? #{1 2} #{1 2}))
    (is (clojure.set/subset? #{1 2} #{1 2}))))

;; 162
;; Elementary
;; logic
;; In Clojure, only nil and false representing the values of logical falsity in conditional tests - anything else is logical truth.
(deftest test-162
  (testing "Logical fasity and truth"
    (is (= 1 (if-not false 1 0)))
    (is (= 1 (if-not nil 1 0)))
    (is (= 1 (if true 1 0)))
    (is (= 1 (if [] 1 0)))
    (is (= 1 (if [0] 1 0)))
    (is (= 1 (if 0 1 0)))
    (is (= 1 (if 1 1 0)))))