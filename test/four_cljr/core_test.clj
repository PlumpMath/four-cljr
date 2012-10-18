(ns four-cljr.core-test
  (:use clojure.test
        four-cljr.core)
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

