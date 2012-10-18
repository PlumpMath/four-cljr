(ns four-cljr.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn problem-1 []
  (loop [iseq (range 1000) tsum 0]
    (if (empty? iseq)
      tsum
      (recur (rest iseq) (let [item (first iseq)]
                           (if (or (zero? (mod item 3))
                                   (zero? (mod item 5)))
                             (+ tsum item)
                             tsum))))))

(defn problem-2 []
  (loop [x 1 y 1 total 0]
    (if (> x 4000000)
      total
      (recur (+ x y) x (if (even? x)
                         (+ x total)
                         total)))))

(defn problem-4-40 [sp sq]
  (loop [isq sq rsq []]
    (if (empty? isq)
      (take (+ (count sq) (dec (count sq))) rsq)
      (recur (rest isq) (conj (conj rsq (first isq)) sp)))))