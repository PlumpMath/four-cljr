(ns four-cljr.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; alternate solution to problem-1 is
;; (reduce + (filter #(or (zero? (mod % 3)) (zero? (mod % 5))) (range 1000)))
;; with profiler
;; (time (reduce + (filter #(or (zero? (mod % 3)) (zero? (mod % 5))) (range 1000))))
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

(defn primefactors 
  ([n] 
    (primefactors n 2 '()))
  ([n candidate acc]
    (cond (<= n 1) (reverse acc)
          (zero? (rem n candidate)) (recur (/ n candidate) candidate (cons candidate acc))
          :else (recur n (inc candidate) acc))))

(defn visit
  [node]
  (println node))

(defn inorder
  [node]
  (if-not (coll? node)        
    (visit node)
    (do (inorder (second node)) 
      (visit (first node)) 
      (inorder (nth node 2)))))
(defn in-order
  ([node] (in-order node (atom [])))
  ([node rs]
     (if-not (coll? node)
       (swap! rs conj node)
       (do (in-order (second node) rs)
           (swap! rs conj (first node))
           (in-order (nth node 2) rs)))))

;;(inorder [:a [:b 1 2] [:b 2 1]])

;;boolean mirrorEquals(BTree left, BTree right) {
;;  if (left == null || right == null) return left == null && right == null;
;;  return left.value == right.value && mirrorEquals(left.left, right.right) && mirrorEquals(left.right, right.left);
;;}

(defn mirror-equals?
  [left right]
  (if (or (nil? left) (nil? right))
    (and (nil? left) (nil? right))
    (and (= (if (coll? left) (first left) left) (if (coll? right) (first right) right))
         (mirror-equals? (second left) (nth right 2))
         (mirror-equals? (nth left 2) (second right)))))

(defn preorder
  [node]
  (if-not (coll? node)
    (visit node)
    (do (visit (first node))
      (preorder (second node))
      (preorder (nth node 2)))))

;; (preorder [:a [:b 1 2] [:b 2 1]])

(defn postorder
  [node]
  (if-not (coll? node)
    (visit node)
    (do (postorder (second node))
      (postorder (nth node 2))
      (visit (first node)))))

;;(postorder [:a [:b 1 2] [:b 2 1]])

(defn tree-into-tables
  [m]
  (apply merge         
         (for [x m y (second x)]
           {[(first x) (first y)] (second y)})))