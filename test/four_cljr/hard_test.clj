(ns four-cljr.hard-test
  (:use clojure.test)
  (:require clojure.set))

;; Hard type of problems

;; 178
;; Hard
;; strings game

;; Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards.
;; The hand rankings are listed below for your convenience.
;; Straight flush: All cards in the same suit, and in sequence
;; Four of a kind: Four of the cards have the same rank
;; Full House: Three cards of one rank, the other two of another rank
;; Flush: All cards in the same suit
;; Straight: All cards in sequence (aces can be high or low, but not both at once)
;; Three of a kind: Three of the cards have the same rank
;; Two pair: Two pairs of cards have the same rank
;; Pair: Two cards have the same rank
;; High card: None of the above conditions are met

(defn problem-178
  [xs]
  (let [ranks-order ["23456789TJQKA" "A23456789TJQK" "AKQJT98765432" "KQJT98765432A"]
        ss (map (fn [[s r]] (str s)) xs)
        rs (map (fn [[s r]] (str r)) xs)
        rs-str (reduce str rs)
        in-seq? (empty? (drop-while nil? (map #(re-find (re-pattern rs-str) %) ranks-order)))
        suits (vals (group-by identity ss))
        ranks (vals (group-by identity rs))
        flush-count (count (filter #(= 5 (count %)) suits))
        four-kind-count (count (filter #(= 4 (count %)) ranks))
        three-kind-count (count (filter #(= 3 (count %)) ranks))
        pair-count (count (filter #(= 2 (count %)) ranks))]
    (cond (and (= 1 flush-count) (not in-seq?)) :straight-flush
          (= 1 four-kind-count) :four-of-a-kind
          (and (= 1 three-kind-count) (= 1 pair-count)) :full-house
          (= 1 flush-count) :flush
          (not in-seq?) :straight
          (= 1 three-kind-count) :three-of-a-kind
          (= 2 pair-count) :two-pair
          (= 1 pair-count) :pair
          :else :high-card)))

(deftest test-178
  (testing "Best Hand"
    (= :pair (problem-178 ["HA" "HQ" "SJ" "DA" "HT"]))
    (= :two-pair (problem-178 ["HA" "DA" "HQ" "SQ" "HT"]))
    (= :three-of-a-kind (problem-178 ["HA" "DA" "CA" "HJ" "HT"]))
    (= :four-of-a-kind (problem-178 ["HA" "DA" "CA" "SA" "DJ"]))
    (= :flush (problem-178 ["HA" "HK" "H2" "H4" "HT"]))
    (= :full-house (problem-178 ["HA" "DA" "CA" "HJ" "DJ"]))
    (= :straight (problem-178 ["HA" "H2" "S3" "D4" "C5"]))
    (= :straight (problem-178 ["HA" "DK" "HQ" "HJ" "HT"]))
    (= :straight-flush (problem-178 ["HA" "HK" "HQ" "HJ" "HT"]))))

