(ns zenlisp.core
  (:require
    [clojure.set :as cset]
    [clojure.string :as cs]))

(defn sum
  [xs]
  (cond
    (empty? xs) 0
    (= 1 (count xs)) (first xs)
    :else (let [[dep bel] (split-at (quot (count xs) 2) xs)]
            (+ (sum dep) (sum bel)))))

(defn sum'
  [[x & xs]]
  (if x (+ x (sum' xs)) 0))

(defn msum
  [xs]
  (reduce +' xs))

(defn sum''
  ([xs] (sum'' xs 0))
  ([[x & xs] res]
    (if x (recur xs (+ x res)) res)))

(defn sreverse
  "Returns a string that is the reverse of st"
  [st]
  (apply str (reverse st)))

(defn snth
  "Returns a string that is the result of (str (nth st n))"
  [st n]
  (str (nth st n)))

(defn stake
  "Like take, but produces a string"
  [n st]
  (apply str (take n st)))

(defn sdrop
  "Like drop, but produces a string"
  [n st]
  (apply str (drop n st)))

(defn stake-while
  "Like take-while but produces a string, and f takes a string form of each element in
  st instead of char"
  [f st]
  (-> (comp (map str) (take-while f))
      (transduce str st)))

(defn sdrop-while
  "Like drop-while but produces a string, and f takes a string form of each element in
  st instead of char"
  [f st]
  (-> (comp (map str) (drop-while f))
      (transduce str st)))

(defn sfilter
  "Like filter but produces a string, and f takes a string form of each element in
  st instead of char"
  [f st]
  (-> (comp (map str) (filter f))
      (transduce str st)))

(defn stake-last
  "Like take-last but produces a string"
  [n st]
  (apply str (take-last n st)))

(defn sdrop-last
  "Like drop-last but produces a string"
  [n st]
  (apply str (drop-last n st)))