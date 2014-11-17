(ns cryptopals.frequencies
  (:refer-clojure :exclude [frequencies])
  (:require [cryptopals.util :as util]
            [clojure.data.priority-map :as p]
            [clojure.set :as set]
            [clojure.string :as str]))

(def english-frequencies
  "from http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html"
  (p/priority-map-by >
                     \e 0.1202
                     \t 0.0910
                     \a 0.0812
                     \o 0.0768
                     \i 0.0731
                     \n 0.0695
                     \s 0.0628
                     \r 0.0602
                     \h 0.0592
                     \d 0.0432
                     \l 0.0398
                     \u 0.0288
                     \c 0.0271
                     \m 0.0261
                     \f 0.0230
                     \y 0.0211
                     \w 0.0209
                     \g 0.0203
                     \p 0.0182
                     \b 0.0149
                     \v 0.0111
                     \k 0.0069
                     \x 0.0017
                     \q 0.0011
                     \j 0.0010
                     \z 0.0007))

(def std-char-per-word "http://www.wolframalpha.com/input/?i=average+english+word+length"
  5.1)

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  {:added "1.2"
   :static true}
  [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          (p/priority-map-by >) coll))

(defn normalise-frequencies [m]
  (let [denom (reduce + (vals m))]
    (util/fmap #(/ % denom) m)))

(defn char-score [expected actual score-out-of]
  (max 0 (- score-out-of (Math/abs ^double (- expected actual)))))

(defn score-text [expected-frequencies text]
  (let [word-count (inc (get (clojure.core/frequencies text) \space 0))
        trimmed-text (str/lower-case (str/replace text #"\W" "")) ;; Remove non word characters
        char-per-word (/ (count trimmed-text) word-count)
        freq (frequencies trimmed-text)
        top6exp (set (keys (take 6 expected-frequencies)))
        bot6exp (set (keys (take-last 6 expected-frequencies)))
        top6found (set (keys (take 6 freq)))
        bot6found (set (keys (take-last 6 freq)))]
    (+ (count (set/intersection top6exp top6found))
       (count (set/intersection bot6exp bot6found))
       (char-score std-char-per-word char-per-word 4))))

(def score-english-text (partial score-text english-frequencies))

(defn frequency-diff [m1 m2]
  "Diffs two maps and returns a similarity score"
  (map
    (fn [[k v]]
      (- (get m2 k) v))
    m1))