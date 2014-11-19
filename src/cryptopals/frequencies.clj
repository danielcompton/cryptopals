(ns cryptopals.frequencies
  (:require [cryptopals.util :as util]
            [clojure.string :as str]
            [cryptopals.bytes :as b])
  (:import [java.util Arrays]))

(def english-frequencies-space
  "from http://www.macfreek.nl/memory/Letter_Distribution"
  (hash-map
    \space 18.28846265
    \e 10.26665037
    \t 7.51699827
    \a 6.53216702
    \o 6.15957725
    \n 5.71201113
    \i 5.66844326
    \s 5.31700534
    \r 4.98790855
    \h 4.97856396
    \l 3.31754796
    \d 3.28292310
    \u 2.27579536
    \c 2.23367596
    \m 2.02656783
    \f 1.98306716
    \w 1.70389377
    \g 1.62490441
    \p 1.50432428
    \y 1.42766662
    \b 1.25888074
    \v 0.79611644
    \k 0.56096272
    \x 0.14092016
    \j 0.09752181
    \q 0.08367550
    \z 0.05128469))

(defn normalise
  "Normalises the values in a map"
  ([m]
    (let [total (reduce + (vals m))]
      (normalise m total)))
  ([m total]
    (util/fmap #(/ % total) m)))

(def alphabet-0 (conj (zipmap (map char (range 97 123)) (repeat 26 0)) [\space 0]))

(defn least-squares [expected observed]
  (assert (= 27 (count expected) (count observed)))
  (->> observed
       (merge-with (fn [exp obs] (Math/pow (- obs exp) 2)) expected)
       (vals)
       (apply +)))

;; TODO make this use arrays instead of maps. Much faster.
(defn score-text [expected text]
  (let [trimmed-text (str/lower-case (str/replace text #"[^a-zA-Z ]" ""))
        char-count (count text)
        observed (normalise (merge alphabet-0 (frequencies trimmed-text)) char-count)
        non-ascii-chars (count (str/replace text #"\p{ASCII}" ""))
        punct-chars (count (str/replace text #"[^\p{Punct}]" ""))]
    (if (zero? (count trimmed-text))
      nil
      (+ (least-squares expected observed)
         ;; Could check for capital letters in the middle of a sentence
         (* 0.05 non-ascii-chars)
         (* 0.02 (if (< punct-chars 2) 0 punct-chars))))))

(def score-english-text (partial score-text (normalise english-frequencies-space)))

(defn crack-key [keys ^bytes ciphertext]
  (->> (map (fn [key ba]
              (let [xor-val (b/xor key ba)
                    str-val (String. ^bytes xor-val "UTF-8")
                    score (score-english-text str-val)]
                {:str str-val :key key :score score}))
            keys
            (repeat ciphertext))
       (remove #(nil? (:score %)))))

;; TODO: check if faster with LongBuffer
(defn hamming-distance [ba1 ba2]
  (.bitCount (BigInteger. (b/xor ba1 ba2))))

(defn hamming-distance-str [^String s1 ^String s2]
  (hamming-distance (.getBytes s1) (.getBytes s2)))

(defn keysize-score
  [^bytes ba size]
  (let [samples 1]
    (->> (range 0 (alength ba) size)
         (partition 2 1)
         (map (fn [[^Long from ^Long to]] (Arrays/copyOfRange ba from to)))
         (partition 2 1)
         (map (fn [[ba1 ba2]] (/ (hamming-distance ba1 ba2) size samples)))
         (take samples)
         (reduce +)
         (float)
         (hash-map :key-size size :score))))

(defn guess-keysize
  [^bytes ba from to]
  (->> (map (partial keysize-score ba) (range from (inc to)))
       (sort-by :score <)))
