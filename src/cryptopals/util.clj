(ns cryptopals.util)

(defn fmap [f m]
  "Taken from algo.generic, applies f to value in map m"
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn merge-common-with
  "From http://stackoverflow.com/questions/18808048/clojure-merge-with-remove-keys-that-are-not-common"
  [f m1 m2]
  (let [[a b] (if (< (count m1) (count m2))
                [m1 m2]
                [m2 m1])]
    (persistent!
      (reduce-kv (fn [out k v]
                   (if (contains? b k)
                     (assoc! out k (f (get a k) (get b k)))
                     out))
                 (transient {})
                 a))))

(defn interleave-all
  "Analogy: partition:partition-all :: interleave:interleave-all.
  From prismatic/plumbing"
  [& colls]
  (lazy-seq
    ((fn helper [seqs]
       (when (seq seqs)
         (concat (map first seqs)
                 (lazy-seq (helper (keep next seqs))))))
      (keep seq colls))))
