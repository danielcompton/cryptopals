(ns cryptopals.bytes-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [cryptopals.bytes :refer :all]
            [cryptopals.util :refer [interleave-all]])
  (:import [java.util Arrays]))

(def bigger-ints (gen/sized (fn [size] (gen/resize (* size size) gen/s-pos-int))))

(defspec xor-roundtrip
  10000
  (prop/for-all [key (gen/such-that
                       #(not (zero? (alength ^bytes %))) gen/bytes)
                 ^bytes plaintext gen/bytes]
    (Arrays/equals (xor key (xor key plaintext))
                   plaintext)))

(defspec byte-encoding-roundtrip
  10000
  (prop/for-all [^bytes ba gen/bytes]
    (Arrays/equals ba ^bytes (hex->bytes (bytes->hex ba)))
    (Arrays/equals ba ^bytes (b64->bytes (bytes->b64 ba)))))

(deftest deinterlace-array-test
  (is (= (apply interleave (partition 3 (range 0 9)))
         (apply concat (seq (deinterlace-array (byte-array (range 0 9)) 3))))))

(defspec deinterlace-array-check
  100
  (prop/for-all [params (gen/bind gen/s-pos-int
                          (fn [partitions] (gen/tuple (gen/such-that #(< partitions %) (gen/choose 1 128))
                                                      (gen/return partitions))))]
    (let [[size partitions] params
          values (range size)]
      (is (= (apply interleave-all (partition-all partitions values))
             (apply concat (seq (deinterlace-array (byte-array values) partitions))))))))
