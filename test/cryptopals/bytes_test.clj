(ns cryptopals.bytes-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [cryptopals.bytes :refer :all])
  (:import [java.util Arrays]))

(def bigger-ints (gen/sized (fn [size] (gen/resize (* size size) gen/pos-int))))

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


