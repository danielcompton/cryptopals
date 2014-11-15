(ns cryptopals.set1
  (:require [clojure.test :refer :all]
            [cryptopals.bytes :as b]))

(deftest hex-to-base64
  (testing "1"
   (let [hex-str "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
         b64-str "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]
     (is (= b64-str (b/bytes->b64 (b/hex->bytes hex-str)))))))

(deftest fixed-xor
  (testing "2"
    (let [hex1 "1c0111001f010100061a024b53535009181c"
          hex2 "686974207468652062756c6c277320657965"
          hex3 "746865206b696420646f6e277420706c6179"]
      ())))
