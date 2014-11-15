(ns cryptopals.set1
  (:require [clojure.test :refer :all]
            [cryptopals.bytes :as b]))

(deftest hex-to-base64
  (testing "1"
   (let [hex-str "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
         b64-str "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]
     (is (= b64-str (b/bytes->b64 (b/hex->bytes hex-str)))))))

