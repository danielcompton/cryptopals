(ns cryptopals.set1
  (:require [clojure.test :refer :all]
            [cryptopals.bytes :as b]
            [cryptopals.frequencies :as f]
            [clojure.java.io :as io]))

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
      (is (= (b/bytes->hex (b/xor (b/hex->bytes hex1)
                                  (b/hex->bytes hex2)))
             hex3)))))

(deftest single-byte-xor
  (testing "3"
    (is (= "Cooking MC's like a pound of bacon"
           (->> (f/crack-key
                  b/byte-0-255
                  (b/hex->bytes
                    "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
                (sort-by :score)
                (first)
                :str)))))

(deftest detect-single-char-xor
  (testing "4"
    (is (= "Now that the party is jumping\n"
           (with-open [test-file (io/reader (io/resource "4.txt"))]
             (->> (mapcat (partial f/crack-key b/byte-0-255)
                          (map b/hex->bytes (line-seq test-file)))
                  (sort-by :score)
                  (first)
                  (:str)))))))

(deftest repeating-key-xor
  (testing "5"
    (let [lyric "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
          xor-val (str "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
                       "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")]
      (is (= (b/bytes->hex (b/xor (.getBytes "ICE") (.getBytes lyric)))
             xor-val)))))

(deftest break-repeating-key-xor
  (testing "6"
    (let [ba (b/b64->bytes (apply str (line-seq (io/reader (io/resource "6.txt")))))
          probable-keysizes (take 3 (f/guess-keysize ba 2 40))]
      (for [keysize probable-keysizes
            partition (vec (b/deinterlace-array ba (:keysize keysize)))]
        (->> (f/crack-key b/byte-0-255 partition)
             (sort-by :score)
             (take 20))))))
