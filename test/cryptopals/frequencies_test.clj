(ns cryptopals.frequencies-test
  (:refer-clojure :exclude [frequencies])
  (:require [cryptopals.frequencies :refer :all]
            [clojure.test :refer :all]))

(deftest hamming-distance-test
  (is (= 37 (hamming-distance-str "this is a test" "wokka wokka!!!"))))
