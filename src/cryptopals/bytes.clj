(ns cryptopals.bytes
  (:import [org.apache.commons.codec.binary Base64 Hex]))

(defn hex->bytes [s]
  (Hex/decodeHex (char-array s)))

(defn bytes->hex [ba]
  (Hex/encodeHexString ba))

(defn b64->bytes [s]
  (Base64/decodeBase64 s))

(defn bytes->b64 [ba]
  (String. (Base64/encodeBase64 ba) "UTF-8"))

(defn sbyte->byte [b]
  (bit-and b 0xff))
