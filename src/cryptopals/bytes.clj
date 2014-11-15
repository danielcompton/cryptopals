(ns cryptopals.bytes
  (:import [org.apache.commons.codec.binary Base64 Hex]))

(defn hex->bytes [s]
  (Hex/decodeHex (char-array s)))

(defn bytes->hex [ba]
  (Hex/encodeHexString ba))

(defn b64->bytes [^String s]
  (Base64/decodeBase64 s))

(defn bytes->b64 [ba]
  (String. (Base64/encodeBase64 ba) "UTF-8"))

(defn xor
  "XOR's ba with a repeating key"
  [^bytes key ^bytes ba]
  (let [key-length (alength key)]
    (amap ba
          idx
          ret
          (byte (bit-xor (aget ^bytes ba idx) (aget ^bytes key (mod idx key-length)))))))

(defn sbyte->byte [b]
  (bit-and b 0xff))
