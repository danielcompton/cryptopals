(ns cryptopals.bytes
  (:import [org.apache.commons.codec.binary Base64 Hex]
           (java.nio ByteBuffer)))

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
  ^bytes [^bytes key ^bytes ba]
  (let [key-length (alength key)]
    (assert (not (zero? key-length)) "Key length must not be zero")
    (amap ba
          idx
          ret
          (byte (bit-xor (aget ^bytes ba idx) (aget ^bytes key (mod idx key-length)))))))

(defn sbyte->byte [b]
  "Convert signed byte to unsigned byte"
  (bit-and b 0xff))

(defn byte->sbyte [b]
  "Convert unsigned byte to signed byte"
  (unchecked-byte b))

(def byte-0-255 (map #(byte-array 1 (byte->sbyte %)) (range 0 255)))


