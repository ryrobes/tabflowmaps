(ns tabflow.util
  (:require [clojure.string :as cstr]))

(defn kvpaths
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (kvpaths kp v (conj res kp)))
                              (conj res (conj prev k))))
              result
              m)))

(defn unkeyword [x]
  (if (keyword? x)
    (-> (str x)
        (cstr/replace #":" ""))
    (str x)))

(defn deep-flatten [x]
  (if (coll? x)
    (mapcat deep-flatten x)
    [x]))

(defn keypaths
  ([m] (keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (keypaths (conj prev k) v res)
                              (conj res (conj prev k))))
              result
              m)))

(defn curved-path-h [x1 y1 x2 y2]
  (let [mx (+ x1 (/ (- x2 x1) 2))
        line ["M" x1 y1 "C" mx y1 mx y2 x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v [x1 y1 x2 y2]
  (let [;mx (+ x1 (/ (- x2 x1) 2))
        my (+ y1 (/ (- y2 y1) 2))
        line ["M" x1 y1 "C" x1 my x2 my x2 y2]]
    (cstr/join " " line)))

(defn stepped-path-h [x1 y1 x2 y2]
  (let [mx (+ x1 (/ (- x2 x1) 2))
        line ["M" x1 y1 "L" mx y1 "L" mx y2 "L" x2 y2]]
    (cstr/join " " line)))