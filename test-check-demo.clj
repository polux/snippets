; Copyright 2017 Google Inc. All Rights Reserved.
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns clfeat.core)

(require '[clojure.test.check :as tc])
(require '[clojure.test.check.generators :as gen])
(require '[clojure.test.check.properties :as prop])

(defn counting-sort [xs]
  (let [len (+ (apply max xs) 1)
        counts (int-array len)]
    (doseq [i xs]
      (aset-int counts i (+ 1 (aget counts i))))
    (let [res (transient [])]
      (doseq [i (range len)]
        (if (> (aget counts i) 0)
          (conj! res i)))
      (persistent! res))))

(counting-sort [42])
(counting-sort [2 5 1])

(defn sort-correctness [my-sort]
  (prop/for-all [v (gen/vector gen/int)]
                (= (my-sort v) (sort v))))

(defn check-sort [my-sort]
  (tc/quick-check 100 (sort-correctness my-sort)))

(check-sort counting-sort)

(defn counting-sort-2 [xs]
  (if (empty? xs)
    xs
    (let [len (+ (apply max xs) 1)
          counts (int-array len)]
      (doseq [i xs]
        (aset-int counts i (+ 1 (aget counts i))))
      (let [res (transient [])]
        (doseq [i (range len)]
          (if (> (aget counts i) 0)
            (conj! res i)))
        (persistent! res)))))

(check-sort counting-sort-2)

(defn counting-sort-3 [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (doseq [i (range len)]
          (if (> (aget counts i) 0)
            (conj! res (+ i minimum))))
        (persistent! res)))))

(check-sort counting-sort-3)

(defn counting-sort-4 [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (doseq [i (range len)]
          (doseq [_ (range (aget counts i))]
            (conj! res (+ i minimum))))
        (persistent! res)))))

(check-sort counting-sort-4)
