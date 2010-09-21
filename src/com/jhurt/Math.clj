;;Copyright (c) 2010, University of Nevada, Las Vegas
;;All rights reserved.
;;
;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;    * Neither the name of the University of Nevada, Las Vegas, nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.Math)

(defn logBase2 [x] (/ (Math/log x) (Math/log 2)))

(def randomNumbers (repeatedly rand))

(defn randomPositive [x]
  (int (Math/ceil (* x (rand 1)))))

(defn randomBounded
  "return a random integer greater than or equal to x and less than y"
  [x y]
  (let [z (+ (rand (- y x)) x)]
    (int (Math/floor z))))

;; Matrix Functions

(defn transposeMatrix2 [matrix]
  (if (not (nil? matrix))
    (apply map list matrix)))

(defn transposeMatrix [matrix]
  (apply map (fn [& column] column) matrix))

(defn matrixMultiply
  "Map a function to each row of matrixA that applies a map
  of the addition of row-column multiplications to each column of matrixB.
  The body of this function is lazy and will not be executed
  if the return value is not read anywhere"
  [matrixA matrixB]
  (map
    (fn [row] (apply map (fn [& column] (apply + (map * row column))) matrixB))
    matrixA))

(defn matrixAdd [matrixA matrixB]
  (if (and (not (empty? matrixA)) (not (empty? matrixB)))
    (conj
      (matrixAdd (rest matrixA) (rest matrixB))
      (map + (first matrixA) (first matrixB)))))

(defn matrixSubtract [matrixA matrixB]
  (if (and (seq matrixA) (seq matrixB))
    (conj
      (matrixSubtract (rest matrixA) (rest matrixB))
      (map - (first matrixA) (first matrixB)))))

;; Vector Functions

(defn transposeVector [v]
  (if (not (nil? v))
    (transposeMatrix (vector v))))

(defn transposeArray [array]
  (map (fn [& column] column) array))

(defn arrayTransposeByAnother [x y]
  (reduce + (map * (map first (transposeArray x)) y)))

(defn arrayLessAnother [x y]
  (map - x y))

(defn arrayPlusAnother [x y]
  (map + x y))

(defn normalizeVector
  "normalize a vector by converting it to a unit vector"
  [x]
  (let [length (Math/sqrt (reduce + (map * x x)))]
    (map / x (repeat (count x) length))))

(defn replace-nth
  "Returns a list with the n-th item of v replaced by x"
  [v n x]
  (concat (take n v) (list x) (drop (inc n) v)))

(defn replace-last
  "Returns a list with the last item of v replaced by x"
  [v x]
  (concat (take (dec (count v)) v) (list x) (drop (count v) v)))

;; Matrix & Vector Functions
(defn vectorByMatrix
  "multiply vector v by matrix m"
  [v m]
  (map (fn [row] (apply + (map * row v))) (transposeMatrix m)))

(defn vectorByMatrixSlow
  "multiply vector v by matrix m"
  [v m r]
  (cond
    (or (empty? m) (empty? (first m))) r
    :else (vectorByMatrix v (map (fn [x] (rest x)) m)
      (conj r (reduce + (map (fn [z y] (* z y)) v (map (fn [x] (first x)) m)))))))

(defn matrixByVector
  "multiply matrix m by vector v"
  [m v]
  (map (fn [row] (reduce + (map * row v))) m))

(defn makeMatrix
  "return a new matrix out of vectorA and vectorB whose (i,j)th element is the value
  of vectorA[i] * vectorB[j]"
  [vectorA vectorB]
  (map (fn [x] (map (fn [y] (* x y)) vectorB)) vectorA))

(defn- mScalar [x scalar result]
  (cond
    (empty? x) result
    (or (seq? (first x)) (vector? (first x)))
      (mScalar (rest x) scalar (conj result (mScalar (first x) scalar [])))
    :else (mScalar (rest x) scalar (conj result (* scalar (first x))))))

(defn multiplyScalar [x scalar] (mScalar x scalar []))

(defn weightsByInput [w i]
  (map (fn [x y] (reduce + (map * (repeat (count x) y) x))) (transposeMatrix w) i))
