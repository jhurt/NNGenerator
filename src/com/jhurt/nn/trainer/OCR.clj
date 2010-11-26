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
  com.jhurt.nn.trainer.OCR
  (:require [com.jhurt.nn.BackPropagation :as BP])
  (:require [com.jhurt.image.MNIST :as MNIST])
  (:use [com.jhurt.nn.Common :only (getRandomWeightMatrices)]))

(def data (ref nil))

(defn getTrainingDatum
  "called by the back-propagation algorithm to get a training input/output pair"
  []
  (if (empty? @data) (dosync (ref-set data (MNIST/loadTrainingPairs))))
  (let [pair (first @data)]
    (dosync (ref-set data (rest @data)))
    pair))

;; this version uses a closure rather than a global ref, but is slower
;(let [data (MNIST/loadTrainingPairs)
;      i (ref 0)]
;  (defn getTrainingDatum
;    "called by the back-propagation algorithm to get a training input/output pair"
;    []
;    (let [pair (nth data @i)]
;      (dosync (ref-set i (inc @i)))
;      pair)))

(defn train
  "train an OCR digit recognition NN"
  [layers numCycles generation alpha gamma callback]
  (let [weights (getRandomWeightMatrices layers 16 4)
        result (BP/trainNetwork numCycles layers getTrainingDatum weights alpha gamma)]
    (callback (result :weights) (result :rms-error) generation layers alpha gamma)))

(defn- fixOutput [output]
  (loop [output output results []]
    (cond
      (= (count output) 0) results
      (< (first output) 0.5) (recur (rest output) (conj results 0))
      :else (recur (rest output) (conj results 1)))))

(defn- listsEqual? [a b]
  (reduce (fn [x y] (and x y)) (map (fn [x y] (= x y)) a b)))

(defn getOcrResults [layers weights iterations]
  (dosync (ref-set data (MNIST/loadTestPairs)))
  (loop [i 0 pair (first @data) results []]
      (dosync (ref-set data (rest @data)))
      (cond
        (or (nil? pair) (>= i iterations))
          results
        :else
          (let [output (fixOutput (BP/calculateOutput layers (pair :input) weights))
                actualOutput (pair :output)
                result (if (listsEqual? output actualOutput) 1 0)]
          (println "actual: " actualOutput ". output: " output)  
          (recur (inc i) (first @data) (conj results result))))))


