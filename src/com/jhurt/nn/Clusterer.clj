;;Copyright (c) 2009, University of Nevada, Las Vegas
;;All rights reserved.
;;
;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;    * Neither the name of the University of Nevada, Las Vegas, nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; an implementation of a competetive learning clustering algorithm as
;; described in R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996, pp 104-105

(ns com.jhurt.nn.Clusterer)
(use 'com.jhurt.Math)

;update the closest weight vector m by:
;delta m = neta(x - m), so that the correction is proportional to the difference of both vectors
(defn getUpdatedWeights [weights input]
  (let [neta 0.8
        deltas (multiplyScalar neta (map - input weights))]
    (normalizeVector (map + weights (deltas)))))

;return a weight vector that lies closest to the input vector
(defn getClosestWeightVector [input weights startWeightVector highestValue]
  (loop [input input
         weights weights
         closestWeightVector startWeightVector
         highestValue highestValue]
    (if (empty? weights) closestWeightVector
      (let [currentValue (arrayTransposeByAnother (first weights) input)]
        (if (> highestValue currentValue)
          (recur input (rest weights) (first weights) currentValue)
          (recur input (rest weights) closestWeightVector highestValue))))))

;train the weight vector
;assumes the inputs vectors are already normalized
(defn trainWeights [inputs weights]
  (loop [weights weights
         inputs inputs]
    (println "current weights: " weights)
    (if (empty? inputs)
      weights
      (let [weightsToReplace (getClosestWeightVector (first inputs) (rest weights) (first weights)
        (arrayTransposeByAnother (first weights) (first inputs)))
            newWeights (getUpdatedWeights weightsToReplace (first inputs))]
        (recur (replace {weightsToReplace newWeights} weights) (rest inputs))))))

