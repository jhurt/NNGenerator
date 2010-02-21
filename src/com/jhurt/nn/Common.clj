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

;Common NN related functions
(ns com.jhurt.nn.Common
  (:use [com.jhurt.Math :only (randomPositive)])
  (:use [com.jhurt.nn.ActivationFunctions]))

(defn getRandomWeightVectors
  "build vector of arity 'number' of vectors of length 'size' populated with random values b/w 0 and 1"
  [size number]
  (take number (repeatedly (fn [] (take size (repeatedly rand))))))

(defn getRandomWeightMatrices [layers inputArity outputArity]
  (loop [i (inc inputArity)
         hiddenLayers layers
         weights []]
    (if (= (count weights) (inc (count layers)))
      weights
      (let
        [j (if-not (seq hiddenLayers)
          outputArity
          ((first hiddenLayers) :number-of-nodes))]
        ;increment j to account for the bias node at each hidden layer
        (recur (inc j)
          (rest hiddenLayers)
          (conj weights (getRandomWeightVectors j i)))))))

(defn randomNetworkLayers [maxLayers maxNodesPerLayer]
  (let [totalLayers (randomPositive maxLayers)
        activationFn (if (> 0.5 (rand 1)) logistic hyperbolicTangent)
        derivFn (fnToDerivMap activationFn)]
    (loop [numLayers totalLayers
           layers ()]
      (if (== 0 numLayers)
        layers
        (recur (dec numLayers)
          (conj layers {:number-of-nodes (randomPositive maxNodesPerLayer)
                        :activation-fn activationFn
                        :derivative-fn derivFn}))))))
