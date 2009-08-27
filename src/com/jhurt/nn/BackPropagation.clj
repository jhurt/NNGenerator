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

;; an implementation of a back propogation algorithm for NN's with one hidden layer as
;; described in R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996, pp 167-169

(ns com.jhurt.nn.BackPropagation)
(use 'com.jhurt.Math)
(use 'com.jhurt.nn.ActivationFunctions)
(use 'com.jhurt.nn.Input)

;learning constant defines step length of correction
(def gamma 0.3)

(def weights1 (ref nil))
(def weights2 (ref nil))

(defn trainWeights [inputs outputs extLayerWeights1 extLayerWeights2]
  (loop [inputs inputs
         outputs outputs
         extLayerWeights1 extLayerWeights1
         extLayerWeights2 extLayerWeights2]
    (if (and (seq inputs) (seq outputs))
      (let [input (first inputs)
            extendedInput (concat input [1.0])
            output (first outputs)

            ;feed-forward step
            layerOutput1 (map hyperbolicTangent (vectorByMatrix extendedInput extLayerWeights1))
            extLayerOutput1 (concat layerOutput1 [1.0])
            layerDerivative1 (map hyperbolicTangentDerivative layerOutput1)
            layerOutput2 (map hyperbolicTangent (vectorByMatrix extLayerOutput1 extLayerWeights2))
            layerDerivative2 (map hyperbolicTangentDerivative layerOutput2)

            ;backpropagation to output layer step
            layerBackPropagatedError2
            (map * layerDerivative2 (arrayLessAnother layerOutput2 output))

            ;backpropagation to hidden layer step
            layerBackPropagatedError1
            (map * layerDerivative1 (matrixByVector extLayerWeights2 layerBackPropagatedError2))]

        ;update weights and recurse step
        (recur (rest inputs) (rest outputs)
          (matrixSubtract extLayerWeights1
            (matrixMultiplyScalar (makeMatrix extendedInput layerBackPropagatedError1) gamma))
          (matrixSubtract extLayerWeights2
            (matrixMultiplyScalar (makeMatrix extLayerOutput1 layerBackPropagatedError2) gamma))))
      (dosync (ref-set weights1 extLayerWeights1) (ref-set weights2 extLayerWeights2)))))

(defn trainXOR [numCycles]
  ;TODO replace take with repeatable
  (let [inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        layerOneWeights (getRandomWeightVectors 2 3)
        layerTwoWeights (getRandomWeightVectors 1 3)]
    (trainWeights inputs outputs layerOneWeights layerTwoWeights)))

(defn classifyInput [input]
  (let [extendedInput (concat input (list 1))
        hiddenLayerOutput (map logistic (vectorByMatrix extendedInput @weights1))]
    (map logistic (vectorByMatrix hiddenLayerOutput @weights2))))    
