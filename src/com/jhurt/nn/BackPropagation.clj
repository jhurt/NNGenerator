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
(def gamma 0.5)

(def weights1 (ref nil))
(def weights2 (ref nil))

(defn trainWeights [inputs outputs inputHiddenLayerWeights hiddenOutputLayerWeights]
  (loop [inputs inputs
         outputs outputs
         inputHiddenLayerWeights inputHiddenLayerWeights
         hiddenOutputLayerWeights hiddenOutputLayerWeights]
    (if (and (seq inputs) (seq outputs))
      (let [input (conj (first inputs) 1)
            output (first outputs)
            ihLayerWeights
            (replace-last inputHiddenLayerWeights (take (count (first inputHiddenLayerWeights)) (cycle [1.0])))
            hoLayerWeights
            (replace-last hiddenOutputLayerWeights (take (count (first hiddenOutputLayerWeights)) (cycle [1.0])))

            ;feed-forward step
            hiddenLayerOutput (conj (map logistic (vectorByMatrix input ihLayerWeights)) 1.0)
            hiddenLayerDerivative (map * hiddenLayerOutput (map (fn [x] (- 1.0 x)) hiddenLayerOutput))
            outputLayerOutput (map logistic (vectorByMatrix hiddenLayerOutput hoLayerWeights))
            outputLayerDerivative (map * outputLayerOutput (map (fn [x] (- 1.0 x)) outputLayerOutput))

            ;backpropagation to output layer step
            outputBackPropagatedError (map * outputLayerDerivative (arrayLessAnother outputLayerOutput output))

            ;backpropagation to hidden layer step
            hiddenBackPropagatedError (map * hiddenLayerDerivative
          (matrixByVector hoLayerWeights outputBackPropagatedError))]
        (println "**************************************************")
        (println "first layer weights: " ihLayerWeights)
        (println "second layer weights: " hoLayerWeights)
        (println "input: " input)
        (println "output: " output)
        (println "outputBackPropagatedError: " outputBackPropagatedError)
        (println " hiddenBackPropagatedError: " hiddenBackPropagatedError)
        (println "hiddenLayerOutput: " hiddenLayerOutput)
;        (println "hiddenLayerDerivative: " hiddenLayerDerivative)
        (println "outputLayerOutput: " outputLayerOutput ";")
;        (println "outputLayerDerivative: " outputLayerDerivative)

        ;update weights and recur step
        (recur (rest inputs) (rest outputs)
          (matrixAdd ihLayerWeights
            (matrixMultiplyScalar (makeMatrix input hiddenBackPropagatedError) (* -1.0 gamma)))
          (matrixAdd hoLayerWeights
            (matrixMultiplyScalar (makeMatrix hiddenLayerOutput outputBackPropagatedError) (* -1.0 gamma)))))
      (dosync (ref-set weights1 inputHiddenLayerWeights) (ref-set weights2 hiddenOutputLayerWeights)))))

(defn trainXOR [numCycles]
  (let [inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        layerOneWeights [[0.5 0.5] [0.5 0.5] [1.0 1.0]]
        layerTwoWeights [[0.5] [0.5] [1.0]]]
    (trainWeights inputs outputs layerOneWeights layerTwoWeights)))

(defn classifyInput [input]
  (let [extendedInput (conj input 1)
    hiddenLayerOutput (conj (map logistic (vectorByMatrix input @weights1)) 1)]
    (map logistic (vectorByMatrix hiddenLayerOutput @weights2))))    
