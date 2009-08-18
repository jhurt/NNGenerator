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

(defn trainWeights [inputs outputs layerWeights1 layerWeights2]
  (loop [inputs inputs
         outputs outputs
         layerWeights1 layerWeights1
         layerWeights2 layerWeights2]
    (if (and (seq inputs) (seq outputs))
      (let [input (first inputs)
            extendedInput (concat input [1])
            output (first outputs)

            ;the extended weight matrices
            extLayerWeights1
            (concat layerWeights1 (vector (take (count (first layerWeights1)) (cycle [1.0]))))
            extLayerWeights2
            (concat layerWeights2 (vector (take (count (first layerWeights2)) (cycle [1.0]))))

            ;feed-forward step
            layerOutput1 (map logistic (vectorByMatrix extendedInput extLayerWeights1))
            layerDerivative1 (map (fn [x] (* x (- 1.0 x))) layerOutput1)
            layerOutput2 (map logistic (vectorByMatrix layerOutput1 extLayerWeights2))
            layerDerivative2 (map (fn [x] (* x (- 1.0 x))) layerOutput2)

            ;backpropagation to output layer step
            layerBackPropagatedError2
              (map * layerDerivative2 (arrayLessAnother layerOutput2 output))

            ;backpropagation to hidden layer step
            layerBackPropagatedError1
              (map * layerDerivative1 (matrixByVector layerWeights2 layerBackPropagatedError2))]

        (println "\n**************************************************")
        (println "input: " input)
        (println "output: " output)
        (println "-------------------------")
        (println "first layer weights: " extLayerWeights1)
        (println "hiddenLayerOutput: " layerOutput1)
        (println "hiddenLayerDerivative: " layerDerivative1)
        (println "hiddenBackPropagatedError: " layerBackPropagatedError1)
        (println "-------------------------")
        (println "second layer weights: " extLayerWeights2)
        (println "outputLayerOutput: " layerOutput2)
        (println "outputLayerDerivative: " layerDerivative2)
        (println "outputBackPropagatedError: " layerBackPropagatedError2)

        ;update weights and recur step
        (recur (rest inputs) (rest outputs)
          (matrixSubtract layerWeights1
            (matrixMultiplyScalar (makeMatrix input layerBackPropagatedError1) gamma))
          (matrixSubtract layerWeights2
            (matrixMultiplyScalar (makeMatrix layerOutput1 layerBackPropagatedError2) gamma))))
      (dosync (ref-set weights1 layerWeights1) (ref-set weights2 layerWeights2)))))

(defn trainXOR [numCycles]
  ;TODO replace take with repeatable
  (let [inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        layerOneWeights [[0.1 0.9] [0.3 0.7]]
        layerTwoWeights [[0.2] [0.9]]]
    (trainWeights inputs outputs layerOneWeights layerTwoWeights)))

(defn classifyInput [input]
  (let [extendedInput (concat input (list 1))
    hiddenLayerOutput (concat (map logistic (vectorByMatrix input @weights1)) (list 1))]
    (map logistic (vectorByMatrix hiddenLayerOutput @weights2))))    