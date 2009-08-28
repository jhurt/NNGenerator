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

(def trainedWeights (ref nil))

(defn calculateOutput [activationFn input weights nodeOutputs]
  (loop [input input
         nodeOutputs nodeOutputs]
    (if (= (count weights) (count nodeOutputs))
      (butlast (last nodeOutputs))
      (let [index (count nodeOutputs)
            nodeOutput (map activationFn (vectorByMatrix input (nth weights index)))
            extNodeOutput (concat nodeOutput [1.0])]
        (recur extNodeOutput (concat nodeOutputs (list extNodeOutput)))))))

(defn calculateNodeValues [activationFn activationFnDerivative input weights nodeOutputs nodeDerivatives]
  (loop [input input
         nodeOutputs nodeOutputs
         nodeDerivatives nodeDerivatives]
    (if (= (count weights) (count nodeOutputs))
      {:nodeOutputs nodeOutputs :nodeDerivatives nodeDerivatives}
      (let [index (count nodeOutputs)
            nodeOutput (map activationFn (vectorByMatrix input (nth weights index)))
            extNodeOutput (concat nodeOutput [1.0])
            nodeDerivative (map activationFnDerivative extNodeOutput)]
        (recur extNodeOutput
          (concat nodeOutputs (list extNodeOutput))
          (concat nodeDerivatives (list nodeDerivative)))))))

(defn calculateNodeErrors [nodeValues weights actualOutput errors]
  (loop [errors errors]
    (if (= (count errors) (count weights))
      errors
      (let [index (- (count weights) (inc (count errors)))
            weightsIndex (inc index)
            difference (if (= 0 (count errors))
          (arrayLessAnother (nth (:nodeOutputs nodeValues) index) actualOutput)
          (matrixByVector (nth weights weightsIndex) (last errors)))
            error (map * (nth (:nodeDerivatives nodeValues) index) difference)]
        (recur (concat errors (list error)))))))

(defn getWeightDeltas [extendedInput errors nodeOutputs deltas]
  (loop [errorIndex (dec (count nodeOutputs))
         nodeValueIndex -1
         deltas deltas]
    (if (= (count nodeOutputs) (count deltas))
      deltas
      (let [delta (if (= 0 (count deltas))
        (matrixMultiplyScalar (makeMatrix extendedInput (nth errors errorIndex)) gamma)
        (matrixMultiplyScalar
          (makeMatrix (nth nodeOutputs nodeValueIndex) (nth errors errorIndex)) gamma))]
        (recur (dec errorIndex) (inc nodeValueIndex) (concat deltas (list delta)))))))

(defn trainWeights [inputs outputs weights]
  (loop [inputs inputs
         outputs outputs
         weights weights]
    (if (and (seq inputs) (seq outputs))
      (let [input (first inputs)
            extendedInput (concat input [1.0])
            output (first outputs)
            ;feed-forward step
            nodeValues
            (calculateNodeValues hyperbolicTangent hyperbolicTangentDerivative extendedInput weights (list) (list))
            ;backpropagation step
            errors (calculateNodeErrors nodeValues weights output (list))
            ;calculate weight deltas
            deltas (getWeightDeltas extendedInput errors (:nodeOutputs nodeValues) (list))]
        ;update weights and recurse step
        (recur (rest inputs) (rest outputs) (map matrixSubtract weights deltas)))
      (dosync (ref-set trainedWeights weights)))))

(defn trainXOR [numCycles]
  ;TODO replace take with repeatable
  (let [inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        weights (list (getRandomWeightVectors 2 3) (getRandomWeightVectors 1 3))]
    (trainWeights inputs outputs weights)))

(defn classifyInput [input]
  (calculateOutput hyperbolicTangent (concat input [1.0]) @trainedWeights (list)))
