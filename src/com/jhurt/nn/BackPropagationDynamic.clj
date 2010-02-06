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

;; an implementation of a back propogation algorithm for NN's with support for variable hidden layers,
;; multidimensional input and output training sets, variable activation functions
;; Loosely based on back propagation algorithm described in
;; R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996, pp 167-171

(ns com.jhurt.nn.BackPropagationDynamic
  (:require [com.jhurt.nn.ActivationFunctions :as Afns]))

(use 'com.jhurt.Math)
(use 'com.jhurt.nn.Input)

;learning constant defines step length of correction
(def gamma 0.3)

;stores the result of the weights after the network has been trained
(def trainedWeights (ref nil))

;stores the final error values of the network after it has been trained
(def finalError (ref nil))

(defn calculateOutput [layers input weights]
  "Get the output of the network for a single input"
  (let [nodeOutputs (list)]
    (loop [input input
           nodeOutputs nodeOutputs]
      (if (= (count weights) (count nodeOutputs))
        (butlast (last nodeOutputs))
        (let [index (count nodeOutputs)
              activationFn ((nth layers index) :activation-fn)
              nodeOutput (map activationFn (vectorByMatrix input (nth weights index)))
              extNodeOutput (concat nodeOutput [1.0])]
          (recur extNodeOutput (concat nodeOutputs (list extNodeOutput))))))))

(defn calculateNodeValues [layers input weights]
  "Get the output of the activation function and the corresponding
  derivative of the activation function for each node in the network"
  (let [nodeOutputs (list) nodeDerivatives (list)]
    (loop [input input
           nodeOutputs nodeOutputs
           nodeDerivatives nodeDerivatives]
      (if (= (count weights) (count nodeOutputs))
        {:nodeOutputs nodeOutputs :nodeDerivatives nodeDerivatives}
        (let [index (count nodeOutputs)
              activationFn ((nth layers index) :activation-fn)
              activationFnDerivative ((nth layers index) :derivative-fn)
              nodeOutput (map activationFn (vectorByMatrix input (nth weights index)))
              extNodeOutput (concat nodeOutput [1.0])
              nodeDerivative (map activationFnDerivative extNodeOutput)]
          (recur extNodeOutput
            (concat nodeOutputs (list extNodeOutput))
            (concat nodeDerivatives (list nodeDerivative))))))))

(defn calculateNodeErrors [nodeValues weights actualOutput]
  "Get the backpropagated error for each node, the results are stored in
  reverse order of the network, the first vector is the backprogated error
  for the output layer"
  (let [errors (list)]
    (loop [errors errors]
      (if (= (count errors) (count weights))
        errors
        (let [index (- (count weights) (inc (count errors)))
              weightsIndex (inc index)
              difference (if (= 0 (count errors))
            (arrayLessAnother (nth (:nodeOutputs nodeValues) index) actualOutput)
            (matrixByVector (nth weights weightsIndex) (last errors)))
              error (map * (nth (:nodeDerivatives nodeValues) index) difference)]
          (recur (concat errors (list error))))))))

(defn calculateRmsError [outputLayerError]
  "Get the root mean squared error for the output layer of the network"
  (reduce + (map (fn [x] (* 0.5 (* x x))) outputLayerError)))

(defn getWeightDeltas [extendedInput errors nodeOutputs]
  "Get the weight deltas for each layer based on
  the given backpropogated errors"
  (let [deltas (list)]
    (loop [errorIndex (dec (count nodeOutputs))
           nodeValueIndex -1
           deltas deltas]
      (if (= (count nodeOutputs) (count deltas))
        deltas
        (let [delta (if (= 0 (count deltas))
          (multiplyScalar (makeMatrix extendedInput (nth errors errorIndex)) gamma)
          (multiplyScalar
            (makeMatrix (nth nodeOutputs nodeValueIndex) (nth errors errorIndex)) gamma))]
          (recur (dec errorIndex) (inc nodeValueIndex) (concat deltas (list delta))))))))

(defn getRandomWeightMatrices [layers inputArity]
  (println "building weight matrices")
  (loop [x inputArity
         hiddenLayers layers
         weights (vector)]
    (if-not (seq hiddenLayers)
      weights
      (let [y ((first hiddenLayers) :number-of-nodes)]
        (recur y (rest hiddenLayers) (conj weights (getRandomWeightVectors x y)))))))

(defn trainNetwork [inputs outputs layers weights]
  "Train the network with the given inputs/outputs and initial weight set.
   Training terminates when there are no more training samples"
  (println "training network")
  (loop [inputs inputs
         outputs outputs
         weights weights
         rmsError 1.0]
    (if (and (seq inputs) (seq outputs))
      ;do one step of training
      (let [input (first inputs)
            extendedInput (concat input [1.0])
            output (first outputs)
            ;feed-forward
            nodeValues (calculateNodeValues layers extendedInput weights)
            ;backpropagation
            errors (calculateNodeErrors nodeValues weights output)
            ;calculate weight deltas
            deltas (getWeightDeltas extendedInput errors (:nodeOutputs nodeValues))]
        ;update weights and recurse step
        (recur (rest inputs) (rest outputs)
          (map matrixSubtract weights deltas) (calculateRmsError (first errors))))
      ;completed training, save the weights and the rms error
      (dosync (ref-set trainedWeights weights) (ref-set finalError rmsError)))))

(defn trainXOR [structure]
  (let [inputs (structure :inputs)
        outputs (structure :outputs)
        layers (structure :layers)
        inputArity (count (first inputs))
        weights (getRandomWeightMatrices layers inputArity)]
    (trainNetwork inputs outputs layers weights)
    (println "RMS Error: " @finalError)))

(def numberOfTrainingDatum 10)

; a NN with 2 hidden layers, each layer with 5 nodes, and each hidden layer node uses
; a hyperbolic tangent activation function to calculate output values
(def structure1 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
                 :layers
                 (list {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative}
                   {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative})})

; a NN with 1 hidden layer with 10 nodes, and each hidden layer node uses
; a hyperbolic tangent activation function to calculate output values
(def structure2 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
                 :layers
                 (list {:number-of-nodes 10 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative})})

; a NN with 2 hidden layers, each layer with 5 nodes, and each hidden layer node uses
; a logistic activation function to calculate output values
(def structure3 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
                 :layers
                 (list {:number-of-nodes 5 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative}
                   {:number-of-nodes 5 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative})})

; a NN with 1 hidden layer with 10 nodes, and each hidden layer node uses
; a logistic activation function to calculate output values
(def structure4 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
                 :layers
                 (list {:number-of-nodes 10 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative})})

(defn classifyInput [input layers]
  (calculateOutput layers (concat input [1.0]) @trainedWeights))
