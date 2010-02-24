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

(ns com.jhurt.nn.BackPropagation
  (:use [com.jhurt.Math])
  (:use [com.jhurt.nn.Input])
  (:use [com.jhurt.nn.Common]))

;learning constant defines step length of correction
(def gamma 0.3)

(defn calculateOutput
  "Get the output of the network for a single input"
  [layers input weights]
  (loop [i input
         w weights
         l layers
         nodeOutputs []]
    (if-not (seq w)
      (last nodeOutputs)
      (let [activationFn ((first l) :activation-fn)
            extI (concat i [1.0])
            nodeOutput (map activationFn (vectorByMatrix extI (first w)))]
        (recur nodeOutput
          (rest w) (rest l) (concat nodeOutputs (vector nodeOutput)))))))

(defn calculateNodeValues
  "Get the output of the activation function and the corresponding
  derivative of the activation function for each node in the network"
  [layers input weights]
  (loop [i input
         w weights
         l layers
         nodeOutputs []
         nodeDerivatives []]
    (if-not (and (seq l) (seq w))
      {:nodeOutputs nodeOutputs :nodeDerivatives nodeDerivatives}
      (let [activationFn ((first l) :activation-fn)
            activationFnDerivative ((first l) :derivative-fn)
            extI (concat i [1.0])
            nodeOutput (map activationFn (vectorByMatrix extI (first w)))
            nodeDerivative (map activationFnDerivative nodeOutput)]
        (recur
          nodeOutput
          (rest w)
          (rest l)
          (concat nodeOutputs (vector nodeOutput))
          (concat nodeDerivatives (vector nodeDerivative)))))))

(defn calculateNodeErrors
  "Get the backpropagated error for each node, the results are stored in
  reverse order of the network, the first vector is the backprogated error
  for the output layer"
  [nodeValues weights actualOutput]
  (loop [o (reverse (nodeValues :nodeOutputs))
         d (reverse (nodeValues :nodeDerivatives))
         w (reverse weights)
         errors []]
    (if-not (and (seq o) (seq w))
      errors
      (let [v (first o)
            dv (first d)
            difference
            (if (= 0 (count errors))
              (arrayLessAnother v actualOutput)
              (matrixByVector (first w) (last errors)))
            error (map * dv difference)]
        (if (= 0 (count errors))
          (recur (rest o) (rest d) w (concat errors (vector error)))
          (recur (rest o) (rest d) (rest w) (concat errors (vector error))))))))

(defn calculateRmsError
  "Get the root mean squared error for the output layer of the network"
  [outputLayerError]
  (reduce + (map (fn [x] (* 0.5 (* x x))) outputLayerError)))

(defn getWeightDeltas
  "Get the weight deltas for each layer based on
  the given backpropogated errors"
  [input nodeOutputs errors]
  (loop [l (concat (vector input) nodeOutputs)
         e (reverse errors)
         deltas []]
    (if-not (and (seq l) (seq e))
      deltas
      (let [i (conj (first l) 1.0)
            delta (multiplyScalar (makeMatrix i (first e)) gamma)]
        (recur (rest l) (rest e) (conj deltas delta))))))

(defn getAverageRmsError [inputToErrorMap]
  (/ (reduce + (vals inputToErrorMap)) (count inputToErrorMap)))

(defn trainNetwork
  "Train the network with the given input/output map and initial weight set
   for the desired # of cycles. Input/output pairs are selected randomly at
   each iteration"
  [cycles layers ioMap weights inputToErrorMap]
  (loop [n cycles
         weights weights
         inputToErrorMap inputToErrorMap]
    (if (> n 0)
      (let [input (nth (keys ioMap) (randomBounded -1 (dec (count ioMap))))
            output (ioMap input)
            ;feed-forward step
            nodeValues (calculateNodeValues layers input weights)
            ;backpropagation step
            errors (calculateNodeErrors nodeValues weights output)
            ;calculate weight deltas
            deltas (getWeightDeltas input (nodeValues :nodeOutputs) errors)
            ;get rms error
            rmsError (calculateRmsError (first errors))]
        (do (println "input: " input)
        ;update weights and recurse step
        (recur (dec n) (map matrixSubtract weights deltas) (assoc inputToErrorMap input rmsError))))
      {:rms-error (getAverageRmsError inputToErrorMap) :weights weights})))

(defn train [cycles layers ioMap weights]
  (let [inputToErrorMap (zipmap (keys ioMap) (take (count ioMap) (cycle [1.0])))]
    (trainNetwork cycles layers ioMap weights inputToErrorMap)))
