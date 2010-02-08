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

(ns com.jhurt.nn.trainer.XOR
  (:require [com.jhurt.nn.ActivationFunctions :as Afns])
  (:require [com.jhurt.nn.BackPropagationDynamic :as BP])
  (:use [com.jhurt.nn.Common]))

(def XOR-table {[-1 -1] [-1]
                [-1 1] [1]
                [1 -1] [1]
                [1 1] [-1]})

(defn trainStructure [structure]
  (let [inputs (structure :inputs)
        outputs (structure :outputs)
        layers (structure :layers)
        inputArity (count (first inputs))
        weights (getRandomWeightMatrices layers inputArity)]
    (BP/trainNetwork inputs outputs layers weights)
    (println "RMS Error: " @BP/finalError)))

(defn train [layers numberOfDatum]
  (let [structure {:inputs (take numberOfDatum (cycle (keys XOR-table)))
                   :outputs (take numberOfDatum (cycle (vals XOR-table)))
                   :layers layers}]
    (trainStructure [structure])))

(defn getResult [] { :weights @BP/trainedWeights :error @BP/finalError })

; a NN with 2 hidden layers, each layer with 5 nodes, and each hidden layer node uses
; a hyperbolic tangent activation function to calculate output values
;(def structure1 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
;                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
;                 :layers
;                 (list {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative}
;                   {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative})})

; a NN with 1 hidden layer with 10 nodes, and each hidden layer node uses
; a hyperbolic tangent activation function to calculate output values
;(def structure2 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
;                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
;                 :layers
;                 (list {:number-of-nodes 10 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative})})

; a NN with 2 hidden layers, each layer with 5 nodes, and each hidden layer node uses
; a logistic activation function to calculate output values
;(def structure3 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
;                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
;                 :layers
;                 (list {:number-of-nodes 5 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative}
;                   {:number-of-nodes 5 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative})})

; a NN with 1 hidden layer with 10 nodes, and each hidden layer node uses
; a logistic activation function to calculate output values
;(def structure4 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
;                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
;                 :layers
;                 (list {:number-of-nodes 10 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative})})
