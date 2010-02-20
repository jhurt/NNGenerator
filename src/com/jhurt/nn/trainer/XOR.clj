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
  (:require [com.jhurt.nn.BackPropagation :as BP])
  (:use [com.jhurt.nn.ActivationFunctions])
  (:use [com.jhurt.nn.Common]))

(def XOR-table {[-1 -1] [-1]
                [-1 1] [1]
                [1 -1] [1]
                [1 1] [-1]})

(defn trainStructure [structure generation callback]
  (let [weights (getRandomWeightMatrices (structure :layers) 2 1)
        result (BP/train structure weights)]
    (callback (result :weights) (result :rms-error) generation (structure :layers))))

(defn train [layers numberOfDatum generation callback]
  (let [structure {:inputs (take numberOfDatum (cycle (keys XOR-table)))
                   :outputs (take numberOfDatum (cycle (vals XOR-table)))
                   :layers layers}]
    (trainStructure structure generation callback)))

(def layer1 (list {:number-of-nodes 3 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 1 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}))

(def layer3 (list {:number-of-nodes 6 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 1 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}))


(def layer4 (list
  {:number-of-nodes 6 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
  {:number-of-nodes 6 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
  {:number-of-nodes 1 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}))

(def layer2 (list {:number-of-nodes 3 :activation-fn logistic :derivative-fn logisticDerivative}
          {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative}))


(defn testXOR1 [numCycles]
  (let [layers layer1
        inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        weights (vector (getRandomWeightVectors 3 3) (getRandomWeightVectors 1 4))]
    (BP/train {:layers layers :inputs inputs :outputs outputs} weights)))

(defn testXOR3 [numCycles]
  (let [layers layer3
        inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        weights (vector (getRandomWeightVectors 6 3) (getRandomWeightVectors 1 7))]
    (BP/train {:layers layers :inputs inputs :outputs outputs} weights)))

(defn testXOR2 [numCycles]
  (let [layers layer2
        inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        weights (vector (getRandomWeightVectors 3 3) (getRandomWeightVectors 1 4))]
    (BP/train {:layers layers :inputs inputs :outputs outputs} weights)))

(defn testXOR4 [numCycles]
  (let [layers layer4
        inputs (take numCycles (cycle (keys XOR-table)))
        outputs (take numCycles (cycle (vals XOR-table)))
        weights (vector (getRandomWeightVectors 6 3) (getRandomWeightVectors 6 7) (getRandomWeightVectors 1 7))]
    (BP/train {:layers layers :inputs inputs :outputs outputs} weights)))

(defn classifyInput [layers input weights]
  (BP/calculateOutput layers input weights))