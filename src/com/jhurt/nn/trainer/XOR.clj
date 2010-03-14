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

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.nn.trainer.XOR
  (:require [com.jhurt.nn.BackPropagation :as BP])
  (:use [com.jhurt.nn.ActivationFunctions])
  (:use [com.jhurt.nn.Common])
  (:use [com.jhurt.CollectionsUtils]))

(def XOR-table {[-1 -1] [-1]
                [-1 1] [1]
                [1 -1] [1]
                [1 1] [-1]})

(defn train
  "train a NN with data from an XOR truth table"
  [layers numCycles generation alpha gamma callback]
  (let [weights (getRandomWeightMatrices layers 2 1)
        result (BP/train numCycles layers XOR-table weights alpha gamma)]
    (callback (result :weights) (result :rms-error) generation layers alpha gamma)))

(def layer1 (vector
          {:number-of-nodes 50 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 10 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 20 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 50 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 50 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 29 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 1  :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}))

(def layer2 (vector
          {:number-of-nodes 5 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}
          {:number-of-nodes 1 :activation-fn hyperbolicTangent :derivative-fn hyperbolicTangentDerivative}))

(defn testXOR1 [numCycles]
  (let [layers layer1
        weights (getRandomWeightMatrices layers 2 1)]
    (BP/train numCycles layers XOR-table weights 0.1 -0.9)))

(defn testXOR2 [numCycles]
  (let [layers layer2
        weights (getRandomWeightMatrices layers 2 1)]
    (BP/train numCycles layers XOR-table weights 0.005 -0.3)))

(defn classifyInput [layers input weights]
  (BP/calculateOutput layers input weights))
