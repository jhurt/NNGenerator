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

(ns com.jhurt.ga.GATest
  (:gen-class
    :extends junit.framework.TestCase
    :state state
    :methods [[testCrossover1HasLeastLayers [] void] [testCrossover1PreservesNumberOfLastLayerNodes [] void]
              [testCrossover2HasMostLayers [] void] [testCrossover2HasMostLayers2 [] void]
              [testCrossover2PreservesNumberOfLastLayerNodes [] void]
              [testGetTotalError [] void]
              [testGetRouletteWheel [] void]
              ])
  (:use [com.jhurt.ga.GA])
  (:use [com.jhurt.nn.ActivationFunctions]))

(import '(junit.framework TestCase Assert))

(defn getLayers [num]
  (loop [n num
         layers []]
    (if (= 0 n)
      layers
      (recur (dec n) (conj layers {:number-of-nodes 5 :activation-fn logistic :derivative-fn logisticDerivative})))))

(defn -testCrossover1HasLeastLayers [_]
  (let [x (getLayers 10)
        y (getLayers 20)
        a (crossover1 x y)
        b (crossover1 x x)
        c (crossover1 y y)]
    (Assert/assertEquals 10 (count a))
    (Assert/assertEquals 10 (count b))
    (Assert/assertEquals 20 (count c))))

(defn -testCrossover1PreservesNumberOfLastLayerNodes [_]
  (let [x (conj (getLayers 10) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        y (conj (getLayers 100) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        a (crossover1 x y)
        b (crossover1 y x)]
    (Assert/assertEquals 1 ((last a) :number-of-nodes))
    (Assert/assertEquals 1 ((last b) :number-of-nodes))))

(defn -testCrossover2HasMostLayers [_]
  (let [x (getLayers 10)
        y (getLayers 15)
        a (crossover2 x y)]
    (Assert/assertEquals 15 (count a))))

(defn -testCrossover2HasMostLayers2 [_]
  (let [x (getLayers 10000)
        y (getLayers 100)
        a (crossover2 x y)]
    (Assert/assertEquals 10000 (count a))))

(defn -testCrossover2PreservesNumberOfLastLayerNodes [_]
  (let [x (conj (getLayers 10) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        y (conj (getLayers 100) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        a (crossover2 x y)
        b (crossover2 y x)]
    (Assert/assertEquals 1 ((last a) :number-of-nodes))
    (Assert/assertEquals 1 ((last b) :number-of-nodes))))

(defn -testGetTotalError [_]
  (let [parents (vector {:weights nil :error 5.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 6.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 17.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 35.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0})]
    (Assert/assertEquals 63.0 (getTotalError parents))))

(defn -testGetRouletteWheel [_]
  (let [parents (vector {:weights nil :error 5.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 6.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 17.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0}
    {:weights nil :error 35.0 :generation nil :layers nil :alpha 1.0 :gamma 2.0})
        wheel (getRouletteWheel parents)
        a (first wheel)
        b (nth wheel 1)
        c (nth wheel 2)
        d (nth wheel 3)]
    (Assert/assertEquals 0.0 (:lower a))
    (Assert/assertEquals 0.4404688463911166 (:upper a))
    (Assert/assertEquals 0.4404688463911166 (:lower b))
    (Assert/assertEquals 0.8075262183837137 (:upper b))
    (Assert/assertEquals 0.8075262183837137 (:lower c))
    (Assert/assertEquals 0.9370758790869833 (:upper c))
    (Assert/assertEquals 0.9370758790869833 (:lower d))
    (Assert/assertEquals 1.0 (:upper d))))

