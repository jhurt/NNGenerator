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

(ns com.jhurt.nn.BackPropagationTest
  (:gen-class
    :extends junit.framework.TestCase
    :state state
    :methods [[testCalculateOutputOneHiddenLayer [] void] [testCalculateNodeValuesOneHiddenLayer [] void]


              ])
  (:use [com.jhurt.nn.BackPropagation])
  (:use [com.jhurt.nn.ActivationFunctions]))

(import '(junit.framework TestCase Assert))

(defn getLayers [num]
  (loop [n num
         layers []]
    (if (= 0 n)
      layers
      (recur (dec n) (conj layers {:number-of-nodes 3 :activation-fn logistic :derivative-fn logisticDerivative})))))

(defn areListsEqual [x y]
  (reduce (fn [a b] (and a b)) (map = x y)))

(defn -testCalculateOutputOneHiddenLayer [_]
  (let [layers (conj (getLayers 1) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        input [-1 1]
        weights [[[0.5 0.6 0.7] [0.2 0.3 0.4] [0.3 0.4 0.5]]
                 [[0.4] [0.5] [0.6] [0.7]]]
        output (calculateOutput layers input weights)
        expectedOutput [(logistic (+ (* 0.4 (logistic (+ -0.5 0.2 0.3)))
                                    (* 0.5 (logistic (+ -0.6 0.3 0.4)))
                                    (* 0.6 (logistic (+ -0.7 0.4 0.5)))
                                    (* 0.7 1.0)))]]
    (Assert/assertEquals 1 (count output))
    (Assert/assertTrue (areListsEqual expectedOutput output))))

(defn -testCalculateNodeValuesOneHiddenLayer [_]
  (let [layers (conj (getLayers 1) {:number-of-nodes 1 :activation-fn logistic :derivative-fn logisticDerivative})
        input [-1 1]
        weights [[[0.5 0.6 0.7] [0.2 0.3 0.4] [0.3 0.4 0.5]]
                 [[0.4] [0.5] [0.6] [0.7]]]
        values ((calculateNodeValues layers input weights) :nodeOutputs)
        expectedHiddenOutput [(logistic (+ -0.5 0.2 0.3))
                              (logistic (+ -0.6 0.3 0.4))
                              (logistic (+ -0.7 0.4 0.5))]
        expectedOutput [(logistic (+ (* 0.4 (logistic (+ -0.5 0.2 0.3)))
                                    (* 0.5 (logistic (+ -0.6 0.3 0.4)))
                                    (* 0.6 (logistic (+ -0.7 0.4 0.5)))
                                    (* 0.7 1.0)))]]
    (Assert/assertEquals 2 (count values))
    (Assert/assertEquals 3 (count (first values)))
    (Assert/assertEquals 1 (count (last values)))
    (Assert/assertTrue (areListsEqual expectedHiddenOutput (first values)))
    (Assert/assertTrue (areListsEqual expectedOutput (last values)))))

