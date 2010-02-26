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

;; an implementation of the perceptron convergence algorithm as
;; described in R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996, pp 85

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.nn.PerceptronRojas)
(use 'com.jhurt.Math)
(use 'com.jhurt.nn.Input)

;the weights of the vector, this will be set upon completion of the training of the perceptron
(def perceptronWeights (ref []))

(defn getWeights [numberOfWeights]
  (repeat numberOfWeights 0.5))

(defn correctAndWeights [initialWeights initialInputs initialNumberCorrect]
  (loop [weights initialWeights
         inputs initialInputs
         numberCorrect initialNumberCorrect]
    (println "current weights: " weights " current input: " (first inputs))
    ;terminating condition: all of the inputs have been correctly classified
    (if (= (count AND-table) numberCorrect)
      weights
      (if (and (<= 0 (arrayTransposeByAnother weights (first inputs))) (= 1 (AND-table (first inputs))))
          (recur (arrayLessAnother weights (first inputs)) (rest inputs) 0)
          (if (and (>= 0 (arrayTransposeByAnother weights (first inputs))) (= 0 (AND-table (first inputs))))
              (recur (arrayPlusAnother weights (first inputs)) (rest inputs) 0)
              (recur weights (rest inputs) (inc numberCorrect)))))))

(defn trainAndWeights []
  (dosync
    (ref-set perceptronWeights (correctAndWeights (getWeights (count (first (keys AND-table)))) (cycle (keys AND-table)) 0))))

(defn classifyInput [input]
  (if (> 0 (arrayTransposeByAnother @perceptronWeights input)) 1 0))
