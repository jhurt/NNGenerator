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

;; an implementation of the perceptron convergence algorithm as described in:
;; S. Haykin: Neural Networks: A Comprehensive Foundation, Prentice Hall, 1999, pp 142-143

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.nn.PerceptronHaykin)
(use 'com.jhurt.Math)
(use 'com.jhurt.nn.ActivationFunctions)
(use 'com.jhurt.nn.Input)

(defn buildInputs [numberOfInputs]
  (loop [inputVector []
         binaryInputCollection infiniteInputCollection
         remainingCount numberOfInputs]
        (if (> 0 remainingCount)
            inputVector
            (recur
              (conj inputVector (first binaryInputCollection)) (rest binaryInputCollection) (dec remainingCount)))))

(defn buildOutputs [numberOfOutputs outputCollection]
  (loop [outputVector []
         andOutputCollection outputCollection
         remainingCount numberOfOutputs]
        (if (> 0 remainingCount)
            outputVector
            (recur (conj outputVector (first andOutputCollection)) (rest andOutputCollection) (dec remainingCount)))))

;; Main

;learning rate parameter, eta 0 < eta <= 1
(def learningRateParameter 1.0)

;the weight vector of the perceptron
;(def weightVector (ref nil))

;multiply the transpose of the weight vector with the input vector
;apply the signum function to the scalar result
(defn computeActualResponse [signumFunction weights inputs]
  (if (and (not (nil? weights)) (not (nil? inputs)))
      ;;TODO create a function that will apply first to a collection until the inner item is obtained
      ;use a doall to force evaluation to avoid overflowing the stack
      (signumFunction (first (first (doall (matrixMultiply (transposeMatrix weights) inputs)))))))

;return an updated weight vector of the perceptron
(defn getAdaptedWeightVector [weights inputs desiredResponse actualResponse]
  (let [etaDeltaDesiredActual (* learningRateParameter (- desiredResponse actualResponse))]
    (println "adapted weight vector" (matrixAdd weights (multiplyScalar inputs etaDeltaDesiredActual)))
    (println "desiredResponse" desiredResponse)
    (println "actualResponse" actualResponse)
    (println "learningRateParameter" learningRateParameter)
    (println "etaDeltaDesiredActual" etaDeltaDesiredActual)
    (println "weights" weights)
    (println "inputs" inputs "\n")
    (if (not (= 0.0 etaDeltaDesiredActual))
      (cons 1 (rest (matrixAdd weights (multiplyScalar inputs etaDeltaDesiredActual))))
      weights)))

;train the perceptron with the inputs and corresponding known outputs
(defn trainPerceptron [beginningWeightVector allInputs allOutputs]
  (loop [weightVector beginningWeightVector
         inputs allInputs
         responses allOutputs]
        (if (and (not (empty? inputs)) (not (empty? responses)))
            (let [adaptedWeightVector
                  (getAdaptedWeightVector
                    weightVector
                    (first inputs)
                    (first responses)
                    (computeActualResponse signum weightVector (first inputs)))]
                 (recur adaptedWeightVector (rest inputs) (rest responses)))
            weightVector)))

(defn main [sizeOfDataSet]
  (let [weights [[1.0 0.5 0.5]]
        inputs (buildInputs sizeOfDataSet)
        outputs (buildOutputs sizeOfDataSet infiniteAndOutputCollection)]
       (trainPerceptron weights inputs outputs)))

(defn classifyInput [input weights]
  (computeActualResponse signum weights input))
  