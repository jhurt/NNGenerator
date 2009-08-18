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

;; an implementation of the Oja's algorithm for finding the first principal component
;; of empirical data
;; as described in R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996, pp 116

(ns com.jhurt.nn.FirstPrincipalComponent)

(use 'com.jhurt.Math)

(def inputs (ref []))

;return a set of n-dimensional input vectors
(defn getInputs [])

(defn getRandomWeightVector [size])

(defn findFirstPrincipalComponent []
  (loop [inputs (getInputs)
         weights (getRandomWeightVector (count (first inputs)))
         gamma 0.005]
    (let [currentInputs (first inputs)
          phi (arrayTransposeByAnother currentInputs weights)
          newWeights
          (arrayPlusAnother (multiplyScalar
            (arrayLessAnother (first inputs) (multiplyScalar weights phi))
            (* phi gamma)))]
          (recur (rest inputs) newWeights (* 0.99 gamma)))))