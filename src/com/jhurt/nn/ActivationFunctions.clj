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

(ns com.jhurt.nn.ActivationFunctions)

;; Neuron Activation Functions

;threshold
(defn threshold [x] (if (>= x 0.0) 1.0 0.0))

;logistic (sigmoidal)
(defn logistic [x] (/ 1.0 (+ 1.0 (Math/pow Math/E (* -1.0 x)))))  

;signum (threshold)
(defn signum [x] (cond (> x 0.0) 1.0 (= x 0.0) 0.0 (< x 0.0) -1.0))
;(defn signum [x] (cond (> x 0.0) 1.0 (<= x 0.0) -1.0))

;piecewise linear
(defn piecewise [x] (cond (>= x 0.5) 1.0 (and (>  x -0.5) (< x 0.5)) x (<= x -0.5) 0.0))

;logistic (sigmoidal)
(defn sigmoid [x slopeParameter] (/ 1.0 (+ 1.0 (Math/exp (* -1.0 (* x slopeParameter))))))

;hyberbolic tangent (sigmoidal)
(defn hyperbolicTangent [x] (Math/tanh x))

;arctangent (sigmoidal)
(defn arcTangent [x] (Math/atan x))

;gompertz curve (sigmoidal)
; a is the upper asymptote
; c is the growth rate
; b, c are negative numbers
(defn gompertzCurve [x a b c] (* a (Math/pow Math/E (* b (Math/pow Math/E (* c x))))))

;algebraic sigmoid
(defn algebraicSigmoid [x] (/ x (Math/sqrt (+ 1.0 (Math/pow x 2.0)))))



