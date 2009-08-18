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

(ns com.jhurt.nn.Input
  (:require [com.jhurt.Math]))

;; Binary Logic Input/Output

(def infiniteInputCollection (cycle [[[1.0 1.0 1.0]] [[1.0 -1.0 1.0]] [[1.0 1.0 -1.0]] [[1.0 -1.0 -1.0]]]))
(def infiniteAndOutputCollection (cycle [1.0 -1.0 -1.0 -1.0]))

(def AND-table {[0 0 1] 0
                [0 1 1] 0
                [1 0 1] 0
                [1 1 1] 1})

(def XOR-table {[0 0] [0]
                [0 1] [1]
                [1 0] [1]
                [1 1] [0]})


(defn getRandomInput [size]
  (map (fn [x] (* 50.0 x)) (take size (repeatedly rand))))

(defn getRandomInputVectors[x y upperBound]
  "build vector of size y of vectors of length x populated with random values b/w 0 and upperBound"
  (take y (repeatedly (fn [] (take x (repeatedly (fn [] (* upperBound (rand)))))))))

(defn getRandomWeightVectors[x y]
  "build vector of size y of vectors of length x populated with random values b/w 0 and 1"
  (take y (repeatedly (fn [] (take x (repeatedly rand))))))