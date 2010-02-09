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

(ns com.jhurt.ga.GA
  (:use [com.jhurt.Math :only (randomPositive)]))

(defn randomNetworkLayers [maxLayers maxNodesPerLayer activationFn derivFn]
  (let [totalLayers (randomPositive maxLayers)]
    (loop [numLayers totalLayers
           layers (list)]
      (if (== 0 numLayers)
        layers
        (recur (dec numLayers)
          (conj layers {:number-of-nodes (randomPositive maxNodesPerLayer)
                        :activation-fn activationFn
                        :derivative-fn derivFn}))))))

(defn sortTrainResults [trainResults]
  (sort-by :error < trainResults))

(defn getBestResults [num trainResults]
  (take num (sortTrainResults trainResults)))

(defn mutate [layersNN1]
  (map
    (fn [ithLayerNN1]
      (let [activationFn (ithLayerNN1 :activation-fn)
            derivFn (ithLayerNN1 :derivative-fn)]
        {:number-of-nodes (+ (ithLayerNN1 :number-of-nodes) (randomPositive 20))
         :activation-fn activationFn :derivative-fn derivFn}))
    layersNN1))

(defn crossover [layersNN1 layersNN2]
  (map
    (fn [ithLayerNN1 ithLayerNN2]
      (let [activationFn (if (> 0.5 (rand 1)) (ithLayerNN1 :activation-fn) (ithLayerNN2 :activation-fn))
            derivFn (if (> 0.5 (rand 1)) (ithLayerNN1 :derivative-fn) (ithLayerNN2 :derivative-fn))]
        {:number-of-nodes (int (* 0.5 (+ (ithLayerNN1 :number-of-nodes) (ithLayerNN2 :number-of-nodes))))
         :activation-fn activationFn :derivative-fn derivFn}))
    layersNN1 layersNN2))

(defn breed [trainResults newPopulationSize]
  (let [parents (getBestResults (int (* 0.5 newPopulationSize)) trainResults)]
    (println "parents size: " (count parents))
    (loop [p parents children ()]
      (if (or (not (seq p)) (< (count p) 4))
        children
        (recur (rest (rest (rest (rest p))))
          (conj children (crossover ((nth p 0) :layers) ((nth p 1) :layers))
            (crossover ((nth p 0) :layers) ((nth p 2) :layers))
            (crossover ((nth p 0) :layers) ((nth p 3) :layers))
            (crossover ((nth p 1) :layers) ((nth p 2) :layers))
            (crossover ((nth p 1) :layers) ((nth p 3) :layers))
            (crossover ((nth p 2) :layers) ((nth p 3) :layers))
            (mutate ((nth p 0) :layers))
            (mutate ((nth p 1) :layers))))))))

;(def structure1 {:inputs (take numberOfTrainingDatum (cycle (keys XOR-table)))
;                 :outputs (take numberOfTrainingDatum (cycle (vals XOR-table)))
;                 :layers
;                 (list {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative}
;                   {:number-of-nodes 5 :activation-fn Afns/hyperbolicTangent :derivative-fn Afns/hyperbolicTangentDerivative})})

