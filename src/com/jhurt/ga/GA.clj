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
  com.jhurt.ga.GA
  (:use [com.jhurt.Math :only (randomBounded)])
  (:require [com.jhurt.nn.Common :as Common])
  (:require [com.jhurt.nn.ActivationFunctions :as Afns])
  (:require [com.jhurt.CollectionsUtils :as CU]))

(defn sortTrainResults [trainResults]
  (sort-by :error < trainResults))

(defn getHealthiestChild [trainResults]
  (first (sortTrainResults trainResults)))

(defn getBestResults [num trainResults]
  (take num (sortTrainResults trainResults)))

;(defn mutate [layersNN1]
;  (map
;    (fn [ithLayerNN1]
;      (let [activationFn (ithLayerNN1 :activation-fn)
;            derivFn (ithLayerNN1 :derivative-fn)]
;        {:number-of-nodes (+ (ithLayerNN1 :number-of-nodes) (randomBounded -5 5))
;         :activation-fn activationFn :derivative-fn derivFn}))
;    layersNN1))

(defn getActivationFns [layer1 layer2]
  (if (> 0.5 (rand 1)) {:activation-fn (layer1 :activation-fn)
                        :derivative-fn (layer1 :derivative-fn)}
    {:activation-fn (layer2 :activation-fn)
     :derivative-fn (layer2 :derivative-fn)}))

(defn chooseConstant [c1 c2]
  (let [x (rand 1)]
    (cond
      (< x 0.3333333) c1
      (< x 0.6666667) c2
      :else (* 0.5 (+ c1 c2)))))

(defn chooseConstantBounded [c1 c2 upper]
  (let [x (rand 1)
        y (cond
            (< x 0.3333333) c1
            (< x 0.6666667) c2
            :else (* 0.5 (+ c1 c2)))]
    (if (> y upper) upper y)))

(defn crossoverBounded [c1 c2 lower upper]
  (let [sign (if (> c1 c2) 1.0 -1.0)
        c (+ (* sign (rand 0.2)) (chooseConstant c1 c2))]
    (cond
      (> c upper) upper
      (< c lower) lower
      :else c)))

(defn crossoverAlpha [c1 c2] (crossoverBounded c1 c2 0.1 1.0))
(defn crossoverGamma [c1 c2] (crossoverBounded c1 c2 -1.0 -0.1))

(defn crossover1
  "perform a crossover on 2 NN structures, in this version the # of layers will be
  the least of layersNN1 and layersNN2"
  [layersNN1 layersNN2 maxNodesPerLayer]
  (let [newLayers
        (vec (map
          (fn [ithLayerNN1 ithLayerNN2]
            (let [activationFns (getActivationFns ithLayerNN1 ithLayerNN2)]
              {:number-of-nodes (int (chooseConstantBounded (ithLayerNN1 :number-of-nodes) (ithLayerNN2 :number-of-nodes) maxNodesPerLayer))
               :activation-fn (activationFns :activation-fn)
               :derivative-fn (activationFns :derivative-fn)}))
          (butlast layersNN1) (butlast layersNN2)))
        activationFns (getActivationFns (last layersNN1) (last layersNN2))
        lastLayer {:number-of-nodes ((last layersNN1) :number-of-nodes)
                   :activation-fn (activationFns :activation-fn)
                   :derivative-fn (activationFns :derivative-fn)}]
    (conj newLayers lastLayer)))

(defn crossover2
  "perform a crossover on 2 NN structures, in this version the # of layers will be
  the most of layersNN1 and layersNN2"
  [layersNN1 layersNN2 maxNodesPerLayer]
  (let [largestLayers (if (> (count layersNN1) (count layersNN2)) layersNN1 layersNN2)
        difference (Math/abs (- (count layersNN1) (count layersNN2)))
        numToRemove (- (count largestLayers) difference)
        firstLayers
        (vec (map
          (fn [ithLayerNN1 ithLayerNN2]
            (let [activationFns (getActivationFns ithLayerNN1 ithLayerNN2)]
              {:number-of-nodes (int (chooseConstantBounded (ithLayerNN1 :number-of-nodes) (ithLayerNN2 :number-of-nodes) maxNodesPerLayer))
               :activation-fn (activationFns :activation-fn)
               :derivative-fn (activationFns :derivative-fn)}))
          layersNN1 layersNN2))
        middleLayers (butlast (CU/removeFirstN largestLayers numToRemove))
        activationFns (getActivationFns (last layersNN1) (last layersNN2))
        lastLayer {:number-of-nodes ((last layersNN1) :number-of-nodes)
                   :activation-fn (activationFns :activation-fn)
                   :derivative-fn (activationFns :derivative-fn)}]
    (conj (vec (concat firstLayers middleLayers)) lastLayer)))

(defn crossoverLayers [layersNN1 layersNN2 maxNodesPerLayer]
  (if (or (= (count layersNN1) (count layersNN2)) (> 0.5 (rand 1)))
    (crossover1 layersNN1 layersNN2 maxNodesPerLayer)
    (crossover2 layersNN1 layersNN2 maxNodesPerLayer)))

(defn getRandomChild
  "generate a random child"
  [maxLayers maxNodesPerLayer outputArity]
  {:layers (Common/randomNetworkLayers maxLayers maxNodesPerLayer outputArity)
   :alpha (Common/randomAlpha)
   :gamma (Common/randomGamma)})

(defn getChild
  "get a new child based on 2 parents"
  [p1 p2 maxNodesPerLayer]
  (let [layers (crossoverLayers (:layers p1) (:layers p2) maxNodesPerLayer)
        alpha (crossoverAlpha (:alpha p1) (:alpha p2))
        gamma (crossoverGamma (:gamma p1) (:gamma p2))]
    {:layers layers :alpha alpha :gamma gamma}))

(defn getTotalError
  "get the total error of a set of training results"
  [trainResults]
  (loop [results trainResults
         total 0.0]
    (if (empty? results) total
      (recur (rest results) (+ total (:error (first results)))))))

(defn- getInverseErrorProportions [trainResults]
  (let [totalError (getTotalError trainResults)]
    (loop [results trainResults
           proportions []]
      (if (empty? results) proportions
        (let [error (:error (first results))
              x (/ 1.0 (/ error totalError))]
          (recur (rest results) (conj proportions x)))))))

(defn getRouletteWheel
  "build a roulette wheel based on training results. The probability
  of selecting a training result is proportional to the fitness of the result
  as compared to all other results"
  [trainResults]
  (let [proportions (getInverseErrorProportions trainResults)
        total (reduce + proportions)]
    (loop [results trainResults
           proportions proportions
           current 0.0
           wheel []]
      (if (empty? results) wheel
        (let [proportion (first proportions)
              probability (/ proportion total)
              upper (+ current probability)
              entry {:lower current :upper upper :result (first results)}]
          (recur (rest results) (rest proportions) upper (conj wheel entry)))))))

(defn selectParent
  "use a weighted roulette wheel to select parents for the next generation.
  The fittest training results have the highest probability of selection"
  [rouletteWheel]
  (let [r (rand 1)
        selection (filter (fn [x] (and (>= r (:lower x)) (<= r (:upper x)))) rouletteWheel)]
    (assert (= 1 (count selection)))
    (:result (first selection))))

(defn breed [trainResults newPopulationSize maxLayers maxNodesPerLayer outputArity]
  (let [parents (getBestResults (int (* 0.5 newPopulationSize)) trainResults)
        rouletteWheel (getRouletteWheel parents)]
    (loop [children [] r (rand 1)]
      (cond (= newPopulationSize (count children)) children
        ;reproduction
        (< r 0.25) (recur (conj children (selectParent rouletteWheel)) (rand 1))
        ;random selection
        (< r 0.29) (recur (conj children (getRandomChild maxLayers maxNodesPerLayer outputArity)) (rand 1))
        ;crossover
        :else (recur (conj children (getChild (selectParent rouletteWheel) (selectParent rouletteWheel) maxNodesPerLayer)) (rand 1))))))
