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

(ns com.jhurt.nn.trainer.SimpleBlackjack
  #^{:author "Jason Lee Hurt"}
  (:require [com.jhurt.nn.BackPropagation :as BP])
  (:require [com.jhurt.CollectionsUtils :as CU])
  (:use [com.jhurt.nn.Common :only (getRandomWeightMatrices)])
  (:use [com.jhurt.Math :only (randomBounded)]))

(def currentSet (ref nil))

(defstruct Card :symbol :value)

(defn toBinaryString [x length]
  (loop [x (Integer/toString x 2)]
    (if (< (.length x) length) (recur (str "0" x)) x)))

(defn getEncodedHands [playerHand dealerCard]
    (loop [pStr (toBinaryString playerHand 5)
           dStr (toBinaryString dealerCard 4)
           encoded []]
      (cond
        (< (count encoded) 5)
          (recur (.substring pStr 1) dStr (conj encoded (Integer/parseInt (.substring pStr 0 1))))
        (< (count encoded) 9)
          (recur pStr (.substring dStr 1) (conj encoded (Integer/parseInt (.substring dStr 0 1))))
        :else encoded)))

(defn getHandValue [hand]
  ((reduce (fn [x y] (let [upper (+ (x :value) (y :value))]
    (cond
      (and (> upper 21) (= :A (x :symbol))) (struct Card (y :symbol) (inc (y :value)))
      (and (> upper 21) (= :A (y :symbol))) (struct Card (x :symbol) (inc (x :value)))
      :else (struct Card (x :symbol) upper)))) hand) :value))

(defn newDeck [] (flatten [(repeat 4 (struct Card :A 11))
                           (repeat 4 (struct Card :K 10))
                           (repeat 4 (struct Card :Q 10))
                           (repeat 4 (struct Card :J 10))
                           (repeat 4 (struct Card :ten 10))
                           (repeat 4 (struct Card :nine 9))
                           (repeat 4 (struct Card :eight 8))
                           (repeat 4 (struct Card :seven 7))
                           (repeat 4 (struct Card :six 6))
                           (repeat 4 (struct Card :five 5))
                           (repeat 4 (struct Card :four 4))
                           (repeat 4 (struct Card :three 3))
                           (repeat 4 (struct Card :two 2))]))

(defn popCard
  "pop a random card from the deck.
   return the resultant deck and the popped card"
  [deck]
  (let [i (randomBounded 0 (count deck))]
    {:deck (CU/removeFrom deck i) :card (nth deck i)}))

(defn makeSet []
  (loop [deck (vec (newDeck))
         s []
         playerHand []
         dealerHand []]
    (cond (empty? playerHand) (let [s1 (popCard deck)
                                    s2 (popCard (s1 :deck))
                                    s3 (popCard (s2 :deck))
                                    s4 (popCard (s3 :deck))
                                    p1 (conj playerHand (s1 :card))
                                    d1 (conj dealerHand (s2 :card))
                                    p2 (conj p1 (s3 :card))
                                    d2 (conj d1 (s4 :card))]
      (if (or (>= (getHandValue p2) 21) (>= (getHandValue d2) 21)) (makeSet)
        (recur (s4 :deck) (conj s {:playerHand p2 :dealerHand d2 :deck (s4 :deck)}) p2 d2)))
      :else
      (let [s1 (popCard deck)
            p1 (conj playerHand (s1 :card))]
        (if (>= (getHandValue p1) 21) s
          (recur (s1 :deck) (conj s {:playerHand p1 :dealerHand dealerHand :deck (s1 :deck)}) p1 dealerHand))))))

(defn hitOutcome
  "return -1 for losing, 0 for tying, 1 for winning"
  [state]
  (loop [s (popCard (state :deck))
         p (state :playerHand)
         d (state :dealerHand)]
    (cond
      (< (getHandValue p) 17) (recur (popCard (s :deck)) (conj p (s :card)) d)
      (< (getHandValue d) 17) (recur (popCard (s :deck)) p (conj d (s :card)))
      :else
      (let [x (getHandValue p) y (getHandValue d)]
        (cond
          (and (> x y) (>= 21 x)) 1
          (and (> y x) (>= 21 y)) -1
          :else 0)))))

(defn stayOutcome
  "return -1 for losing, 0 for tying, 1 for winning"
  [state]
  (loop [s (popCard (state :deck))
         d (state :dealerHand)]
    (cond
      (< (getHandValue d) 17) (recur (popCard (s :deck)) (conj d (s :card)))
      :else
      (let [p (state :playerHand) x (getHandValue p) y (getHandValue d)]
        (cond
          (and (> x y) (>= 21 x)) 1
          (and (> y x) (>= 21 y)) -1
          :else 0)))))

(defn getOutcome
  "return -1 if staying caused a win, 1 if hitting caused a win, else 0"
  [state]
  (let [h (hitOutcome state) s (stayOutcome state)]
    (cond
      (and (not= h s) (> h s)) 1
      (and (not= h s) (> s h)) -1
      :else 0)))

(defn getTrainingDatum
  "called by the back-propagation algorithm to get a training input/output pair"
  []
  (if (empty? @currentSet) (dosync (ref-set currentSet (makeSet))))
  (assert (not (empty? @currentSet)))
  (let [state (first @currentSet)
        input (getEncodedHands (getHandValue (state :playerHand)) ((first (rest (state :dealerHand))) :value))
        output [(getOutcome state)]]
    (dosync (ref-set currentSet (rest @currentSet)))
    {:input input :output output}))

(defn train
  "train a blackjack player NN"
  [layers numCycles generation alpha gamma callback]
  (let [weights (getRandomWeightMatrices layers 9 1)
        result (BP/trainNetwork numCycles layers getTrainingDatum weights alpha gamma)]
    (callback (result :weights) (result :rms-error) generation layers alpha gamma)))

(defn playDealerRules
  "play by dealer rules, staying at 17 or greater.
   return a 0 for a tie, -1 for a loss, 1 for a win"
  []
  (let [initialDeck (vec (newDeck))
        s1 (popCard initialDeck)
        s2 (popCard (s1 :deck))
        s3 (popCard (s2 :deck))
        s4 (popCard (s3 :deck))
        p1 (conj [] (s1 :card))
        d1 (conj [] (s2 :card))
        p2 (conj p1 (s3 :card))
        d2 (conj d1 (s4 :card))]
    (loop [state s4 playerHand p2 dealerHand d2]
      (let [deck (state :deck) pVal (getHandValue playerHand) dVal (getHandValue dealerHand) s (popCard deck)]
        (cond
          (< pVal 17) (recur s (conj playerHand (s :card)) dealerHand)
          (< dVal 17) (recur s playerHand (conj dealerHand (s :card)))
          (and (> pVal 21) (> dVal 21)) 0
          (> pVal 21) -1
          (> dVal 21) 1
          (= pVal dVal) 0
          (> pVal dVal) 1
          :else -1)))))

(defn- shouldHit?
  "return true if the NN determines the player should hit, else false"
  [playerHand dealerHand layers weights]
  (let [input (getEncodedHands (getHandValue playerHand) ((first (rest dealerHand)) :value))
        output (BP/calculateOutput layers input weights)]
    ;(println "input " input ", output " output)
    (if (> (first output) 0) true false)))

(defn playWithTrainedPlayer
  "play with a trained player NN. return a 0 for a tie, -1 for a loss, 1 for a win"
  [layers weights]
  (let [initialDeck (vec (newDeck))
        s1 (popCard initialDeck)
        s2 (popCard (s1 :deck))
        s3 (popCard (s2 :deck))
        s4 (popCard (s3 :deck))
        p1 (conj [] (s1 :card))
        d1 (conj [] (s2 :card))
        p2 (conj p1 (s3 :card))
        d2 (conj d1 (s4 :card))]
    (loop [state s4 playerHand p2 dealerHand d2]
      (let [deck (state :deck) pVal (getHandValue playerHand) dVal (getHandValue dealerHand) s (popCard deck)]
        (cond
          (and (> pVal 21) (> dVal 21)) 0
          (> dVal 21) 1
          (> pVal 21) -1
          (and (< pVal 21) (shouldHit? playerHand dealerHand layers weights))
            (recur s (conj playerHand (s :card)) dealerHand)
          (< dVal 17)
            (recur s playerHand (conj dealerHand (s :card)))
          (= pVal dVal) 0
          (> pVal dVal) 1
          :else -1)))))

(defn getBlackjackResults [nn iterations]
    (loop [results []
           i 0]
      (if (< i iterations)
        (recur (conj results (playWithTrainedPlayer (nn :layers) (nn :weights))) (inc i))
        results)))

(defn testBlackjackDealer [iterations]
    (loop [results []
           i 0]
      (if (< i iterations)
        (recur (conj results (playDealerRules)) (inc i))
        results)))
