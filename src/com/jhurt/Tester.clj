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
  com.jhurt.Tester
  (:gen-class)
  (:require [com.jhurt.nn.PerceptronRojas :as PR])
  (:require [com.jhurt.nn.Clusterer :as Clusterer])
  (:require [com.jhurt.nn.Input :as Input])
  (:require [com.jhurt.nn.Common :as Common])
  (:require [com.jhurt.nn.BackPropagation :as BP])
  (:require [com.jhurt.Graph :as Graph])
  (:use [com.jhurt.ThreadUtils])
  (:use [com.jhurt.nn.trainer.SimpleBlackjack :only (getBlackjackResults testBlackjackDealer)])
  (:use [com.jhurt.nn.trainer.OCR :only (getOcrResults getTrainingDatum)])
  (:use [com.jhurt.Serialization]))

(import
  '(javax.swing JFrame JPanel JButton JFileChooser GrayFilter)
  '(javax.swing.filechooser FileFilter)
  '(java.awt.event ActionListener)
  '(java.awt.image BufferedImage ImageObserver FilteredImageSource)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(java.awt Color Graphics BorderLayout))

(def blackjackIterations 50000)

(def fileFilter (proxy [FileFilter] []
  (accept [f]
    (and (not (nil? f))
      (or (.isDirectory f) (.. f (getName) (endsWith "nn")))))
  (getDescription [] "Neural Network Files")))

(defn testBlackjack [nnFile]
  (let [nn (deserializeFile nnFile)
        iterations blackjackIterations
        results (getBlackjackResults nn iterations)
        wins (filter (fn [x] (= x 1)) results)
        ties (filter (fn [x] (= x 0)) results)
        losses (filter (fn [x] (= x -1)) results)]
    (println "wins: " (count wins) ", ties: " (count ties) ", losses: " (count losses))))

(def testBlackjackBtn (doto (new JButton "Test Blackjack")
  (.addActionListener
    (let [fileChooser (doto (new JFileChooser)
      (.setFileFilter fileFilter))]
      (proxy [ActionListener] []
        (actionPerformed [e]
          (if (= (JFileChooser/APPROVE_OPTION) (. fileChooser (showOpenDialog testBlackjackBtn)))
            (testBlackjack (.getSelectedFile fileChooser)))))))))

(def testBlackjackDealerBtn (doto (new JButton "Test Blackjack Dealer")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (let [results (testBlackjackDealer blackjackIterations)
              wins (filter (fn [x] (= x 1)) results)
              ties (filter (fn [x] (= x 0)) results)
              losses (filter (fn [x] (= x -1)) results)]
          (println "wins: " (count wins) ", ties: " (count ties) ", losses: " (count losses))))))))

(defn testOcr [nnFile]
  (let [nn (deserializeFile nnFile)
        iterations 10000
        results (getOcrResults (nn :layers) (nn :weights) iterations)
        correct (filter (fn [x] (= x 1)) results)
        incorrect (filter (fn [x] (= x 0)) results)]
    (println "correct: " (count correct) ", incorrect: " (count incorrect))))

(def testOcrBtn (doto (new JButton "Test OCR")
  (.addActionListener
    (let [fileChooser (doto (new JFileChooser)
      (.setFileFilter fileFilter))]
      (proxy [ActionListener] []
        (actionPerformed [e]
          (if (= (JFileChooser/APPROVE_OPTION) (. fileChooser (showOpenDialog testOcrBtn)))
            (testOcr (.getSelectedFile fileChooser)))))))))

(defn launchGraphWindow [canvas]
  (let [frame (new JFrame)
        panel
        (doto (new JPanel)
          (.setLayout (new BorderLayout))
          (.setBackground Color/WHITE)
          (.add canvas BorderLayout/CENTER))]
    (doto frame
      (.add panel)
      (.setTitle "Resultant Neural Network")
      (.pack)
      (.setSize 800 600)
      (.setVisible true))))

(def trainOcrBtn (doto (new JButton "Train OCR")
  (.addActionListener (proxy [ActionListener] []
    (actionPerformed [e]
      (let [layers (Common/randomNetworkLayers 2 5 4)
            weights (Common/getRandomWeightMatrices layers 16 4)
            cycles 5
            alpha 0.7
            gamma -0.5
            nn (BP/trainNetwork cycles layers getTrainingDatum weights alpha gamma)
            iterations 1
            results (getOcrResults layers (nn :weights) iterations)
            correct (filter (fn [x] (= x 1)) results)
            incorrect (filter (fn [x] (= x 0)) results)]
        (println "correct: " (count correct) ", incorrect: " (count incorrect))
        (launchGraphWindow (Graph/getNewCanvas (nn :weights) layers 16 4))))))))

(def buttonPanel (doto (new JPanel)
  (.add testBlackjackBtn)
  (.add testBlackjackDealerBtn)
  (.add testOcrBtn)
  (.add trainOcrBtn)))

(defn -main [& args]
  (let [frame (new JFrame "Neural Network Tester")]
    (doto frame
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
      (.add buttonPanel)
      (.setSize 300 200)
      (.setVisible true))))
