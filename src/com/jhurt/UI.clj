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

(ns com.jhurt.UI
  (:require [com.jhurt.nn.PerceptronRojas :as PR])
  (:require [com.jhurt.nn.Clusterer :as Clusterer])
  (:require [com.jhurt.nn.Input :as Input])
  (:use [com.jhurt.SwingUtils :only (doOnEdt)])
  (:use [com.jhurt.Plot3D :as Plot3D]))

(import
  '(javax.swing JFrame JPanel JButton JFileChooser GrayFilter)
  '(javax.swing.filechooser FileFilter)
  '(java.awt.event ActionListener)
  '(java.awt.image BufferedImage ImageObserver FilteredImageSource)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(java.awt Graphics))

(defn doInNewThread [action]
  "Launch a new thread to do the specified action"
  (.start (Thread. action)))

(defn convertInputsToPoints [inputs]
  (let [input (first inputs)
        point (struct Plot3D/Point3D (double (nth input 0)) (double (nth input 1)) (double (nth input 2)))]
    (if (= 1 (count inputs))
      (list point)
      (cons point (convertInputsToPoints (rest inputs))))))

(defn convertWeightVectorsToLines [weightVectors]
  (let [weights (first weightVectors)
        line (struct Plot3D/Line (* 100.0 (nth weights 0)) (* 100.0 (nth weights 1)) (* 100.0 (nth weights 2)))]
    (if (= 1 (count weightVectors))
      (list line)
      (cons line (convertWeightVectorsToLines (rest weightVectors))))))

(defn trainPerceptronRojas []
  (do
    (PR/trainAndWeights)
    (Plot3D/displayNewPlot
      ;input vertices
      (list (struct Plot3D/Point3D 0.0 0.0 1.0) (struct Plot3D/Point3D 1.0 0.0 1.0)
        (struct Plot3D/Point3D 0.0 1.0 1.0) (struct Plot3D/Point3D 1.0 1.0 1.0))
      ;weight vector lines
      (list (struct Plot3D/Line
        (first (deref PR/perceptronWeights))
        (first (rest (deref PR/perceptronWeights)))
        (first (rest (rest (deref PR/perceptronWeights))))))
      20.0
      "Perceptron Weight Vector")))

(defn trainClusterer []
  (let [inputs (Input/getRandomInputVectors 3 100 50)
        weights (Clusterer/trainWeights (Input/getRandomWeightVectors 3 4) inputs)]
    (Plot3D/displayNewPlot
      (convertInputsToPoints inputs)
      (convertWeightVectorsToLines weights)
      100.0
      "Clusterer Weight Vectors")))

(def perceptronRojasButton (doto (new JButton "Train Single Perceptron (Rojas)")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (doInNewThread trainPerceptronRojas))))))

(def clustererButton (doto (new JButton "Train For Clustering")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (doInNewThread trainClusterer))))))

(def pcaButton (doto (new JButton "Train For PCA")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e])))))

(def buttonPanel (doto (new JPanel)
  (.add perceptronRojasButton)
  (.add clustererButton)
  (.add pcaButton)))

(defn loadUI []
  (let [frame (new JFrame "Neural Network UI")]
    (doto frame
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
      (.add buttonPanel)
      (.setSize 300 100)
      (.setVisible true))))
