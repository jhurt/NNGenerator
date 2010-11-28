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

;;Functions for extracting data from label and image files from the MNIST database
;;The file formats are described here: http://yann.lecun.com/exdb/mnist/

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.image.MNIST
  (:gen-class)
  (:use [clojure.java.io])
  (:use [com.jhurt.Serialization]))

(import
  '(java.awt.image BufferedImage ImageObserver FilteredImageSource)
  '(java.io DataInputStream File FileWriter)
  '(javax.imageio ImageIO)
  '(java.awt Graphics))
  
(defn- extractLabels [stream count]
  (loop [labels [] c count]
    (if (= 0 c)
      (do (.close stream) labels)
      (recur (conj labels (.readUnsignedByte stream)) (dec c)))))

(defn- readLabels
  "read the labels from an idx file from the MNIST database"
  [labelFile]
  (let [stream (new DataInputStream (input-stream labelFile))
        magic (do (.readInt stream))
        count (do (.readInt stream))]
    (extractLabels stream count)))

(defn- getOutputs [labels]
  (let [binaryStrings (map (fn [x] (Integer/toString x 2)) labels)
        paddedStrings (map (fn [x] (let [pad (- 4 (count x))] (str (reduce str (repeat pad "0")) x))) binaryStrings)]
    (map (fn [x] (map (fn [y] (Integer/parseInt (str y))) (.toCharArray x))) paddedStrings)))

(defn extractGrid
  "extract a 4x4 grid from the image in the form of a one dimensional array"
  [image]
  (assert (= 28 (count image)))
  (assert (= 28 (count (last image))))
  (loop [i 0 j 0 grid []]
    (let [upperRow (* 7 (inc i))
          upperCol (* 7 (inc j))
          rows (subvec image (* i 7) upperRow)
          totalPixels (reduce + (flatten (map (fn [row] (subvec row (* j 7) upperCol)) rows)))
          averagePixel (/ totalPixels 49)
          gridVal (if (< averagePixel 60) 0 1)]
      (cond
        (and (= 3 j) (= 3 i)) (let [g (conj grid gridVal)] (assert (= 16 (count g))) g)
        (= 3 i) (recur 0 (inc j) (conj grid gridVal))
        :else (recur (inc i) j (conj grid gridVal))))))

(defn- extractInputs [stream count width height]
  (loop [inputs [] rows [] row [] c count w 0 h 0]
    (cond
      (= 0 c) (do (.close stream) inputs)
      (= w width) (recur inputs (conj rows row) [] c 0 (inc h))
      (= h height) (recur (conj inputs (extractGrid rows)) [] [] (dec c) 0 0)
      :else (recur inputs rows (conj row (.readUnsignedByte stream)) c (inc w) h))))

(defn- readInputs
  "read the digit images from an idx file from the MNIST database"
  [imageFile]
  (let [stream (new DataInputStream (input-stream imageFile))
        magic (do (.readInt stream))
        total (do (.readInt stream))
        width (do (.readInt stream))
        height (do (.readInt stream))]
    (extractInputs stream total width height)))

(defn- extractImages [stream count width height]
  (loop [images [] rows [] row [] c count w 0 h 0]
    (cond
      (= 0 c) (do (.close stream) images)
      (= w width) (recur images (conj rows row) [] c 0 (inc h))
      (= h height) (recur (conj images rows) [] [] (dec c) 0 0)
      :else (recur images rows (conj row (.readUnsignedByte stream)) c (inc w) h))))

(defn- readImages
  "read the digit images from an idx file from the MNIST database"
  [imageFile]
  (let [stream (new DataInputStream (input-stream imageFile))
        magic (do (.readInt stream))
        total (do (.readInt stream))
        width (do (.readInt stream))
        height (do (.readInt stream))]
    (extractImages stream total width height)))

(defn- getInputOutputPairs [imageFile labelFile]
  (println "reading image file: " imageFile " and label file: " labelFile)
  (let [inputs (readInputs imageFile)
        labels (readLabels labelFile)
        outputs (getOutputs labels)]
       (map (fn [x y] {:input x :output y}) inputs outputs)))

(def trainingLabels "train-labels-idx1-ubyte")
(def trainingImages "train-images-idx3-ubyte")
(def testLabels "t10k-labels-idx1-ubyte")
(def testImages "t10k-images-idx3-ubyte")
(def trainingData "nn_training_data")
(def testData "nn_test_data")

(defn- loadPairs
  "load training pairs from a file"
  [f] (deserializeFile f))

(defn loadTrainingPairs [] (loadPairs trainingData))

(defn loadTestPairs [] (loadPairs testData))

(defn -main
  "extract the training data from the files so this does not have to
  be done by the slaves each time"
  [& args]
   (let [trainingDataWriter (new FileWriter (new File trainingData))
         testDataWriter (new FileWriter (new File testData))]
         (doto trainingDataWriter (.write (serialize (getInputOutputPairs trainingImages trainingLabels))) (.flush) (.close))
         (doto testDataWriter (.write (serialize (getInputOutputPairs testImages testLabels))) (.flush) (.close))))
