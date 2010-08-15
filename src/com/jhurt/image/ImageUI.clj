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
  com.jhurt.image.ImageUI
  (:gen-class)
  (:require [com.jhurt.SwingUtils :as SwingUtils])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils])
  (:use com.jhurt.image.FFT)
  (:use com.jhurt.image.ImageUtils))

(import
  '(javax.swing JFrame JPanel JButton JFileChooser GrayFilter)
  '(javax.swing.filechooser FileFilter)
  '(java.awt.event ActionListener)
  '(java.awt.image BufferedImage ImageObserver FilteredImageSource)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(java.awt Graphics))

(def image (ref nil))
(def imageFrame (ref nil))

(defn getImagePanel [img]
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (.drawImage g img 0 0 nil))))

(defn openImage [imageFile]
  (let [bufferedImage (. ImageIO (read imageFile))]
    (if-not (nil? @imageFrame) (.dispose @imageFrame))
    (dosync (ref-set image bufferedImage) (ref-set imageFrame (new JFrame)))
    (doto @imageFrame
      (.setVisible false)
      (.setTitle (.getName imageFile))
      (.setSize (.getWidth bufferedImage) (.getHeight bufferedImage))
      (.add (getImagePanel bufferedImage))
      (.setVisible true))))

(def imageFileFilter (proxy [FileFilter] []
  (accept [f]
    (and (not (nil? f))
      (or (.isDirectory f) (.. f (getName) (endsWith "gif")) (.. f (getName) (endsWith "jpg")) (.. f (getName) (endsWith "jpeg")) (.. f (getName) (endsWith "png")))))
  (getDescription [] "Image Files")))

(def openButton (doto (new JButton "Open Image")
  (.addActionListener
    (let [fileChooser (doto (new JFileChooser)
      (.setFileFilter imageFileFilter))]
      (proxy [ActionListener] []
        (actionPerformed [e]
          (if (= (JFileChooser/APPROVE_OPTION) (. fileChooser (showOpenDialog openButton)))
            (openImage (.getSelectedFile fileChooser)))))))))

(def grayscaleButton (doto (new JButton "Grayscale")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (ThreadUtils/onThread (fn []
          (let [img (grayscaleImage @image)
                frame (new JFrame "Grayscale")]
            (doto frame
              (.setSize (.getWidth img) (.getHeight img))
              (.add (getImagePanel img)))
            (SwingUtils/doOnEdt #(doto frame
              (.setVisible true)
              (.repaint)
              (.requestFocus)))))))))))

(def fftPhaseButton (doto (new JButton "FFT Phase")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (ThreadUtils/onThread (fn []
          (let [vals (getComplexValues (grayscaleImage @image))
                img (getPhaseImage (fft vals))
                frame (new JFrame "FFT Phase")]
            (doto frame
              (.setSize (.getWidth img) (.getHeight img))
              (.add (getImagePanel img)))
            (SwingUtils/doOnEdt #(doto frame
              (.setVisible true)
              (.repaint)
              (.requestFocus)))))))))))

(def fftMagButton (doto (new JButton "FFT Magnitude")
  (.addActionListener
    (proxy [ActionListener] []
      (actionPerformed [e]
        (ThreadUtils/onThread (fn []
          (let [img (getMagnitudeImage (fft (getComplexValues (grayscaleImage @image))))
                frame (new JFrame "FFT Magnitude")]
            (doto frame
              (.setSize (.getWidth img) (.getHeight img))
              (.add (getImagePanel img)))
            (SwingUtils/doOnEdt #(doto frame
              (.setVisible true)
              (.repaint)
              (.requestFocus)))))))))))

(def buttonPanel (doto (new JPanel)
  (.add openButton)
  (.add grayscaleButton)
  (.add fftMagButton)
  (.add fftPhaseButton)))

(defn -main [s] (doto (new JFrame "Image UI")
  (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
  (.add buttonPanel)
  (.setSize 250 200)
  (.setVisible true)))
