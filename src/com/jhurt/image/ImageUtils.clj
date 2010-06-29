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
  com.jhurt.image.ImageUtils)

(import
  '(java.awt.image BufferedImage FilteredImageSource)
  '(javax.imageio ImageIO)
  '(javax.swing GrayFilter JFrame))

(defstruct Complex :real :imag)

(defn getImageFromFile [f] (ImageIO/read f))

(def grayFilter (new GrayFilter false 50))

(defn- getGrayValue
  "return the grayscale value of a specified coordinate of a grayscale BufferedImage"
  [img i j]
  (.getRed (.getColorModel img) (.getDataElements (.getRaster img) i j nil)))

(defn- getRGB
  "return the Red value of a specified coordinate of a grayscale BufferedImage"
  [img i j]
  (let [colorModel (.getColorModel img)
        dataElements (.getDataElements (.getRaster img) i j nil)]
    (int-array (list (.getRed colorModel dataElements)
      (.getGreen colorModel dataElements)
      (.getBlue colorModel dataElements)))))

(defn- setGrayValue
  "sets the grayscale value of a BufferedImage"
  [img i j val]
  (.setPixel (.getRaster img) i j (int-array [val val val])))

;int red = (pixel >> 16) & 0xff;
;int green = (pixel >> 8) & 0xff;
;int blue = (pixel) & 0xff;

(defn grayscaleImage
  "return a grayscale BufferedImage representation of an RGB BufferedImage"
  [img]
  (loop [i 0
         j 0
         grayImage (new BufferedImage (.getWidth img) (.getHeight img) BufferedImage/TYPE_BYTE_GRAY)]
    (if (= (.getWidth img) i) grayImage
      (do
        (let [RGB (getRGB img i j)]
          (setGrayValue grayImage i j (/ (+ (nth RGB 0) (nth RGB 1) (nth RGB 2)) 3.0))
          (if (= (dec (.getHeight img)) j)
            (recur (inc i) 0 grayImage)
            (recur i (inc j) grayImage)))))))

(defn getComplexValues
  "return a set of complex values whose real parts are the
grayscale value of a particular image coordinate and imaginary parts are 0"
  [img]
  (loop [i 0
         j 0
         vals []]
    (if (= (.getHeight img) j) vals
      (let [val (struct Complex (getGrayValue img i j) 0)]
        (if (= (dec (.getWidth img)) i)
          (recur 0 (inc j) (conj vals val))
          (recur (inc i) j (conj vals val)))))))

(defn- buildImage
  [cs img calcFn]
  (loop [cs cs
         i 0
         j 0]
    (if (= (.getHeight img) j) img
      (let [val (calcFn (first cs))]
        (setGrayValue img i j val)
        (if (= (dec (.getWidth img)) i)
          (recur (rest cs) 0 (inc j))
          (recur (rest cs) (inc i) j))))))

;PHASE(F) = ATAN( IMAGINARY(F)/REAL(F) )
(defn- calcPhase [c] (Math/atan (/ (c :imag) (c :real))))

;MAGNITUDE(F) = SQRT( REAL(F)^2+IMAGINARY(F)^2 )
(defn- calcMag [c] (Math/sqrt (+ (* (c :real) (c :real)) (* (c :imag) (c :imag)))))

(defn getPhaseImage [c]
  ;make sure it's a perfect square
  (assert (= 0 (mod (count c) (Math/sqrt (count c)))))
  (let [x (int (Math/sqrt (count c)))
        img (new BufferedImage x x BufferedImage/TYPE_BYTE_GRAY)]
    (buildImage c img calcPhase)))

(defn getMagnitudeImage [c]
  ;make sure it's a perfect square
  (assert (= 0 (mod (count c) (Math/sqrt (count c)))))
  (let [x (int (Math/sqrt (count c)))
        img (new BufferedImage x x BufferedImage/TYPE_BYTE_GRAY)]
    (buildImage c img calcMag)))
