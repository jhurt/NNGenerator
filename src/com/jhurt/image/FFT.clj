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

;;radix-2 Cooley-Tukey algorithm for computing the Fast Fourier transform of an image

(ns
  #^{:author "Jason Lee Hurt"}
  com.jhurt.image.FFT
  (:use [com.jhurt.image.ImageUtils]))

(defn combine
  "combine the results of each radix split"
  [N qs rs]
  (loop [k 0
         y1 []
         y2 []
         qs qs
         rs rs]
    (if (= N k) (concat y1 y2)
      (let [kth (/ (* -2.0 k Math/PI) N)
            wreal (Math/cos kth)
            wimag (Math/sin kth)
            q (first qs)
            r (first rs)
            treal (- (* wreal (r :real)) (* wimag (r :imag)))
            timag (+ (* wreal (r :imag)) (* wimag (r :real)))
            y1real (+ (q :real) treal)
            y1imag (+ (q :imag) timag)
            y2real (- (q :real) treal)
            y2imag (- (q :imag) timag)]
        (recur
          (inc k)
          (conj y1 (struct Complex y1real y1imag))
          (conj y2 (struct Complex y2real y2imag))
          (rest qs)
          (rest rs))))))

(defn fft
  "compute the FFT of a set of Complex values x"
  [x]
  (let [N (count x)]
    (if (= 1 N) (vector (first x))
      (do
        (assert (= 0 (mod N 2)))
        (let [even (take-nth 2 x)
              odd (take-nth 2 (rest x))
              qs (fft even)
              rs (fft odd)]
          (combine (count qs) qs rs))))))

; public static Complex[] cconvolve(Complex[] x, Complex[] y) {
;
;    // should probably pad x and y with 0s so that they have same length
;    // and are powers of 2
;    if (x.length != y.length) {
;      throw new RuntimeException("Dimensions don't agree");
;    }
;
;    int N = x.length;
;
;    // compute FFT of each sequence
;    Complex[] a = fft(x);
;    Complex[] b = fft(y);
;
;    // point-wise multiply
;    Complex[] c = new Complex[N];
;    for (int i = 0; i < N; i++) {
;      c[i] = a[i].times(b[i]);
;    }
;
;    // compute inverse FFT
;    return ifft(c);
;  }
;
;
;  // compute the linear convolution of x and y
;
;  public static Complex[] convolve(Complex[] x, Complex[] y) {
;    Complex ZERO = new Complex(0, 0);
;
;    Complex[] a = new Complex[2 * x.length];
;    for (int i = 0; i < x.length; i++) a[i] = x[i];
;    for (int i = x.length; i < 2 * x.length; i++) a[i] = ZERO;
;
;    Complex[] b = new Complex[2 * y.length];
;    for (int i = 0; i < y.length; i++) b[i] = y[i];
;    for (int i = y.length; i < 2 * y.length; i++) b[i] = ZERO;
;
;    return cconvolve(a, b);
;  }
