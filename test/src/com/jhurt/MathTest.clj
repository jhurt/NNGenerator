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

(ns com.jhurt.MathTest
  (:gen-class
    :extends junit.framework.TestCase
    :state state
    :methods [[testMatrixAdd [] void] [testMatrixSubtract [] void]
              [testMatrixMultiply [] void] [testMatrixMultiplyScalar [] void]
              [testTransposeMatrix [] void] [testTransposeMatrix2 [] void]
              [testTransposeVector [] void] [testTransposeArray [] void]
              [testAreListsEqual [] void] [testMultiplyScalar [] void]
              [testArrayLessAnother [] void] [testArrayPlusAnother [] void]
              [testMatrixByVector [] void] [testVectorByMatrix [] void]
              [testMakeMatrix [] void]
              ]))
(use 'com.jhurt.Math)

(import '(junit.framework TestCase Assert))

(defn -testAreListsEqual [_]
  (Assert/assertTrue (areListsEqual [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]
    [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]))
  (Assert/assertFalse (areListsEqual [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]
    [[1 2 3 4] [5 5 5 5] [9 10 11 12] [13 14 15 16]])))

(defn -testMatrixAdd [_]
  (Assert/assertEquals [[2 4] [6 8]] (matrixAdd [[1 2] [3 4]] [[1 2] [3 4]]))
  (Assert/assertEquals [[2 4 6 8] [10 12 14 16] [18 20 22 24] [26 28 30 32]]
    (matrixAdd [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]
      [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])))

(defn -testMatrixSubtract [_]
  (Assert/assertEquals [[1 2] [3 4]] (matrixSubtract [[2 4] [6 8]] [[1 2] [3 4]]))
  (Assert/assertEquals [[1 1 1] [1 1 1] [1 1 1]]
    (matrixSubtract [[9 8 7] [6 5 4] [3 2 1]] [[8 7 6] [5 4 3] [2 1 0]])))

(defn -testMatrixMultiply [_]
  (Assert/assertTrue (areListsEqual [[1 2 3] [-1 -2 -3] [0 -1 -2]]
    (matrixMultiply [[1 0 0] [1 0 -1] [0 1 -1]] [[1 2 3] [2 3 4] [2 4 6]])))
  (Assert/assertTrue (areListsEqual [[4 11 -15]]
    (matrixMultiply [[2 0 -1 1]] [[1 5 -7] [1 1 0] [0 -1 1] [2 0 0]])))
  (Assert/assertTrue (areListsEqual [[7 4 -2] [1 3 -4]]
    (matrixMultiply [[3 -1 -2] [2 -2 -1]] [[4 2 0] [3 0 2] [1 1 0]]))))

(defn -testMatrixMultiplyScalar [_]
  (Assert/assertTrue (areListsEqual [[3 3 3] [3 3 3] [3 3 3]]
    (matrixMultiplyScalar [[1 1 1] [1 1 1] [1 1 1]] 3))))

(defn -testTransposeMatrix [_]
  (Assert/assertEquals [[1 3] [2 4]] (transposeMatrix [[1 2] [3 4]]))
  (Assert/assertTrue (areListsEqual
    [[1 5 9 13] [2 6 10 14] [3 7 11 15] [4 8 12 16]]
    (transposeMatrix [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]))))

(defn -testTransposeMatrix2 [_]
  (Assert/assertEquals [[1 3] [2 4]] (transposeMatrix2 [[1 2] [3 4]]))
  (Assert/assertTrue (areListsEqual
    [[1 5 9 13] [2 6 10 14] [3 7 11 15] [4 8 12 16]]
    (transposeMatrix2 [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]))))

(defn -testTransposeVector [_]
  (Assert/assertTrue (areListsEqual
    [[1] [2] [3] [4]]
    (transposeVector [1 2 3 4]))))

(defn -testTransposeArray [_]
  (Assert/assertTrue (areListsEqual
    [[1] [2] [3] [4]]
    (transposeArray [1 2 3 4]))))

(defn -testMultiplyScalar [_]
  (Assert/assertTrue (areListsEqual [2 4 6 8 10] (multiplyScalar [1 2 3 4 5] 2))))

(defn -testArrayLessAnother [_]
  (Assert/assertTrue (areListsEqual [1 2 3 4 5] (arrayLessAnother [9 10 11 12 13] [8 8 8 8 8]))))

(defn -testArrayPlusAnother [_]
  (Assert/assertTrue (areListsEqual [17 18 19 20 21] (arrayPlusAnother [9 10 11 12 13] [8 8 8 8 8]))))

(defn -testMatrixByVector [_]
  (Assert/assertTrue (areListsEqual [20 47 74] (matrixByVector [[1 2 3] [4 5 6] [7 8 9]] [2 3 4]))))

(defn -testVectorByMatrix [_]
  (Assert/assertTrue (areListsEqual [30 36 42] (vectorByMatrix [1 2 3] [[1 2 3] [4 5 6] [7 8 9]]))))

(defn -testMakeMatrix [_]
  (Assert/assertTrue (areListsEqual [[4 5 6] [8 10 12] [12 15 18]] (makeMatrix [1 2 3] [4 5 6]))))
