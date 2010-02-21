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

(ns com.jhurt.CollectionsUtils)

(defn bimap
  "Return a bimap with unique keys and unique values given an input map x"
  [x]
  (merge (zipmap (keys x) (vals x)) (zipmap (vals x) (keys x))))

(defn inverseMap
  "Return a map with keys and values reversed given an input map x"
  [x]
  (zipmap (vals x) (keys x)))

(defn structmapToHashmap [s]
  (zipmap (keys s) (vals s)))

;; 'flatten' written by Rich Hickey,
;; see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn removeFirstN
  "return a collection with the first n elements of the
  passed in collection removed"
  [coll n]
  (loop [x n
         results coll]
    (if (or (= 0 x) (not (seq coll)))
      results
      (recur (dec x) (rest results)))))
