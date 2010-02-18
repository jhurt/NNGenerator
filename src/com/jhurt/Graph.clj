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

(ns com.jhurt.Graph
  (:gen-class)
  (:require [com.jhurt.CollectionsUtils :as CU])
  (:require [com.jhurt.ga.GA :as GA])
  (:require [com.jhurt.nn.Common :as Common])
  (:require [com.jhurt.nn.ActivationFunctions :as Afns]))

(import
  '(javax.swing JFrame)
  '(java.awt Color Dimension Font)
  '(java.awt.event InputEvent MouseEvent)
  '(java.awt.geom Point2D)
  '(java.util ArrayList Random)
  '(edu.umd.cs.piccolo PCanvas PLayer PNode)
  '(edu.umd.cs.piccolo.event PDragEventHandler PInputEvent PInputEventFilter)
  '(edu.umd.cs.piccolo.nodes PPath PText))

(defn createNode
  "create a single node"
  [xPosition yPosition]
  (doto (PPath/createEllipse xPosition yPosition 20 20)
    (.addAttribute "edges" (new ArrayList))))

(defn getNodesForLayer
  "get a set of nodes for a single layer"
  [numberOfNodes xPosition]
  (loop [num numberOfNodes nodes [] yPosition 100]
    (if (= 0 num)
      nodes
      (recur (dec num) (conj nodes (createNode xPosition yPosition)) (+ yPosition 40)))))

(defn buildNodes
  "build a set of nodes based on the hidden layers, input and output arity"
  [nnLayers inputArity outputArity]
  (loop [layers nnLayers
         nodes [(getNodesForLayer (inc inputArity) 100)]
         xPosition 200]
    (if-not (seq layers)
      (conj nodes (getNodesForLayer outputArity xPosition))
      (recur (rest layers)
        (conj nodes (getNodesForLayer (inc ((first layers) :number-of-nodes)) xPosition))
        (+ xPosition 100)))))

(defn addNodesToCanvas
  "add the nodes from all nn layers to the PCanvas"
  [nodeLayer nodes]
  (loop [n (CU/flatten nodes)]
    (if (seq n)
      (do (.addChild nodeLayer (first n))
        (recur (rest n))))))

(defn getStartPoint [edge]
  (let [node (.get (.getAttribute edge "nodes") 0)]
    (.getCenter2D (.getFullBoundsReference node))))

(defn getEndPoint [edge]
  (let [node (.get (.getAttribute edge "nodes") 1)]
    (.getCenter2D (.getFullBoundsReference node))))

(defn drawEdge [edge]
  (let [start (getStartPoint edge) end (getEndPoint edge)]
    (doto edge (.reset)
      (.moveTo (.getX start) (.getY start))
      (.lineTo (.getX end) (.getY end)))))

(defn addEdgesToCanvas
  "add the edges to the PCanvas"
  [edgeLayer edges]
  (loop [e (CU/flatten edges)]
    (if (seq e)
      (do
        (.addChild edgeLayer (first e))
        (drawEdge (first e))
        (recur (rest e))))))

(defn stripHiddenBiasNodes [nodes]
  (loop [n (butlast (rest nodes))
         strippedNodes [(first nodes)]]
    (if-not (seq n)
      (conj strippedNodes (last nodes))
      (recur (rest n) (conj strippedNodes (butlast (first n)))))))

(defn getEdges [nodes]
  (loop [n nodes
         s (stripHiddenBiasNodes nodes)
         edges []]
    (if (= 1 (count n))
      edges
      (recur (rest n) (rest s)
        (conj edges (for [x (first n) y (first (rest s))]
          (let [edge (new PPath)]
            (.addAttribute edge "nodes" (new ArrayList))
            (.add (.getAttribute x "edges") edge)
            (.add (.getAttribute y "edges") edge)
            (.add (.getAttribute edge "nodes") x)
            (.add (.getAttribute edge "nodes") y)
            edge)))))))

(defn drawWeights [edgeLayer edges weights]
  (loop [e (CU/flatten edges) w (CU/flatten weights)]
    (println "count edges: " (count e) " count weights: " (count w))
    (if (and (seq e) (seq w))
      (do
        (let [edge (first e)
              weight (first w)
              start (getStartPoint edge) end (getEndPoint edge)
              xDiff (- (.getX end) (.getX start))
              yDiff (- (.getY end) (.getY start))
              theta (Math/atan (/ yDiff xDiff))
              text (new PText (str weight))]
          (.addChild edgeLayer text)
          (doto text (.setX (+ 15.0 (.getX start))) (.setY (.getY start))
            (.setFont (new Font "Times New Roman", Font/PLAIN, 6))
            (.rotateAboutPoint theta (.getX start) (.getY start))))
        (recur (rest e) (rest w)))))) 

(defn getNewCanvas [weights nnLayers inputArity outputArity]
  (let [canvas (new PCanvas)
        nodeLayer (.getLayer canvas)
        edgeLayer (new PLayer)
        nodes (buildNodes nnLayers inputArity outputArity)
        edges (getEdges nodes)]
    (do
      (.addChild (.getRoot canvas) edgeLayer)
      (.addLayer (.getCamera canvas) 0 edgeLayer)
      (addNodesToCanvas nodeLayer nodes)
      (addEdgesToCanvas edgeLayer edges)
      (drawWeights edgeLayer edges weights)
      (.setVisible canvas true))
    canvas))

;(defn -main []
;  (let [frame (new JFrame)
;        weights [[[1.1239125098872669 -3.599903081624636 2.3333333]
;                  [-0.6927869275859633 3.751123516794292 2.3333333]
;                  [-0.23056227969902052 3.3794881155733525 2.3333333]]
;                 [[1.1239125098872669 -3.599903081624636 2.3333333]
;                                   [-0.6927869275859633 3.751123516794292 2.3333333]
;                                   [-0.23056227969902052 3.3794881155733525 2.3333333]]
;                 [[1.6586619486262897] [-8.884370688215798] [1.5670473521919515]]]
;        nnLayers (list {:number-of-nodes 3 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative}
;        {:number-of-nodes 3 :activation-fn Afns/logistic :derivative-fn Afns/logisticDerivative})
;        canvas (getNewCanvas weights nnLayers 3)]
;    (.. frame (getContentPane) (add canvas))
;    (doto frame
;      (.setTitle "NN Graph")
;      (.setSize 800 600)
;      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
;      (.setVisible true))))

(defn -main []
  (let [frame (new JFrame)
        layers (Common/randomNetworkLayers 3 5 Afns/logistic Afns/logisticDerivative)
        inputArity 2
        outputArity 1
        weights (Common/getRandomWeightMatrices layers inputArity outputArity)
        canvas (getNewCanvas weights layers inputArity outputArity)]
    (.. frame (getContentPane) (add canvas))
    (doto frame
      (.setTitle "NN Graph")
      (.setSize 800 600)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))
