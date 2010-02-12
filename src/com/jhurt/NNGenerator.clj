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

(ns com.jhurt.NNGenerator
  (:gen-class)
  (:use [com.jhurt.Serialization])
  (:require [com.jhurt.p2p.MasterR :as Master])
  (:require [com.jhurt.p2p.Jxta :as Jxta])
  (:require [com.jhurt.SwingUtils :as SwingUtils])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils])
  (:require [com.jhurt.CollectionsUtils :as CU])
  (:require [com.jhurt.ga.GA :as GA])
  (:require [com.jhurt.nn.ActivationFunctions :as Afns]))

(import
  '(javax.swing JButton JFrame JLabel JMenu JMenuBar JMenuItem JPanel JProgressBar JScrollPane JSplitPane JTable JTextField)
  '(javax.swing.table AbstractTableModel)
  '(java.awt Color BorderLayout GridBagConstraints GridBagLayout Insets)
  '(java.awt.event ActionListener)
  '(java.util Date))

(def peerIdToPipeIdMap (ref (sorted-map)))
(def pipeIdToLastMessageMap (ref (sorted-map)))

(def masterButton (new JButton "Connect Master"))

(def inputMaxLayers (doto (new JTextField 10) (.setText "2")))
(defn getMaxLayers []
  (Integer/parseInt (.getText inputMaxLayers)))

(def inputMaxNodesPerLayer (doto (new JTextField 10) (.setText "5")))
(defn getMaxNodesPerLayer []
  (Integer/parseInt (.getText inputMaxNodesPerLayer)))

(def inputMaxTrainingCycles (doto (new JTextField 10) (.setText "10000")))
(defn getMaxTrainingCycles []
  (Integer/parseInt (.getText inputMaxTrainingCycles)))

(def inputNumberOfGenerations (doto (new JTextField 10) (.setText "50")))
(defn getNumberOfGenerations []
  (Integer/parseInt (.getText inputNumberOfGenerations)))

(def progressBar (doto (new JProgressBar 0) (.setValue 0) (.setStringPainted true)))

(def generateXorMenuItem (new JMenuItem "Generate XOR NN"))

(def tableModel (proxy [AbstractTableModel] []
  (getColumnName [index]
    (cond (== 0 index) "Peer ID"
      (== 1 index) "Last Message Key"
      (== 2 index) "Last Message Value"
      (== 3 index) "Last Message Received"
      (== 4 index) "Pipe ID"))
  (getRowCount [] (count @peerIdToPipeIdMap))
  (getColumnCount [] 5)
  (getValueAt [rowIndex columnIndex]
    (let [pipeId (nth (seq (keys @pipeIdToLastMessageMap)) rowIndex)
          msg (@pipeIdToLastMessageMap pipeId)
          peerId ((CU/inverseMap @peerIdToPipeIdMap) pipeId)]
      (if-not (nil? msg)
        (cond (== 0 columnIndex) peerId
          (== 1 columnIndex) (msg :name)
          (== 2 columnIndex) (msg :value)
          (== 3 columnIndex) (str (new Date (msg :time)))
          (== 4 columnIndex) pipeId))))))

(defn getLivePeers
  "return a list of peers to which the master is currently connected"
  []
  (let [peerIds (keys @peerIdToPipeIdMap)]
    (filter Master/isConnectedToPeer peerIds)))

(def generationToResults (ref {}))

(defn breed [generation results]
  (println "\n\n******* breeding generation: " generation "\n\n")
  (let [peers (getLivePeers)
        children (GA/breed results (count peers))]
    (loop [p peers c children]
      (if (and (seq p) (seq c))
        (let [peerId (first p)
              child (first c)
              msg {:layers child :training-cycles (getMaxTrainingCycles)
                   :generation (inc generation)}]
          (Master/sendMessageToPeer peerId Jxta/TRAIN_XOR_ELEMENT_NAME (serialize msg))
          (recur (rest p) (rest c)))))))

(defn removeTrainingGeneration [generation]
  (let [results (@generationToResults generation)]
    (if-not (nil? results)
      (dosync (ref-set generationToResults (dissoc @generationToResults generation))))))

(defn checkBreedGeneration
  "check to see if the generation is ready to breed"
  [generation results]
  (println " " (count results) " results for generation: " generation)
  (if (= (count (getLivePeers)) (count results))
    (if (= generation (getNumberOfGenerations))
      (do
        (println "final results: " results)
        (SwingUtils/doOnEdt
          #(do (.setValue progressBar generation)
            (.setEnabled generateXorMenuItem true))))
      (do
        (breed generation results)
        (removeTrainingGeneration generation)
        (SwingUtils/doOnEdt
          #(do (.setValue progressBar generation)))))))

(defn addTrainingResult [generation result]
  (dosync
    (let [results (@generationToResults generation)]
      (if (seq results)
        (ref-set generationToResults
          (conj @generationToResults {generation (conj results result)}))
        (ref-set generationToResults (conj @generationToResults {generation (list result)})))))
  (checkBreedGeneration generation (@generationToResults generation)))

;a multimethod for handling incoming messages
;the dispatch function is the name of the message element
(defmulti handleIncomingMessage (fn [msg] (msg :name)))

(defmethod handleIncomingMessage Jxta/HEARTBEAT_ELEMENT_NAME [msg]
  ;keep a map of pipe id to peer id's
  (dosync (ref-set peerIdToPipeIdMap (conj @peerIdToPipeIdMap
    (assoc (sorted-map) (msg :value) (msg :pipeId)))))
  ;keep a map of pipe id to last incoming heartbeat
  (dosync (ref-set pipeIdToLastMessageMap (conj @pipeIdToLastMessageMap
    (assoc (sorted-map) (msg :pipeId) msg)))))

(defmethod handleIncomingMessage Jxta/FINISH_TRAIN_XOR_ELEMENT_NAME [msg]
  (let [trainResult (deserialize (msg :value))]
    (if-not (nil? trainResult)
      (let [generation (trainResult :generation)]
        (if-not (nil? generation)
          (addTrainingResult generation trainResult))))))

;fired whenever a slave sends the master a message
(defn messageInCallback [messages]
  (loop [msgs messages]
    (if (seq msgs)
      (do
        (handleIncomingMessage (first msgs))
        (.fireTableDataChanged tableModel)
        (recur (rest msgs))))))

;forward declaration
(declare disconnectMasterListener)

;fired when the connect button is clicked, connects the master to the network
(def connectMasterListener
  (proxy [ActionListener] []
    (actionPerformed [e]
      (.setEnabled masterButton false)
      (ThreadUtils/onThread
        (fn []
          (Master/start messageInCallback)
          (SwingUtils/doOnEdt
            #(do (.setText masterButton "Disconnect Master")
              (.removeActionListener masterButton connectMasterListener)
              (.addActionListener masterButton disconnectMasterListener)
              (.setEnabled masterButton true))))))))

;fired when the disconnect button is clicked, disconnects the master from the network
(def disconnectMasterListener
  (proxy [ActionListener] []
    (actionPerformed [e]
      (.setEnabled masterButton false)
      (ThreadUtils/onThread
        (fn []
          (Master/stop)
          (dosync (ref-set peerIdToPipeIdMap (sorted-map)))
          (dosync (ref-set pipeIdToLastMessageMap (sorted-map)))
          (.fireTableDataChanged tableModel)
          (SwingUtils/doOnEdt
            #(do (.setText masterButton "Connect Master")
              (.removeActionListener masterButton disconnectMasterListener)
              (.addActionListener masterButton connectMasterListener)
              (.setEnabled masterButton true))))))))

(defn exit []
  (System/exit 0))

(def exitMenuItem (doto (new JMenuItem "Exit")
  (.addActionListener (proxy [ActionListener] []
    (actionPerformed [e] (exit))))))

(def menuItemGenerateXOR (doto generateXorMenuItem
  (.addActionListener (proxy [ActionListener] []
    (actionPerformed [e]
      (ThreadUtils/onThread
        (fn [] (doall (map
          (fn [peerId] (let [layers (GA/randomNetworkLayers (getMaxLayers) (getMaxNodesPerLayer) Afns/logistic Afns/logisticDerivative)
                             msg {:layers layers :training-cycles (getMaxTrainingCycles) :generation 1}]
            (Master/sendMessageToPeer peerId Jxta/TRAIN_XOR_ELEMENT_NAME (serialize msg))))
          (getLivePeers)))
          (SwingUtils/doOnEdt
            #(do (.setEnabled generateXorMenuItem false)
              (.setMaximum progressBar (getNumberOfGenerations))
              (.setValue progressBar 0)
              (.setVisible progressBar true))))))))))

(def menuItemGenerateFacialRecognition (doto (new JMenuItem "Generate Facial Recoginition NN")
  (.addActionListener (proxy [ActionListener] []
    (actionPerformed [e] ;TODO
      )))))

(def fileMenu (doto (new JMenu "File")
  (.add menuItemGenerateXOR)
  (.add menuItemGenerateFacialRecognition)
  (.addSeparator)
  (.add exitMenuItem)))

(def menuBar (doto (new JMenuBar)
  (.add fileMenu)))

(def topLeftPanel (doto (new JPanel)
  (.setBackground Color/WHITE)))

(defn layoutTopLeftPanel []
  (let [constraints (new GridBagConstraints)]
    (.setLayout topLeftPanel (new GridBagLayout))
    (set! (.fill constraints) (. GridBagConstraints VERTICAL))
    (set! (.gridx constraints) 0)
    (set! (.gridy constraints) 0)
    (set! (.insets constraints) (new Insets 10 0 40 0))
    (set! (.gridwidth constraints) 2)
    (.add topLeftPanel (doto masterButton (.addActionListener connectMasterListener)) constraints)

    (set! (.gridy constraints) 1)
    (set! (.gridwidth constraints) 1)
    (set! (.insets constraints) (new Insets 5 0 5 0))
    (.add topLeftPanel (new JLabel "Maximum # of Hidden Layers:") constraints)
    (set! (.gridx constraints) 1)
    (.add topLeftPanel inputMaxLayers constraints)

    (set! (.gridx constraints) 0)
    (set! (.gridy constraints) 2)
    (.add topLeftPanel (new JLabel "Maximum # of Nodes Per Layer:") constraints)
    (set! (.gridx constraints) 1)
    (.add topLeftPanel inputMaxNodesPerLayer constraints)

    (set! (.gridx constraints) 0)
    (set! (.gridy constraints) 3)
    (.add topLeftPanel (new JLabel "Maximum # of Training Cycles:") constraints)
    (set! (.gridx constraints) 1)
    (.add topLeftPanel inputMaxTrainingCycles constraints)

    (set! (.gridx constraints) 0)
    (set! (.gridy constraints) 4)
    (.add topLeftPanel (new JLabel "# of Generations:") constraints)
    (set! (.gridx constraints) 1)
    (.add topLeftPanel inputNumberOfGenerations constraints)


    (set! (.insets constraints) (new Insets 30 0 30 0))
    (set! (.gridx constraints) 0)
    (set! (.gridy constraints) 5)
    (set! (.gridwidth constraints) 2)
    (.add topLeftPanel (doto progressBar (.setVisible false)) constraints)))

(def leftPanel (doto (new JPanel)
  (.setBackground Color/WHITE)
  (.setLayout (new BorderLayout))
  (.add topLeftPanel BorderLayout/NORTH)))

(def nodeTable (doto (new JTable) (.setModel tableModel)))

(def tableScrollPane (new JScrollPane nodeTable))

(def splitPane (doto (new JSplitPane JSplitPane/HORIZONTAL_SPLIT leftPanel tableScrollPane)
  (.setContinuousLayout true)
  (.setOneTouchExpandable true)
  (.setDividerLocation 0.85)))

(defn -main []
  (let [frame (new JFrame "Neural Network UI")]
    (SwingUtils/setSizeBasedOnResolution frame)
    (layoutTopLeftPanel)
    (.. frame (getContentPane) (add splitPane))
    (doto frame
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
      (.setJMenuBar menuBar)
      (.setVisible true))))
