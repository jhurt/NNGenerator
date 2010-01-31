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
  (:require [com.jhurt.p2p.Master :as Master])
  (:require [com.jhurt.p2p.Jxta :as Jxta])
  (:require [com.jhurt.SwingUtils :as SwingUtils])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils])
  (:require [com.jhurt.CollectionsUtils :as CU]))

(import
  '(javax.swing JButton JFrame JMenu JMenuBar JMenuItem JPanel JScrollPane JSplitPane JTable JTextField)
  '(javax.swing.table AbstractTableModel)
  '(java.awt Color BorderLayout FlowLayout)
  '(java.awt.event ActionListener)
  '(java.util Date))

(def peerIdToPipeIdMap (ref (sorted-map)))
(def pipeIdToLastMessageMap (ref (sorted-map)))

(def masterButton (new JButton "Connect Master"))

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

(defn messageInCallback [messages]
  (loop [msgs messages]
    (if (seq msgs)
      (do
        (let [msg (first msgs)]
          ;keep a map of pipe id to peer id's
          (if (= (msg :name) Jxta/HEARTBEAT_ELEMENT_NAME)
            (dosync (ref-set peerIdToPipeIdMap (conj @peerIdToPipeIdMap
              (assoc (sorted-map) (msg :value) (msg :pipeId))))))
          ;keep a map of pipe id to last incoming message
          (dosync (ref-set pipeIdToLastMessageMap (conj @pipeIdToLastMessageMap
            (assoc (sorted-map) (msg :pipeId) msg)))))
        (.fireTableDataChanged tableModel)
        (recur (rest msgs))))))

(declare disconnectMasterListener)

(def connectMasterListener
  (proxy [ActionListener] []
    (actionPerformed [e]
      (.setEnabled masterButton false)
      (ThreadUtils/onThread (do
        (Master/start messageInCallback)
        (SwingUtils/doOnEdt (do
          (.setText masterButton "Disconnect Master")
          (.removeActionListener masterButton connectMasterListener)
          (.addActionListener masterButton disconnectMasterListener)
          (.setEnabled masterButton true))))))))

(def disconnectMasterListener
   (proxy [ActionListener] []
    (actionPerformed [e]
      (.setEnabled masterButton false)
      (ThreadUtils/onThread (do
        (Master/stop)
        (dosync (ref-set peerIdToPipeIdMap (sorted-map)))
        (dosync (ref-set pipeIdToLastMessageMap (sorted-map)))
        (.fireTableDataChanged tableModel)
        (SwingUtils/doOnEdt (do
          (.setText masterButton "Connect Master")
          (.removeActionListener masterButton disconnectMasterListener)
          (.addActionListener masterButton connectMasterListener)
          (.setEnabled masterButton true))))))))

(defn exit []
  ;TODO: cleanup
  (System/exit 0))

(def exitMenuItem (doto (new JMenuItem "Exit")
  (.addActionListener (proxy [ActionListener] []
    (actionPerformed [e] (exit))))))

(def trainMenuItem (new JMenuItem "Train"))

(def fileMenu (doto (new JMenu "File")
  (.add trainMenuItem)
  (.addSeparator)
  (.add exitMenuItem)))

(def menuBar (doto (new JMenuBar)
  (.add fileMenu)))

(def buttonPanel (doto (new JPanel)
  (.setBackground Color/WHITE)
  (.setLayout (new FlowLayout FlowLayout/CENTER 10 5))
  (.add (doto masterButton (.addActionListener connectMasterListener)))))

(def leftPanel (doto (new JPanel)
  (.setBackground Color/WHITE)
  (.setLayout (new BorderLayout))
  (.add buttonPanel BorderLayout/NORTH)))

(def nodeTable (doto (new JTable) (.setModel tableModel)))

(def tableScrollPane (new JScrollPane nodeTable))

(def splitPane (doto (new JSplitPane JSplitPane/HORIZONTAL_SPLIT leftPanel tableScrollPane)
  (.setContinuousLayout true)
  (.setOneTouchExpandable true)
  (.setDividerLocation 0.85)))

(defn -main []
  (let [frame (new JFrame "Neural Network UI")]
    (SwingUtils/setSizeBasedOnResolution frame)
    (.. frame (getContentPane) (add splitPane))
    (doto frame
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
      (.setJMenuBar menuBar)
      (.setVisible true))))
