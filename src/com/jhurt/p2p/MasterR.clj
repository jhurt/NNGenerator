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
  com.jhurt.p2p.MasterR
  (:gen-class)
  (:require [com.jhurt.p2p.Jxta :as Jxta])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils]))

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.endpoint Message MessageElement StringMessageElement)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg PipeAdvertisement)
  '(net.jxta.impl.protocol ModuleImplAdv)
  '(net.jxta.platform NetworkConfigurator NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeID PipeService PipeMsgEvent PipeMsgListener)
  '(net.jxta.util JxtaBiDiPipe JxtaServerPipe)
  '(java.io File)
  '(java.util Enumeration)
  '(java.net URI))

(def serverPipe (ref nil))
(def registrate (ref false))
(def listen (ref false))
(def callback (ref nil))
(def peerIdsToPipes (ref {}))

(defn defaultMessageInCallback [messages]
  (loop [msgs messages]
    (if (seq msgs)
      (do
        (println (first msgs))
        (recur (rest msgs))))))

(def manager (new NetworkManager NetworkManager$ConfigMode/EDGE "Master"
  (.toURI (new File (new File Jxta/JXTA_HOME) "Master"))))

(def pipeMsgListener
  (proxy [PipeMsgListener] []
    (pipeMsgEvent [#^PipeMsgEvent event]
      (let [source (.getSource event)
            msg (.getMessage event)
            elements (iterator-seq (.getMessageElements msg))]
        (if-not (nil? @callback)
          (@callback
            (map
              (fn [element]
                (struct Jxta/InputMessage (str (.getPipeID (.getPipeAdvertisement source)))
                  (.getElementName element) (str element) (System/currentTimeMillis)))
              elements)))))))

(defn isConnectedToPeer
  "return true if we are connected to the peer specified by peer id"
  [peerId]
  (let [pipe (@peerIdsToPipes peerId)]
    (and (not (nil? pipe)) (.isBound pipe))))

(defn sendMessageToPeer [peerId elementName msg]
  (let [pipe (@peerIdsToPipes peerId)]
    (if-not (nil? pipe)
      (do
        (println "sending message: " msg " to " peerId)
        (let [strMsgElement (new StringMessageElement elementName msg nil)]
          (.sendMessage pipe
            (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME strMsgElement))))))))

(defn registrarLoop [#^DiscoveryService discoveryService pipeAdv]
  (ThreadUtils/onThread
    #(while @registrate
      (let [waitTime 60000]
        (println "master publishing register pipe advertisement")
        (.publish discoveryService pipeAdv)
        (.remotePublish discoveryService pipeAdv)
        (Thread/sleep waitTime)))))

(defn acceptNewPeerConnection [#^JxtaBiDiPipe pipe]
  (let [peerId (.getPeerID (.getRemotePeerAdvertisement pipe))]
    (if @listen
      (do
        ;keep a map of input PipeId's to JxtaBidiPipes
        (dosync (ref-set peerIdsToPipes (conj @peerIdsToPipes {(str peerId) pipe})))
        (ThreadUtils/onThread
          #(do
            (println "JxtaBidiPipe accepted from " peerId "\n")
            (.setMessageListener pipe pipeMsgListener)))))))

(defn pipeConnectionLoop [#^JxtaServerPipe serverPipe]
  (ThreadUtils/onThread
    #(while @listen
      (if-not (.isClosed serverPipe)
        (do
          (println "\n\nserver waiting for peer connection\n")
          (acceptNewPeerConnection (.accept serverPipe)))
        (Thread/sleep 100)))))

(defn configureMasterNode []
  (let [seedingURI (URI/create Jxta/RDV_URI)]
    (doto (.getConfigurator manager)
      (.setHome (new File Jxta/JXTA_HOME))
      (.setUseMulticast false)
      (.addSeedRelay seedingURI)
      (.addSeedRendezvous seedingURI)
      (.addRdvSeedingURI seedingURI)
      (.addRelaySeedingURI seedingURI)
      (.setUseOnlyRelaySeeds true)
      (.setUseOnlyRendezvousSeeds true)
      (.setTcpEnabled true)
      (.setTcpIncoming false)
      (.setTcpOutgoing true)
      (.save))))

(defn start [messageInCallback]
  (Jxta/clearLocalCache)
  (dosync (ref-set callback messageInCallback))
  (configureMasterNode)
  (.startNetwork manager)
  (let [netPeerGroup (.getNetPeerGroup manager)
        adv (Jxta/getPipeAdvertisement)
        discoveryService (.getDiscoveryService netPeerGroup)]
    (Jxta/waitForRendezvous netPeerGroup)
    (dosync (ref-set serverPipe (doto (new JxtaServerPipe netPeerGroup adv) (.setPipeTimeout 0))))
    (dosync (ref-set registrate true))
    (dosync (ref-set listen true))
    (registrarLoop discoveryService adv)
    (pipeConnectionLoop @serverPipe)))

(defn stop []
  (println "Stopping master")
  (dosync (ref-set registrate false))
  (dosync (ref-set listen false))
  (.close @serverPipe)
  (.stopNetwork manager)
  (doall (map (fn [pipe] (do (.setMessageListener pipe nil) (.close pipe))) (vals @peerIdsToPipes))))

(defn -main [] (start defaultMessageInCallback))
