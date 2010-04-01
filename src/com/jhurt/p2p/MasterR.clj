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

(def managers (ref []))
(def peerIdsToPipes (ref {}))

(defn defaultMessageInCallback [messages]
  (doall (map (fn [msg] (println msg)) messages)))

(defn getPipeMsgListener [callback]
  (proxy [PipeMsgListener] []
    (pipeMsgEvent [#^PipeMsgEvent event]
      (let [source (.getSource event)
            msg (.getMessage event)
            elements (iterator-seq (.getMessageElements msg))]
        (callback
          (map
            (fn [element]
              (struct Jxta/InputMessage (str (.getPipeID (.getPipeAdvertisement source)))
                (.getElementName element) (str element) (System/currentTimeMillis)))
            elements))))))

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
    #(while true
      (let [waitTime 60000]
        (println "master publishing register pipe advertisement")
        (.publish discoveryService pipeAdv)
        (.remotePublish discoveryService pipeAdv)
        (Thread/sleep waitTime)))))

(defn acceptNewPeerConnection [#^JxtaBiDiPipe pipe callback]
  (let [peerId (.getPeerID (.getRemotePeerAdvertisement pipe))]
    (do
      ;keep a map of input PipeId's to JxtaBidiPipes
      (dosync (ref-set peerIdsToPipes (conj @peerIdsToPipes {(str peerId) pipe})))
      (ThreadUtils/onThread
        #(do
          (println "JxtaBidiPipe accepted from " peerId "\n")
          (.setMessageListener pipe (getPipeMsgListener callback)))))))

(defn pipeConnectionLoop [#^JxtaServerPipe serverPipe callback]
  (ThreadUtils/onThread
    #(while true
      (if-not (.isClosed serverPipe)
        (do
          (println "\n\nserver waiting for peer connection\n")
          (acceptNewPeerConnection (.accept serverPipe) callback))
        (Thread/sleep 100)))))

(defn configureMasterNode [rdvUri configurator]
  (let [seedingURI (URI/create rdvUri)]
    (doto configurator
      (.setHome (new File Jxta/JXTA_HOME))
      (.setUseMulticast false)
      (.setUseOnlyRelaySeeds true)
      (.setUseOnlyRendezvousSeeds true)
      (.setTcpEnabled true)
      (.setTcpIncoming false)
      (.setTcpOutgoing true)
      (.addSeedRelay seedingURI)
      (.addSeedRendezvous seedingURI)
      (.addRdvSeedingURI seedingURI)
      (.addRelaySeedingURI seedingURI)
      (.save))))

(defn start
  "Start a master node. messageInCallback gets called whenever a message is received"
  [rdvUri messageInCallback]
  (let [port (Jxta/getPortFromUri rdvUri)
        manager (new NetworkManager NetworkManager$ConfigMode/EDGE (str "Master" port)
      (.toURI (new File (new File Jxta/JXTA_HOME) (str "Master" port))))]
    (configureMasterNode rdvUri (.getConfigurator manager))
    (.startNetwork manager)
    (let [netPeerGroup (.getNetPeerGroup manager)
          adv (Jxta/getPipeAdvertisement)
          discoveryService (.getDiscoveryService netPeerGroup)
          serverPipe (doto (new JxtaServerPipe netPeerGroup adv) (.setPipeTimeout 0))]
      (dosync (ref-set managers (conj @managers manager)))
      (Jxta/waitForRendezvous netPeerGroup)
      (registrarLoop discoveryService adv)
      (pipeConnectionLoop serverPipe messageInCallback))))

(defn startAll
  "start all master nodes, one for each rdv/relay node"
  [rdvUris messageInCallback]
  (start (first rdvUris) messageInCallback))
  ;(doall (map (fn [rdvUri] (start rdvUri messageInCallback)) rdvUris)))

(defn stop
  [manager]
  (println "Stopping master")
  (.stopNetwork manager))

(defn stopAll
  "stop all master nodes"
  []
  (doall (map (fn [m] (stop m)) @managers)))
