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

(ns com.jhurt.Discovery
  (:use [com.jhurt.ThreadUtils :as ThreadUtils]))

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg PipeAdvertisement)
  '(net.jxta.platform NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeService)
  '(java.io File)
  '(java.util Enumeration))

(def manager (new NetworkManager NetworkManager$ConfigMode/ADHOC "DiscoveryServer"
  (.toURI (new File (new File ".cache") "DiscoveryServer"))))

(def discoveryService (ref nil))

(def defaultDiscoveryListener (proxy [DiscoveryListener] []
  (discoveryEvent [#^DiscoveryEvent event]
    (let [response (.getReponse event)
          advertisements (.getAdvertisements response)]
      (println "server received discovery response from peer  " (.getSource event) "\n")
      (apply println advertisements)))))

(defn getPipeAdvertisement []
  (doto (AdvertisementFactory/newAdvertisement (PipeAdvertisement/getAdvertisementType))
    (.setPipeID (IDFactory/newPipeID PeerGroupID/defaultNetPeerGroupID))
    (.setType PipeService/UnicastType)
    (.setName "Discovery Tutorial")))

(defn serverLoop [discoveryService]
  (while true
    (let [pipeAdv (getPipeAdvertisement)
          lifetime 30000 expiration 30000 waitTime 31000]
      (println "server publising pipeAdv:")
      (println (.toString pipeAdv))
      (doto discoveryService
        (.publish pipeAdv lifetime expiration)
        (.remotePublish #^Advertisement pipeAdv)) ;expiration))
      (Thread/sleep waitTime))))

(defn startServer [discoveryService]
  (println "*************************starting server node*************************\n\n")
  (serverLoop discoveryService))

(defn clientLoop [discoveryService]
  (while true
    (let [waitTime 5000]
      (println "client sleeping for: " waitTime)
      (Thread/sleep waitTime)
      (println "client sending a discovery message")
      ; look for any peer
      (.getRemoteAdvertisements discoveryService
        ; no specific peer (propagate)
        nil
        ; Adv type
        DiscoveryService/ADV
        ; Attribute = name
        "Name"
        ; Value = the tutorial
        "Discovery tutorial"
        ; one advertisement response is all we are looking for
        1
        ; no query specific listener. we are using a global listener
        nil))))

(defn startClient [discoveryService]
  (println "*************************starting client node*************************\n")
  (.getRemoteAdvertisements discoveryService
    ; no specific peer
    nil
    ; adv type
    DiscoveryService/ADV
    ; attr = any
    nil
    ; value = any
    nil
    ; one advertisement response is all we are looking for
    1
    ; no query specific listener. we are using a global listener
    nil)
  (clientLoop discoveryService))

(defn -main [s]
  (.startNetwork manager)
  (let [netPeerGroup (.getNetPeerGroup manager)]
    (dosync
      (ref-set discoveryService (doto (.getDiscoveryService netPeerGroup)
        (.addDiscoveryListener defaultDiscoveryListener))))
    (future (startServer @discoveryService))
    (future (startClient @discoveryService))
    (future (startClient @discoveryService))
    (read-line)))
  