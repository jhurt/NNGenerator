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

(ns com.jhurt.p2p.Slave
  (:gen-class)
  (:use [com.jhurt.p2p.Jxta :as Jxta]))

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.endpoint Message MessageElement StringMessageElement)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg PipeAdvertisement)
  '(net.jxta.platform NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeID PipeMsgEvent PipeMsgListener PipeService)
  '(net.jxta.logging Logging)
  '(net.jxta.util JxtaBiDiPipe JxtaServerPipe)
  '(java.io File)
  '(java.net URI)
  '(java.util Enumeration))

(def manager (new NetworkManager NetworkManager$ConfigMode/ADHOC "Slave"
  (.toURI (new File (new File ".nn_cache") (str "Slave" (rand-int Integer/MAX_VALUE))))))

(def netPeerGroup (ref nil))

(def pipe (ref nil))
(def discoveryService (ref nil))

(def pipeMsgListener (proxy [PipeMsgListener] []
  (pipeMsgEvent [#^PipeMsgEvent event]
    (let [msg (.getMessage event)
          msgElement (.getMessageElement msg Jxta/MESSAGE_NAMESPACE_NAME Jxta/TEST_ELEMENT_NAME)
          currentThreadName (.getName (Thread/currentThread))]
      (println "slave thread " currentThreadName " received message: " msg "\nmsg element: " msgElement)))))

(defn printAdv [advs]
  (apply
    (fn [adv & x]
      (println "pipe instance: " (instance? PipeAdvertisement adv))
      (println "adv class: " (.getName (class adv)))
      (println "adv type: " (.getAdvType adv))
      (if (seq? x) (printAdv x) (println (str x))))
    advs))

(defn getPipe [advs]
  (apply
    (fn [adv & x]
      (if (instance? PipeAdvertisement adv)
        (dosync (ref-set pipe (new JxtaBiDiPipe @netPeerGroup adv 20000 pipeMsgListener true)))
        (if (seq? x) (getPipe x))))
    advs))

;called whenever Advertisement responses are received from remote peers by the Discovery Service.
(def defaultDiscoveryListener (proxy [DiscoveryListener] []
  (discoveryEvent [#^DiscoveryEvent event]
    (let [response (.getResponse event)
          peerAdv (.getPeerAdvertisement response)
          remoteAdvertisements (.getAdvertisements response)
          localAdvertisements
          (.getLocalAdvertisements @discoveryService DiscoveryService/ADV nil nil)]
      (println "slave received discovery response from node: " (.getSource event) "\n")
      (getPipe (enumeration-seq remoteAdvertisements))))))

(defn registrationLoop [#^DiscoveryService discoveryService]
  (while (nil? @pipe)
    (println "slave getting remote advertisements\n")
    ; look for any peer
    (.getRemoteAdvertisements discoveryService
      ; no specific peer (propagate)
      nil
      DiscoveryService/ADV
      nil ;"Name"
      nil ;"pipe"
      10
      defaultDiscoveryListener)
    (Thread/sleep 10000)))

(defn start [discoveryService]
  (println "*************************starting slave node*************************\n")
  (registrationLoop discoveryService))

(defn -main []
  (.startNetwork manager)
  (dosync (ref-set netPeerGroup (.getNetPeerGroup manager)))
  (dosync (ref-set discoveryService (.getDiscoveryService @netPeerGroup)))
  (future (start @discoveryService)))
