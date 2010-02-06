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

(ns com.jhurt.p2p.Jxta)

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg PipeAdvertisement)
  '(net.jxta.platform NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeID PipeService)
  '(java.io File)
  '(java.util Enumeration)
  '(java.net URI))

(defstruct InputMessage :pipeId :name :value :time)

;(def RDV_URI "tcp://70.180.196.124:9701")
(def RDV_URI "tcp://192.168.0.191 :9701")
;(def RDV_URI "tcp://192.168.0.190:9701")

(def NETWORK_NAME "NNGeneratorNetwork")

(def MESSAGE_NAMESPACE_NAME "NNGenerator")

(def TEST_ELEMENT_NAME "test")

(def HEARTBEAT_ELEMENT_NAME "heartbeat")

(def TRAIN_ELEMENT_NAME "train")

(def RESPONSE_ELEMENT_NAME "response")

(def JXTA_HOME ".nn_cache")

(def NN_SERVER_PIPE_ID (PipeID/create
  (URI/create "urn:jxta:uuid-59616261646162614E504720503250338944BCED387C4A2BBD8E9411B78C284104")))

(defn getPipeAdvertisement []
  (let [adv (AdvertisementFactory/newAdvertisement (PipeAdvertisement/getAdvertisementType))]
    (doto adv
      (.setPipeID NN_SERVER_PIPE_ID)
      ;(.setPipeID (IDFactory/newPipeID PeerGroupID/defaultNetPeerGroupID))
      (.setType PipeService/UnicastType)
      (.setName "JxtaBiDiPipe tutorial"))))

(defn getNewPipeAdvertisement [name]
  (doto (AdvertisementFactory/newAdvertisement (PipeAdvertisement/getAdvertisementType))
    (.setPipeID (IDFactory/newPipeID PeerGroupID/defaultNetPeerGroupID))
    (.setType PipeService/UnicastType)
    (.setName name)))

(defn waitForRendezvous [netPeerGroup]
  (let [rdvService (.getRendezVousService netPeerGroup)]
    (while (not (.isConnectedToRendezVous rdvService))
      (println "waiting for rendezvous connection")
      (Thread/sleep 5000))))
