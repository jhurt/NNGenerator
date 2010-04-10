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
  com.jhurt.example.Consumer
  (:gen-class))

(import
  '(javax.jms DeliveryMode ExceptionListener JMSException
    Message MessageListener Session TextMessage)
  '(org.apache.activemq ActiveMQConnection ActiveMQConnectionFactory))

(defstruct Consumer :clientId)

(def consumer (ref nil))

(def dfltExceptionListener (proxy [ExceptionListener] []
  (onException [#^JMSException exception]
    (println "JMS Exception for client " (@consumer :clientId)))))

(def dfltMessageListener (proxy [MessageListener] []
  (onMessage [#^Message message]
    (println "Received msg: " (.getText message)))))

;        if (message.getJMSReplyTo() != null) {
;                replyProducer.send(message.getJMSReplyTo(), session.createTextMessage("Reply: " + message.getJMSMessageID()));
;            }

(defn createConnection [clientId exceptionListener]
  (let [user ActiveMQConnection/DEFAULT_USER
        password ActiveMQConnection/DEFAULT_PASSWORD
        url ActiveMQConnection/DEFAULT_BROKER_URL]
  (doto (.createConnection (new ActiveMQConnectionFactory user password url))
    (.setClientId clientId)
    (.setExceptionListener exceptionListener))))

(defn start [clientId]
  (dosync (ref-set consumer (struct Consumer clientId)))
  (let [connection (doto (createConnection clientId dfltExceptionListener) (.start))
        session (.createSession connection false Session/AUTO_ACKNOWLEDGE)
        destination (.createQueue "TestSubject")
        replyProducer (doto (.createProducer session nil) (.setDeliveryMode DeliveryMode/NON_PERSISTENT))
        consumer (.createConsumer session destination)]
    (.setMessageListener consumer dfltMessageListener)))


;
;
;(def manager (let [r (rand-int Integer/MAX_VALUE)
;                   peerName (str "Slave" r)]
;  (new NetworkManager NetworkManager$ConfigMode/EDGE peerName
;    (.toURI (new File (new File Jxta/JXTA_HOME) peerName)))))
;
;(def netPeerGroup (ref nil))
;
;(def pipe (ref nil))
;(def discoveryService (ref nil))
;
;(defn sendMessageToMaster [elementName msg]
;  (if-not (nil? @pipe)
;    (let [strMsgElement (new StringMessageElement elementName msg nil)]
;      (.sendMessage @pipe
;        (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME strMsgElement))))))
;
;(defn trainNetworkCallback [weights error generation layers alpha gamma]
;  (let [; this is a small hack because serializing NaN and deserializing it will result
;        ; in a symbol instead of Double/NaN, so we send 1.0 in the case of NaN
;        ; this should ideally be fixed in Clojure at some point
;        e (if (> error 0.0) error 1.0)
;        msg {:weights weights :error e :generation generation :layers layers :alpha alpha :gamma gamma}]
;    (sendMessageToMaster Jxta/FINISH_TRAIN_XOR_ELEMENT_NAME (serialize msg))))
;
;;a multimethod for handling incoming messages
;;the dispatch function is the name of the message element
;(defmulti handleIncomingMessage (fn [msgElem] (.getElementName msgElem)))
;
;(defmethod handleIncomingMessage Jxta/TRAIN_XOR_ELEMENT_NAME [msgElem]
;  (println "\n\nslave received train xor msg: " (str msgElem))
;  (let [msg (deserialize (str msgElem))]
;    (XOR/train (msg :layers) (msg :training-cycles) (msg :generation) (msg :alpha) (msg :gamma) trainNetworkCallback)))
;
;(def pipeMsgListener (proxy [PipeMsgListener] []
;  (pipeMsgEvent [#^PipeMsgEvent event]
;    (let [msg (.getMessage event)
;          msgElems (iterator-seq (.getMessageElements msg))]
;      (doall (map handleIncomingMessage msgElems))))))
;
;;attempt to create a bidirectional pipe based on the pipe advertisement
;(defn createPipeFromAdv [#^PipeAdvertisement adv]
;  (try
;    (if (or (nil? @pipe) (not (.isBound @pipe)))
;      (dosync (ref-set pipe (new JxtaBiDiPipe @netPeerGroup adv Integer/MAX_VALUE pipeMsgListener true))))
;    (catch IOException ioe (dosync ref-set pipe nil))))
;
;;send a heartbeat message along the pipe
;(defn sendHeartbeat [#^JxtaBiDiPipe pipe]
;  (let [strMsgElement
;        (new StringMessageElement Jxta/HEARTBEAT_ELEMENT_NAME (str (.getPeerID manager)) nil)
;        msg
;        (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME strMsgElement))]
;    (.sendMessage pipe msg)))
;
;;loop as long as a pipe is available, sending a heartbeat message every 2 minutes
;(defn heartbeat []
;  (ThreadUtils/onThread
;    #(while (and (not (nil? @pipe)) (.isBound @pipe))
;      (do
;        (try
;          (sendHeartbeat @pipe)
;          (catch IOException ioe (dosync ref-set pipe nil)))
;        (Thread/sleep 120000)))))
;
;;look for a pipe adv from a seq of advertisements
;;for the first one that is found, a bidirectional pipe
;;is created and a heartbeat is started to the other end
;(defn findPipeAdv [advs]
;  (doall (map (fn [adv]
;    (do (println "advertisement type: " (.getAdvType adv) "\n\n")
;      (if (instance? PipeAdvertisement adv)
;        (do
;          (createPipeFromAdv adv)
;          (heartbeat))))) advs)))
;
;;called whenever Advertisement responses are received from remote peers by the Discovery Service.
;(def defaultDiscoveryListener (proxy [DiscoveryListener] []
;  (discoveryEvent [#^DiscoveryEvent event]
;    (let [response (.getResponse event)
;          peerAdv (.getPeerAdvertisement response)
;          remoteAdvertisements (.getAdvertisements response)
;          localAdvertisements
;          (.getLocalAdvertisements @discoveryService DiscoveryService/ADV nil nil)]
;      ;(println "slave received discovery response from node: " (.getSource event) "\n")
;      (findPipeAdv (enumeration-seq remoteAdvertisements))))))
;
;(defn registrationLoop [#^DiscoveryService discoveryService]
;  (ThreadUtils/onThread
;    #(while true
;      (if (or (nil? @pipe) (not (.isBound @pipe)))
;        (.getRemoteAdvertisements discoveryService nil DiscoveryService/ADV nil nil 10000 defaultDiscoveryListener))
;      (Thread/sleep 60000))))
;
;(defn configureSlaveNode [rdvUri]
;  (let [seedingURI (URI/create rdvUri)
;        port (Integer/parseInt (last (.split rdvUri ":")))]
;    (doto (.getConfigurator manager)
;      (.setHome (new File Jxta/JXTA_HOME))
;      (.setUseMulticast false)
;      (.addSeedRelay seedingURI)
;      (.addSeedRendezvous seedingURI)
;      (.addRdvSeedingURI seedingURI)
;      (.addRelaySeedingURI seedingURI)
;      (.setUseOnlyRelaySeeds true)
;      (.setUseOnlyRendezvousSeeds true)
;      (.setTcpEnabled true)
;      (.setTcpIncoming false)
;      (.setTcpOutgoing true)
;      (.save))))
;
;(defn -main [rdvUri]
;  (configureSlaveNode rdvUri)
;  (.startNetwork manager)
;  (println "*************************starting slave node*************************\n")
;  (dosync (ref-set netPeerGroup (.getNetPeerGroup manager)))
;  (Jxta/waitForRendezvous @netPeerGroup)
;  (dosync (ref-set discoveryService (.getDiscoveryService @netPeerGroup)))
;  (registrationLoop @discoveryService))
