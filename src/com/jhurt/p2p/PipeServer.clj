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

(ns com.jhurt.p2p.PipeServer
  (:gen-class)
  (:use [com.jhurt.p2p.Jxta :as Jxta]))

(import
  '(net.jxta.document AdvertisementFactory)
  '(net.jxta.endpoint Message MessageElement StringMessageElement)
  '(net.jxta.logging Logging)
  '(net.jxta.peergroup PeerGroup)
  '(net.jxta.pipe PipeID PipeMsgEvent PipeMsgListener PipeService)
  '(net.jxta.protocol PipeAdvertisement)
  '(net.jxta.platform NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.util JxtaBiDiPipe JxtaServerPipe)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeService)
  '(java.io File)
  '(java.net URI))

(defn- onThread [f]
  (doto (Thread. #^Runnable f)
    (.start)))

(def pipeMsgListener (proxy [PipeMsgListener] []
  (pipeMsgEvent [#^PipeMsgEvent event]
    (let [msg (.getMessage event)
          responseMsgElement (.getMessageElement msg Jxta/MESSAGE_NAMESPACE_NAME Jxta/RESPONSE_ELEMENT_NAME)
          heartbeatElement (.getMessageElement msg Jxta/MESSAGE_NAMESPACE_NAME Jxta/HEARTBEAT_ELEMENT_NAME)
          currentThreadName (.getName (Thread/currentThread))]
      (println "server thread " currentThreadName " received message: " msg)
      (if-not (nil? responseMsgElement) (println "response: " (str responseMsgElement)))
      (if-not (nil? heartbeatElement) (println "heartbeat: " (str heartbeatElement)))))))

(defn sendTestMessage [#^JxtaBiDiPipe pipe data]
  (let [strMsgElement (new StringMessageElement Jxta/TEST_ELEMENT_NAME data nil)
        msg (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME strMsgElement))]
    (.sendMessage pipe msg)))

(defn acceptNewPeerConnection [#^JxtaBiDiPipe pipe]
  (onThread #(do
    (println "JxtaBidiPipe accepted from " (.getPeerID (.getRemotePeerAdvertisement pipe)) "\n")
    (.setMessageListener pipe pipeMsgListener)
    (sendTestMessage pipe "TESTMESSAGETESTMESSAGEDFJDFLJFDJLFDJLS"))))

(defn serverLoop [#^JxtaServerPipe serverPipe]
  (onThread
    #(when-not (.isClosed serverPipe)
      (println "server waiting for peer connection\n")
      (acceptNewPeerConnection (.accept serverPipe))
      (recur))))

(defn -main []
  (let [home (new File (new File ".nn_cache") "server")
        manager (new NetworkManager NetworkManager$ConfigMode/ADHOC Jxta/NETWORK_NAME (.toURI home))]
    (.startNetwork manager)
    (let [netPeerGroup (.getNetPeerGroup manager)
          adv (Jxta/getPipeAdvertisement)
          serverPipe (doto (new JxtaServerPipe netPeerGroup adv) (.setPipeTimeout 0))]
      (println "Waiting for JxtaBidiPipe connections on JxtaServerPipe : " (.getPipeID adv) "\n")
      (serverLoop serverPipe))))
