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

(ns com.jhurt.p2p.PipeClient
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

(def pipe (ref nil))

(defn sendHeartbeat [#^JxtaBiDiPipe pipe]
  (let [strMsgElement (new StringMessageElement Jxta/HEARTBEAT_ELEMENT_NAME "heartbeat" nil)
        msg (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME strMsgElement))]
    (.sendMessage pipe msg)))

(defn sendResponse [pipe]
  (if-not (nil? pipe)
    (let [respElement (new StringMessageElement Jxta/RESPONSE_ELEMENT_NAME "client_response_message" nil)
          response (doto (new Message) (.addMessageElement Jxta/MESSAGE_NAMESPACE_NAME respElement))]
      (.sendMessage pipe response))))

(def clientPipeMsgListener (proxy [PipeMsgListener] []
  (pipeMsgEvent [#^PipeMsgEvent event]
    (let [msg (.getMessage event)
          msgElement (.getMessageElement msg Jxta/MESSAGE_NAMESPACE_NAME Jxta/TEST_ELEMENT_NAME)
          currentThreadName (.getName (Thread/currentThread))]
      (println "client thread " currentThreadName " received message: " msg "\nmsg element: " msgElement)))))

(defn heartbeat []
  (while (not (nil? @pipe))
    (do
      (sendHeartbeat @pipe)
      (Thread/sleep 30000))))

(defn run [netPeerGroup]
  (let [connectPipe (Jxta/getPipeAdvertisement)]
    (println "Attempting to establish a connection to: " (.getPipeID connectPipe))
    (dosync (ref-set pipe (new JxtaBiDiPipe netPeerGroup   connectPipe 20000 clientPipeMsgListener true)))
    (println "JxtaBiDi pipe created\n\n")
    (heartbeat)))

(defn -main []
  (let [home (new File (new File ".nn_cache") "client")
        manager (new NetworkManager NetworkManager$ConfigMode/ADHOC Jxta/NETWORK_NAME (.toURI home))]
    (.startNetwork manager)
    (let [netPeerGroup (.getNetPeerGroup manager)]
      (.waitForRendezvousConnection manager 0)
      (run netPeerGroup))))
  