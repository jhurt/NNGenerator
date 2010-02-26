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
  com.jhurt.p2p.Master
  (:gen-class)
  (:require [com.jhurt.p2p.Jxta :as Jxta])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils]))

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg PipeAdvertisement)
  '(net.jxta.impl.protocol ModuleImplAdv)
  '(net.jxta.platform NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeID PipeService PipeMsgEvent PipeMsgListener)
  '(net.jxta.util JxtaBiDiPipe JxtaServerPipe)
  '(java.io File)
  '(java.util Enumeration))

(def serverPipe (ref nil))
(def registrate (ref false))
(def listen (ref false))
(def callback (ref nil))
(def pipes (ref ()))

(def manager (new NetworkManager NetworkManager$ConfigMode/ADHOC "Master"
  (.toURI (new File (new File ".nn_cache") "Master"))))

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
                (struct Jxta/InputMessage (str (.getPipeID source))
                  (.getElementName element) (str element) (System/currentTimeMillis)))
              elements)))))))

(defn registrarLoop [#^DiscoveryService discoveryService pipeAdv]
  (ThreadUtils/onThread
    #(while @registrate
      (let [waitTime 10000]
        (println "master publishing register pipe advertisement")
        (.publish discoveryService pipeAdv)
        (.remotePublish discoveryService pipeAdv)
        (println "master sleeping for: " waitTime " ms.\n")
        (Thread/sleep waitTime)))))

(defn acceptNewPeerConnection [#^JxtaBiDiPipe pipe]
  (if @listen
    (do
      (dosync (ref-set pipes (conj @pipes pipe)))
      (ThreadUtils/onThread
        #(do
          (println "JxtaBidiPipe accepted from " (.getPeerID (.getRemotePeerAdvertisement pipe)) "\n")
          (.setMessageListener pipe pipeMsgListener))))))

(defn pipeConnectionLoop [#^JxtaServerPipe serverPipe]
  (ThreadUtils/onThread
    #(while @listen
      (if-not (.isClosed serverPipe)
        (do
          (println "server waiting for peer connection\n")
          (acceptNewPeerConnection (.accept serverPipe)))
        (Thread/sleep 100)))))

(defn start [messageInCallback]
  (dosync (ref-set pipes ()))
  (dosync (ref-set callback messageInCallback))
  (.startNetwork manager)
  (let [netPeerGroup (.getNetPeerGroup manager)
        adv (Jxta/getPipeAdvertisement)
        discoveryService (.getDiscoveryService netPeerGroup)]
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
  (doall (map (fn [pipe] (do (.setMessageListener pipe nil) (.close pipe))) @pipes)))

(defn -main [] (start nil))
