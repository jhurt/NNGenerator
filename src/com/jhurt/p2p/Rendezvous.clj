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

(ns com.jhurt.p2p.Rendezvous
  (:gen-class)
  (:require [com.jhurt.p2p.Jxta :as Jxta])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils]))

(import
  '(net.jxta.discovery DiscoveryEvent DiscoveryListener DiscoveryService)
  '(net.jxta.document Advertisement AdvertisementFactory)
  '(net.jxta.peergroup PeerGroup PeerGroupID)
  '(net.jxta.protocol DiscoveryResponseMsg ModuleImplAdvertisement PeerGroupAdvertisement PipeAdvertisement)
  '(net.jxta.impl.protocol ModuleImplAdv)
  '(net.jxta.platform ModuleSpecID NetworkConfigurator NetworkManager NetworkManager$ConfigMode)
  '(net.jxta.id IDFactory)
  '(net.jxta.pipe PipeID PipeService PipeMsgEvent PipeMsgListener)
  '(net.jxta.util JxtaBiDiPipe JxtaServerPipe)
  '(net.jxta.rendezvous RendezvousEvent RendezvousListener RendezVousService)
  '(java.io File)
  '(java.util Enumeration)
  '(java.net.URI))

(def defaultRendezvousListener (proxy [RendezvousListener] []
  (rendezvousEvent [#^RendezvousEvent event]
    (println "Rendezvous event: " (str event)))))

(defn configureRdvNode []
  (let [seedingURI (URI/create "tcp://70.180.196.124:9701")]
    (doto (new NetworkConfigurator)
      (.setHome (new File (Jxta/JXTA_HOME)))
      (.setUseMulticast false)
      (.addSeedRelay seedingURI)
      (.addSeedRendezvous seedingURI)
      (.addRdvSeedingURI seedingURI)
      (.addRelaySeedingURI seedingURI)
      (.setMode (+ NetworkConfigurator/RDV_SERVER NetworkConfigurator.RELAY_SERVER))
      ;    (.setUseOnlyRelaySeeds true)
      ;    (.setUseOnlyRendezvousSeeds true)
      (.setTcpEnabled true)
      (.setTcpIncoming true)
      (.setTcpOutgoing true)
      (.save))))
