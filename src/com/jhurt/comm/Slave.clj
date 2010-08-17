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
  com.jhurt.comm.Slave
  (:gen-class)
  (:require [com.jhurt.nn.trainer.XOR :as XOR])
  (:require [com.jhurt.ThreadUtils :as ThreadUtils])
  (:require [com.jhurt.comm.Comm :as Comm])
  (:use [com.jhurt.Serialization]))

(import
  '(javax.jms MessageListener))

(def myName (ref nil))
(def masterPublisher (ref nil))
(def slavesSubscriber (ref nil))
(def doHeartbeat (ref nil))

;loop as long as a pipe is available, sending a heartbeat message every 2 minutes
(defn heartbeatLoop []
  (dosync (ref-set doHeartbeat true))
  (ThreadUtils/onThread
    #(while @doHeartbeat
      (do
        (Comm/heartbeat @myName @masterPublisher)
        (Thread/sleep 60000)))))

(defn trainNetworkCallback
  "called after a network has been trained by a trainer"
  [weights error generation layers alpha gamma]
  (let [; this is a small hack because serializing NaN and deserializing it will result
        ; in a symbol instead of Double/NaN, so we send 1.0 in the case of NaN
        ; this should ideally be fixed in Clojure at some point
        e (if (> error 0.0) error 1.0)
        msg {:weights weights :error e :generation generation :layers layers :alpha alpha :gamma gamma}]
    (println "\n***Slave " @myName " TRAINED NETWORK for generation: " generation ",error=" e ",alpha=" alpha ",gamma=" gamma)
    (Comm/publishMessage @masterPublisher Comm/FINISH_TRAIN_XOR (serialize msg))))

;a multimethod for handling incoming messages
;the dispatch function is the name of the message
(defmulti handleIncomingMessage (fn [msg] (.getStringProperty msg "name")))

(defmethod handleIncomingMessage Comm/TRAIN_XOR
  [msg]
  ;(println "\n\nslave received train xor msg: " (.getText msg))
  (let [trainMsg (deserialize (.getText msg))]
    (XOR/train
      (trainMsg :layers) (trainMsg :training-cycles) (trainMsg :generation) (trainMsg :alpha) (trainMsg :gamma) trainNetworkCallback)))

(def dfltMessageListener (proxy [MessageListener] []
  (onMessage [message]
    (handleIncomingMessage message))))

(defn start [slaveName brokerIp brokerPort]
  (let [connection (doto (Comm/getNewConnection brokerIp brokerPort) (.setClientID slaveName))]
    (.start connection)
    (dosync
      (ref-set myName slaveName)
      (ref-set masterPublisher (Comm/getPublisher connection Comm/MASTER_QUEUE_NAME))
      (ref-set slavesSubscriber (Comm/getSubscriber connection Comm/SLAVES_QUEUE_NAME dfltMessageListener)))
    (heartbeatLoop)))

(defn -main [& args]
  (start (nth args 0) (nth args 1) (nth args 2)))
