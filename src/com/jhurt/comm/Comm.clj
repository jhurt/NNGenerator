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
  com.jhurt.comm.Comm)

(import
  '(javax.jms DeliveryMode ExceptionListener JMSException Message MessageListener Session TextMessage)
  '(org.apache.activemq ActiveMQConnection ActiveMQConnectionFactory))

(def SLAVES_QUEUE_NAME "SLAVE_QUEUE")
(def MASTER_QUEUE_NAME "MASTER_QUEUE")

(def TRAIN_XOR "trainXOR")
(def FINISH_TRAIN_XOR "finishTrainXOR")
(def HEARTBEAT "hb")

(defstruct Publisher :producer :session)
(defstruct Subscriber :consumer :replyProducer :session)

(def dfltExceptionListener (proxy [ExceptionListener] []
  (onException [#^JMSException exception]
    (println (str "JMS Exception for client " (.getMessage exception))))))

(defn getNewConnection [ip port]
  (let [url (str "failover://tcp://" ip ":" port)
    connectionFactory (new ActiveMQConnectionFactory ActiveMQConnection/DEFAULT_USER ActiveMQConnection/DEFAULT_PASSWORD url)]
    ;ActiveMQConnection/DEFAULT_BROKER_URL)
    (.createConnection connectionFactory)))

(defn getPublisher [connection queueName]
  (let [session (.createSession connection false Session/AUTO_ACKNOWLEDGE)
        destination (.createQueue session queueName)]
    (struct Publisher
      (doto (.createProducer session destination) (.setDeliveryMode DeliveryMode/PERSISTENT))
      session)))

(defn getSubscriber [connection queueName msgListener]
  (let [c (doto connection (.setExceptionListener dfltExceptionListener))
        session (.createSession c false Session/AUTO_ACKNOWLEDGE)
        destination (.createQueue session queueName)
        consumer (doto (.createConsumer session destination) (.setMessageListener msgListener))
        producer (doto (.createProducer session nil) (.setDeliveryMode DeliveryMode/NON_PERSISTENT))]
    (struct Subscriber consumer producer session)))

(defn heartbeat [clientId publisher]
  (let [producer (publisher :producer)
        session (publisher :session)
        m (doto (.createTextMessage session "hb") (.setStringProperty "name" HEARTBEAT) (.setStringProperty "clientId" clientId))]
    (.send producer m)))

(defn publishMessage
  "publish a string message"
  [publisher name content]
  (let [producer (publisher :producer)
        session (publisher :session)
        m (doto (.createTextMessage session content) (.setStringProperty "name" name))]
    (.send producer m)))

(defn close [endpoint] (.close endpoint))
