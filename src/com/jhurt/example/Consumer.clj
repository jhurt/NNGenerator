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

(defn createConnection [clientId exceptionListener]
  (let [user ActiveMQConnection/DEFAULT_USER
        password ActiveMQConnection/DEFAULT_PASSWORD
        url ActiveMQConnection/DEFAULT_BROKER_URL]
    (doto (.createConnection (new ActiveMQConnectionFactory user password url))
      (.setClientID clientId)
      (.setExceptionListener exceptionListener))))

(defn start [clientId]
  (dosync (ref-set consumer (struct Consumer clientId)))
  (let [connection (createConnection clientId dfltExceptionListener)]
    (.start connection)
    (let [session (.createSession connection false Session/AUTO_ACKNOWLEDGE)
          destination (.createQueue session "TestSubject")
          replyProducer (.createProducer session nil)
          consumer (.createConsumer session destination)]
      (.setDeliveryMode replyProducer DeliveryMode/NON_PERSISTENT)
      (.setMessageListener consumer dfltMessageListener))))
