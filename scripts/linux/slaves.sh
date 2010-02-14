#!/bin/bash
#!/bin/bash
for (( i=1;i<=$1;i+=1 )) ;
do java -cp nn.jar:bcprov-jdk14.jar:javax.servlet.jar:jxta.jar:org.mortbay.jetty.jar:clojure.jar com.jhurt.p2p.SlaveR &
done;


