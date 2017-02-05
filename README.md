# scala_interface_ipopt
This project is to use the java interface of IPOPT https://projects.coin-or.org/Ipopt and call the optimization API through scala.

It requires the build of IPOPT and its JAVA interface:  https://www.coin-or.org/Ipopt/documentation/node16.html

It also requires add the java interface path to CLASSPATH
export CLASSPATH=<JavaInterfacePath>:$CLASSPATH

The static library libjipopt.so needs to be put under /lib folder to run the scala program. 

To build/run, follow the usual *scalac* and *scala* procedure. 

The code implements the HS071 example in the IPOPT JavaInterface examples. The HS071 problem definition can be found in Hock-Schittkowsky test suite
http://www.ai7.uni-bayreuth.de/test_problem_coll.pdf
