## Bitcoin Mining Using Erlang

This project utilises actor model architecture for simulating bitcoin mining.

#### Team members

1. Bhagya Raj Varadaraju, UFID: 60212561, varadaraju.b@ufl.edu
2. Sharath Kumar Raju Addepalli, UFID: 33589528, s.addepalli@ufl.edu

#### Architecture






#### Answers

1. Size of the work unit that you determined results in the best performance for your implementation and an explanation
   of how you determined it. The size of the work unit refers to the number of sub-problems that a worker gets in a
   single request from the boss.


2. The result of running your program for input 4\
Check the below screenshot for the output with K = 4
   
3. The running time for the above is reported by time for the above and report the time. The ratio of CPU time to REAL
   TIME tells you how many cores were effectively used in the computation. If you are close to 1 you have almost no
   parallelism (points will be subtracted).\
No of cores effectively used in the computation is approximately equal to 6.

4. The coin with the most 0s you managed to find.\
We were able to mine a bitcoin with 6 leading zeroes. See the below output:
   

5. The largest number of working machines you were able to run your code with.\
We ran our code with two nodes i.e, two erlang systems. 