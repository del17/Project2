# Project2

#https://docs.google.com/document/d/1eShukKEbx9Ug9WYU2wLlAkJ3rHSYixGzxMl6kh81o3w/edit

<b>Problem Definition:</b> </br>
As described in class Gossip type algorithms can be used both for group communication and for aggregate computation. The goal of this project is to determine the convergence of such algorithms through a simulator based on actors written in Erlang. Since actors are fully asynchronous, the particular type of Gossip implemented is the so-called Asynchronous Gossip.

<b>Gossip Algorithm for information propagation: </b>
</br>
- Starting: A participant(actor) told/sent a rumor (fact) by the main process</br>
- Step: Each actor selects a random neighbor and tells it the rumor.</br>
- Termination: Each actor keeps track of rumors and how many times he has heard the rumor. It stops transmitting once it has heard the rumor 10 times (10 is arbitrary, you can select other values).</br>

<b>Push-Sum algorithm for sum computation:</b>
</br>
- State: Each actor Ai maintains two quantities: s and w. Initially, s  = xi = i (that is actor number i has value i, play with other distribution if you so desire) and w = 1</br>
- Starting: Ask one of the actors to start from the main process.</br>
- Receive: Messages sent and received are pairs of the form (s, w). Upon receiving, an actor should add the received pair to its own corresponding values. Upon receiving, each actor selects a random neighbor and sends it a message.</br>
- Send: When sending a message to another actor, half of s and w is kept by the sending actor, and half is placed in the message.</br>
- Sum Estimate: At any given moment of time, the sum estimate is s/w where s and w are the current values of an actor.</br>
- Termination: If an actor's ratio s/w did not change more than 10−10 in 3 consecutive rounds the actor terminates. WARNING: the values s and w independently never converge, only the ratio does.</br>

<b>Topologies:</b> 
</br>
The actual network topology plays a critical role in the dissemination speed of Gossip protocols. As part of this project, you have to experiment with various topologies. The topology determines who is considered a neighbor in the above algorithms. </br>

- Full Network: Every actor is a neighbor of all other actors. That is, every actor can talk directly to any other actor.
- 2D Grid: Actors form a 2D grid. The actors can only talk to the grid neighbors
- Line: Actors are arranged in a line. Each actor has only 2 neighbors (one left and one right, unless you are the first or last actor).
- Imperfect 3D Grid: Grid arrangement but one random other neighbor is selected from the list of all actors (8+1 neighbors).
</br>
<b> Input: </b> 
The input provided (as a command line to your project2) will be of the form:</br>
project2 numNodes topology algorithm</br>
Where, numNodes is the number of actors involved (for 2D-based topologies you can round up until you get a square), topology is one of full, 2D, line, imp2D, the algorithm is one of gossip, push-sum.
</br>
</br>
<b> Output:</b> 
Print the amount of time it took to achieve convergence of the algorithm. 
