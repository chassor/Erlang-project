
<br />
<p align="center">

  <h3 align="center">neuroevolution simulation</h3>
  <h5 align="center">Hananel Assor, Dor Avni</h5>

  <p align="center">
    Out final project on 'Concurrent and Distributed Programming' course, Ben Gurion University of the Negev.
    <br />
  </p>




<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Contact](#contact)



<!-- ABOUT THE PROJECT -->
## About The Project
modular neuroevolution simulation using erlang processes as neurons  

* using gen_statem OTP for neurons
* using gen_server OTP for master
* using wx widegt for gui 
* using Graphwiz package

### Built With
* [Erlang](https://www.erlang.org/) - version 23



<!-- GETTING STARTED -->
## Getting Started


install graphwiz package from graphwiz.org.
Open src folder and ubuntu terminal for running the 'master' node.
compile the files and run the program from master


in terminal, go to src folder and then:
```sh
erl -name master
```

in case of running heavy simulations its possible to increase the atom table size,
the default is max=1048576, and its often not enough for heavy simulations.
by adding +t #?max_number like this:

```sh
erl +t ?SOME_NUMBER -name master/node1
```

to run the program in erlang shell:
```sh
make:all().
master:run().
```

<h3>populations: </h3>
Do the next stages for every other node. each node will be a population of NN's.


start erlang shell and insert the node name

```sh
erl -sname nodeX
```
if you use erlang long name:
```sh
erl -name nodex@IP -setcookie Some_cookie
```
compile all files if its another computer:
```sh
make:all().
```
no need to make startlink in the nodes its will be commit by the master using rpc.
just need to make sure the connection is ok.


<h3>Master: </h3>

if you use erlang short name, start erlang:
```sh
erl -sname master
```

if you use erlang long name:
```sh
erl -name master@IP -setcookie Some_cookie
```
compile all file:
```sh
make:all().
```
start the program:
```sh
master:run().
```


<!-- USAGE EXAMPLES -->
## Usage
<br />

* choose fitness func</b>. 
* choose resolution of simulation.
* choose number of sensors, neurons, actuators for initial NN. 
* Insert the short/long names of all the nodes separated by (,) .
* Click 'start'
* if there are no connection problems the simulation will start.
* you can stop the simulation and run another one with different values
<br />
<br />




<!-- CONTACT -->
## Contact

hananel assor - assorh [at ] post.bgu.ac.il

dor avni - avnido [at ] post.bgu.ac.il


