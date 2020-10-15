
<br />
<p align="center">

  <h3 align="center">neuroevolution simulation</h3>
  <h5 align="center">Hananel Assor, Dor Avni</h5>

  <p align="center">
    Out final project on 'Concurrent and Distributed Programming' course, Ben Gurion University of the Negev.
    <br />
  </p>
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
modular neuroevolution simulation using erlang proccesses as neurons  

* using gen_statem OTP for neurons
* using Graphwiz package

### Built With
* [Erlang](https://www.erlang.org/) - version 23


<!-- GETTING STARTED -->
## Getting Started


install graphwiz package from graphwiz.org
Open folder and terminal for the 'master' node compile the files and run the program from master



```sh
erl -name master
make:all().
master:run().
```

<h3>Submaster: </h3>
Do the next stages for every other node, every node will be a population of NN's.

start erlang shell and isert the node name
```sh
erl -sname nodex
```
if you use erlang long name:
```sh
erl -name nodex@IP -setcookie bsp
```
compile all files if its another computer:
```sh
make:all().
```
no need to make startlink in the nodes its will be commit by the master using rpc


<h3>Master: </h3>

if you use erlang short name, start erlang:
```sh
erl -sname master
```

if you use erlang long name:
```sh
erl -name master@IP -setcookie bsp
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
* choose resolution.
* choose number of sensors, neurons, actuators for initial NN. 
* Insert the short/long names of all the nodes separated by comma .
* Click 'start'
* if there no connection problem the simulation will start.
<br />
<br />

![GitHub Logo](/1.jpg)
Format: ![Alt Text](url)


<!-- CONTACT -->
## Contact

hananel assor - assorh [at ] post.bgu.ac.il

dor avni - avnido [at ] post.bgu.ac.il


