<!-- PROJECT LOGO -->
<br />
<p align="center">

  <h3 align="center">Propositional Logic/First Order Logic AI Agent in Wumpus world</h3>

  <p align="center">
    Knowledge, Reasoning and Planning Challenges :: Wumpus Problem :: Fundamentals of AI 
    <br />
  </p>
  </br>
  <p align="center">
    <img src="images/wumpus-world.png" alt="original wumpus world map" width="350" height="350" />
  </p>
</p>

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li><a href="#about">About</a></li>
    <li><a href="#simulation">Simulation</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->

## About

|            AI Wumpus agent (Traditional map)            |            AI Wumpus agent (random map)            |
| :-----------------------------------------------------: | :------------------------------------------------: |
| ![Product Name Screen Shot](images/traditional_map.gif) | ![Product Name Screen Shot](images/random_map.gif) |

<div style="text-align: justify">
  
**Motivation**: Academic project for Fundamentals of Artificial Intelligence, M2AI. Use SWI-Prolog to create an AI agent for the Wumpus World game using Prepositional Logic/First-Order Logic.

**Implementation**: SWI-Prolog was used in order to build all the game logic and all the agent logic aswell. Later on was built a Pygame 2D game interface in order to evaluate the agent behavior. Pyswip (python package) was used to execute prolog queries through Python. There's also a mode where user can play the game. The agent possible actions are move-forward, turn-left, turn-right, grab-gold, climb-out, shoot(not at the moment/feature update). The agent sucess rate is about 70% on random maps.

</br>    
<p align="center">
    <img src="images/interface.gif" alt="wumpus world interface(pygame)" width="400" height="400" />
    
<p align="center"><em>Wumpus game interface without the fog (pygame)</em></p>

  </p>
  </br>

**_Tested with_** with random placed wumpus, pits and gold in the map (70% win).

**_Built With_** Pygame (game interface) and SWI-Prolog for the AI agent/Wumpus game logic.

<!-- SIMULATION -->

## Simulation

### Prolog

```prolog
consult('sr/prolog/main.pl'). % load prolog file

run(user). % cli for user to test the game
run(map). % agent simulation on traditional map
run. % agent simulation on a random map w/ logs
```

### Python

```python
pip install -r requirements.txt # pygame and pyswip

python main.py # agent simulation on a random map w/ interface
python main.py -user # playing mode for user to test the game
python main.py -map # run simulation on traditional map
```

## License

Distributed under the MIT License. See `LICENSE` for more information.

<!-- CONTACT -->

## Contact

Fábio Oliveira - [LinkedIn](https://www.linkedin.com/in/fabioo29/) - fabiodiogo29@gmail.com

Project Link: [https://github.com/fabioo29/ai-wumpus-world](https://github.com/fabioo29/ai-wumpus-world)  
Project built as a Msc. Applied Artificial Intelligence Student.
