# ScalaJack

## Description
This project aims to develop a digital version of the popular card game **Blackjack**, implemented as a command-line interface (CLI) application using the Scala programming language.

The system is designed to support flexible gameplay sessions, allowing participation from human players as well as autonomous bots driven by automated behaviors. Built with a strong focus on rule compliance, the application ensures strict adherence to the traditional rules of Blackjack. This rigor is reflected in:

* **Accurate Score Calculation** and sequential game flow management.

* **Classic Gameplay Options**, including seamless support for hit, stand, double down, and split.
## How to run

In order to run the game, download the `.jar` file from the [latest release](../../releases) and execute it, for example running the following command via CLI:<br>
```bash
java -jar pps-scalajack.jar
```

## Playing online
Start the server with Docker (`docker compose up --build`), then run the client from the root of the repository and follow the prompts (register, log in, sit at a table, play):
```bash
sbt client/run                                  # the lobby of the Docker deployment, http://localhost:9080
sbt "client/run http://another-host:9080"       # another lobby
```
On Windows, run `chcp 65001` first, so that the terminal shows the suits of the cards. Every player runs their own client. The local game of the first version is still there: `sbt "client/runMain controller.GameController"`.

## Failure demo
The failures described in the [decision record](DECISION_RECORD.md) can be played on the Docker deployment. Build the image once, then run the scenarios from the root of the repository:
```bash
docker compose build
sbt "demo/run"           # every scenario
sbt "demo/run crash"     # only one of them: crash, partition or backup
```
Each scenario starts the deployment afresh, sends a player to the table, kills a node or cuts it off the network, and prints what it observes.

## Documentation
The project documentation is visible at [(./docs/report)](https://nicoforlivesi.github.io/PPS-26-scalajack/).

## Authors
[Nicholas Forlivesi](https://github.com/NicoForlivesi)<br>
[Elena Sarti](https://github.com/elena-sarti)
