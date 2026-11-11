# ScalaJack

## Description
This project aims to develop a digital version of the popular card game **Blackjack**, implemented as a command-line interface (CLI) application using the Scala programming language.

The system is designed to support flexible gameplay sessions, allowing participation from human players as well as autonomous bots driven by automated behaviors. Built with a strong focus on rule compliance, the application ensures strict adherence to the traditional rules of Blackjack. This rigor is reflected in:

* **Accurate Score Calculation** and sequential game flow management.

* **Classic Gameplay Options**, including seamless support for hit, stand, double down, and split.

## Playing online
Start the server with Docker,run the client from the root of the repository executing the `.jar` file downloadable from the [latest release](../../releases) and follow the prompts (register, log in, sit at a table, play):
```bash
docker compose up --build
java -jar scalajack-client.jar
```
On Windows, run `chcp 65001` first, so that the terminal shows the suits of the cards.

## Failure demo
The failures described in the [decision record](DECISION_RECORD.md) can be played on the Docker deployment. Build the image then run the scenarios from the root of the repository:
```bash
docker compose build
sbt "demo/run"           # every scenario
sbt "demo/run crash"     # only one of them: crash, partition or backup
```
Each scenario starts the deployment afresh, sends a player to the table, kills a node or cuts it off the network and prints what it observes.

## Authors
[Nicholas Forlivesi](https://github.com/NicoForlivesi)<br>
[Elena Sarti](https://github.com/elena-sarti)
