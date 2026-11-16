# ScalaJack

## Description
ScalaJack is the game of **Blackjack** played online from the command line, written in Scala 3 with scores
computed by a Prolog theory. It started as a local game for one terminal and is now a distributed system: players
log in from their own client and sit at shared tables.

## Playing online
The only requirements are Docker and Java 17 (or later).
Start the server, then run the client on a UTF-8 terminal (Linux, macOS or **WSL2** on Windows). The client is downloadable from the
[latest release](../../releases):
```bash
docker compose up --build
java -jar scalajack-client.jar
```
The client asks to register or log in, then shows the lobby: a player can either deposit some fiches (a new account has none, and a table
needs at least the minimum bet of 5), withdraw some fiches or sit at a table and play. Once the player is seated at the table, he has 30 seconds for every decision: if he does not
answer the client follows a default behavior, depending on which is the situation. 

### Running from the prebuilt image
The [latest release](../../releases) contains everything needed to play without cloning the repository:
`scalajack-backend.tar.gz` (the Docker image of the server), `docker-compose.yml` and `scalajack-client.jar`.
Put the three files in the same folder, then:
```bash
docker load < scalajack-backend.tar.gz
docker compose up
java -jar scalajack-client.jar
```
Do not add `--build`: the image is already loaded and Compose uses it as it is.

### Playing from different machines
By default the tables announce `localhost` to the clients, so everyone must play on the machine that runs Docker.
To play from other computers on the same network:
1. on the machine that runs Docker, create a `.env` file next to `docker-compose.yml` and set `PUBLIC_HOST` in it to its IP address on the network (on Windows
   `ipconfig` shows it as IPv4 Address) and start the server;
2. run the client with the address of the lobby (it can also be set with the environment variable `SCALAJACK_LOBBY`):
```bash
java -jar scalajack-client.jar http://<server-ip>:9080
```
On the machine that runs Docker the client also works without arguments, as in a local run.
The ports 9080, 9091-9093 and 9191-9193 of the server machine must be reachable.

### More tables
This step is optional, only
to see more tables: the lobby can only start containers that already exist, and `docker compose up` creates only the container of `table-1` and of its backup. When `table-1` is full, the lobby starts `table-2`, then `table-3`, each with its backup. Create the containers for `table-2` and `table-3` once, from the folder containing `docker-compose.yml`, after the image has been built or loaded:
```bash
docker compose --profile pool up --no-start
```
Without it the deployment works the same with one table, and the eighth player is told the server is full.

### Configuration
Nothing has to be configured for a local run. A `.env` file overrides the defaults:
`PUBLIC_HOST`, the database credentials, the secret that signs the session tokens (`SESSION_SECRET`), the heartbeat period and timeout
and how long a promoted node waits for the players of the failed primary (`RETURN_WINDOW_SECONDS`). Changing the
database password after the first start needs a new database: `docker compose down -v`.

## Tests
These commands need the source code and sbt, so they must be run from a clone of the repository.
```bash
sbt test                          # unit and integration tests, no Docker needed
sbt "demo/runMain demo.EndToEnd"  # end-to-end, on the Docker deployment started from zero
```
The end-to-end tests delete the local database, start every node again and use the system as the client does:
registration, login, deposit and withdrawal, a whole hand checked against the rules, two players at the same table,
forged tokens.

## Failure demo
Possible failure scenarios, such as the crash of the primary node that runs a table, a network partition or a crash of the backup can be played on the Docker deployment:
```bash
docker compose build
sbt "demo/run"           # every scenario
sbt "demo/run crash"     # only one of them: crash, partition or backup
```
Each scenario starts the deployment afresh, sends a player to the table, kills a node or cuts it off the network and
prints what it observes.

## Modules
- `model`: the rules of the game, from the non-distributed version.
- `protocol`: the messages between clients and nodes, as JSON.
- `server`: the lobby, the database, table nodes and their backup.
- `client`: the terminal client, and the local game of the non-distributed version (`sbt "client/runMain controller.GameController"`).
- `demo`: the failure demo and the end-to-end tests.

## Authors
[Nicholas Forlivesi](https://github.com/NicoForlivesi)<br>
[Elena Sarti](https://github.com/elena-sarti)
