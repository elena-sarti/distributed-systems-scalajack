# ScalaJack

## Description
ScalaJack is the game of **Blackjack** played online from the command line, written in Scala 3 with the scores
computed by a Prolog theory. It started as a local game for one terminal and is now a distributed system: players
log in from their own client and sit at shared tables, a table survives the failure of the node that plays it.

The rules of the game are strict: dealer standing at 17, blackjack paying 3:2, insurance, double down and split.

## Architecture
- **Lobby**: registration and login (session tokens), the balance of every player (deposit and withdrawal) and the
  table to sit at. It knows which tables are up from their heartbeats and starts a new table when all are full.
- **Tables**: each table is played by a **primary** node, which talks to the clients over WebSocket and copied by a
  **backup** node. Every move is stored on the backup before the players see it: if the primary fails, the backup takes
  its place and the hand goes on from where it was. The table prefers consistency to availability (CP): when a move
  cannot be copied, the table pauses rather than risk two diverging hands.
- **Database** (PostgreSQL): the users and their balances.
- **Client**: the terminal game. It finds its table again by itself after a failure.

The choices behind it, and the alternatives discarded, are in the [decision record](DECISION_RECORD.md).

## Playing online
The only requirement is Docker. Start the server, then run the client, downloadable from the
[latest release](../../releases) (it needs Java 17 or later):
```bash
docker compose up --build
java -jar scalajack-client.jar
```
By default the tables announce `localhost` to the clients, so everyone must play on the machine that runs Docker.
To play from other computers on the same network, set `PUBLIC_HOST` in `.env` to the IP address of the server
machine, start it with `docker compose up --build`, make sure ports 9080, 9091-9093 and 9191-9193 are reachable and give the lobby address to the client:
```bash
java -jar scalajack-client.jar http://<server-ip>:9080
```
The address can also be set with the environment variable `SCALAJACK_LOBBY`.

The client asks to register or log in, then shows the lobby: deposit some fiches (a new account has none, and a table
needs at least the minimum bet of 5), sit at a table and play. Every decision has 30 seconds, a player who does not
answer follows a default behavior (ask until 12 then stand). On Windows, run `chcp 65001` first, so that the terminal shows the suits of the cards.

### More tables
To play, `docker compose up --build` is all it takes: it starts one table, `table-1` with its backup, which seats 7
players. When it is full the lobby starts `table-2`, then `table-3`, each with its backup. This step is optional, only
to see more tables: the lobby can only start containers that already exist, and Compose does not create these ones
with `up` (they belong to the profile `pool`, so they do not run from the start). Create them once from the
root of the repository after the image has been built:
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
```bash
sbt test                          # unit and integration tests, no Docker needed
sbt "demo/runMain demo.EndToEnd"  # end-to-end, on the Docker deployment started from zero
```
The end-to-end tests delete the local database, start every node again and use the system as the client does:
registration, login, deposit and withdrawal, a whole hand checked against the rules, two players at the same table,
forged tokens.

## Failure demo
The failures described in the decision record can be played on the Docker deployment:
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
- `server`: lobby, table and backup nodes (one jar, the role is chosen by `NODE_ROLE`).
- `client`: the terminal client, and the local game of the non-distributed version (`sbt "client/runMain controller.GameController"`).
- `demo`: the failure demo and the end-to-end tests.

## Authors
[Nicholas Forlivesi](https://github.com/NicoForlivesi)<br>
[Elena Sarti](https://github.com/elena-sarti)
