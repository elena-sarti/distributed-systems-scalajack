# Decision record: table replication and the CAP theorem

The choices that drive the replication of the tables' state: behaviour under partition (CP or AP), the promotion
policy of the backup and the role of the lobby as arbiter. Each one comes with the alternatives that were discarded
and the reason why.

## Context

Each table is a node that keeps the state of the hand in memory: cards, bets, turn, balances of the seated players.
If the node goes down, the hand in progress is lost. To tolerate the failure of a node, each table has two replicas:
a **primary**, which plays with the clients, and a **backup**, ready to take its place.

With two copies of the state we must decide what happens when the replicas cannot talk to each other. By the CAP
theorem, under a network partition a system has to choose between **consistency** (everybody sees the same state,
possibly by not answering) and **availability** (it always answers, possibly with diverging states).

The system already has a lobby that receives the tables' heartbeats and knows their status (the table registry), and
that can act as a third party in the decisions.

## Decision

### 1. The table is CP

The state of a table favours **consistency**: if a move cannot be replicated, the table refuses it and the hand is
paused until the situation is clear.

A blackjack hand has cards already shown and money already moved. Two primaries active at the same time
(*split brain*) would deal different cards and pay different bets, and there is no sensible way to merge two
diverging hands. A pause of a few seconds, on the other hand, is acceptable: the game goes at the pace of people
thinking about their moves.

### 2. Passive, synchronous replication

- **Passive**: only the primary executes the moves; after every transition it sends the new state of the table to the
  backup.
- **Synchronous**: the primary considers a move confirmed only when the backup has acknowledged it, and only then
  tells the clients and writes the balances to the database (*output commit*). Every state a player has seen therefore
  exists on both replicas: after a failure of the primary no confirmed move is lost.

The cost is one round trip on the local network per move (latency traded for consistency even without partitions),
negligible compared with the pace of the game.

### 3. The lobby is the arbiter

Primary, backup and lobby form a group of three: a decision on the configuration of a table needs the agreement of at
least two of them (a majority).

- For every table the lobby keeps the current **configuration**: which node is the primary, which is the backup, and an
  **epoch** number that grows at every change of configuration.
- Every replication message carries the epoch of its sender; a node refuses the messages with an epoch older than its
  own. The epoch works as a *fencing token*: a deposed primary can no longer get anything accepted.
- No replica changes role on its own: promotions and exclusions are decided by the lobby.

### 4. Promotion and exclusion policy

| Situation | Who decides | Outcome |
|---|---|---|
| The lobby gets no heartbeat from the primary within the timeout, **and** the up-to-date backup receives nothing more from the primary | lobby and backup | the backup becomes primary, epoch + 1; the clients are redirected to the new primary |
| The lobby does not hear the primary, but the backup keeps receiving its replication (partition between primary and lobby) | no change | the primary is alive and replicating: the table goes on, the lobby waits for the primary to be reachable again |
| The backup does not answer the primary, and the lobby does not hear it either | lobby and primary | the primary goes on alone, epoch + 1, in degraded mode until a new backup is brought up to date |
| The backup does not answer the primary, but the lobby hears it (partition between the two replicas) | lobby and primary | the primary goes on, because it has the last confirmed state; the backup is excluded and must be brought up to date again |
| The primary reaches neither the backup nor the lobby | nobody | the primary refuses the moves: the table is paused (the CP choice) |
| The lobby is unreachable but primary and backup talk to each other | no change | the table goes on as usual: the lobby takes no part in the single moves |

In every row that changes the configuration, two participants out of three agree that the third one is unreachable:
that is the majority rule.

The backup can be promoted only if it was up to date with the latest epoch: an excluded backup, or one not yet
brought up to date, cannot become primary, because it does not have all the confirmed moves.

## Discarded alternatives

**AP (keep going on both replicas during a partition and reconcile afterwards).**
Discarded: there is no correct reconciliation of two diverging blackjack hands (cards already shown, bets already
paid). Accepting moves during the partition would mean undoing plays and payments afterwards.

**Asynchronous replication (the primary answers the clients at once and updates the backup later).**
Discarded: it is faster, but when the primary fails the last moves seen by the players might not have reached the
backup, which would resume the hand from an earlier state. It contradicts the requirement "the state of the backup
matches the primary's after every move".

**Active replication (both replicas execute the same moves).**
Discarded: the table is not deterministic. The deck is shuffled at random and the timeouts depend on each node's
clock, so the two replicas would diverge. Making them deterministic (a shared seed for the deck, timeouts turned into
messages) is possible, but more complex than transferring the state, which for a table is small.

**Automatic promotion of the backup when the primary's heartbeats stop, without an arbiter.**
Discarded: the backup cannot tell a crashed primary from a broken link between the two. During a partition there would
be two active primaries (*split brain*), exactly the divergence the CP choice wants to avoid.

**Full consensus (Raft/Paxos) on three or more replicas per table.**
Discarded: it is the most general solution, but it needs at least three nodes per table and a consensus algorithm to
implement and verify. Using the lobby as the third vote gives a majority with only two replicas per table, and there is
a single lobby for all the tables.

**Table state in the database (stateless tables).**
Discarded: every move would become a database write, so the database would be the bottleneck and the single point of
failure of every game. It would also still be necessary to make sure that a single node at a time writes the state of
a table.

## Consequences

- During a partition or a failure a table may stop for a few seconds: detection (heartbeat timeout), promotion and
  reconnection of the clients. That is the price of consistency.
- The lobby becomes necessary for configuration changes: if the lobby is down and a replica fails too, the table stays
  paused until the lobby is back. The lobby is not replicated: it remains a single point of failure for entering the
  tables and for the failover, but not for the hands in progress between two healthy replicas.
- When the clients lose the connection with the primary, they ask the lobby where the table is now and reconnect to the
  new primary with the same session token.
- The logical table keeps the same id after the failover, so the "sitting at this table" marks in the database stay
  valid. The new primary must therefore not free the players at start-up, as a table that starts from scratch does.
- Only the primary of the current epoch writes the balances to the database, and only for transitions already
  confirmed by the backup.
- An excluded replica can come back only after receiving the whole state from the current primary (resynchronisation).

## Failure detection (F3)

Every failure is detected with the rule already used by the lobby for the tables (E2): heartbeats every period, and a
peer is *suspected* when it has been silent for longer than the timeout (`HEARTBEAT_TIMEOUT_SECONDS`, 3 periods by
default, so one lost heartbeat is not enough). The detector (`FailureDetector`) only signals: promotion and the
behaviour under partition are decided by F4 and F5, which plug into its listener.

| Observer | Watches | Sign of life |
|---|---|---|
| lobby | primary | heartbeat to `/tables/heartbeat` (registry, E2) |
| lobby | backup | heartbeat to `/replicas/heartbeat` (the backup is not in the registry: it takes no players) |
| backup | primary | heartbeat to `/replica/heartbeat`, sent also when the table is idle and replicates nothing |
| primary | backup | the answer to that heartbeat |
| primary, backup | lobby | the answer to their heartbeats to the lobby |

A heartbeat from a primary with an older epoch is refused (409) and is not a sign of life. A signalled node that is
heard again is reported as reachable again. Worst-case detection time: the timeout plus one check period.

## Promotion of the backup (F4)

Implements the first row of the promotion table: the primary is silent for the lobby **and** for the backup.

1. The backup suspects its primary (F3) and, at every period, asks the lobby `POST /tables/promote` with
   `epoch = last epoch received + 1`.
2. The lobby (`PromotionArbiter`) grants it only if the table is marked unavailable in its registry (it does not hear the
   primary either: two of three agree) and the epoch is greater than the last one granted for that table. Otherwise 409:
   the primary is still alive for the lobby, so the backup is just cut off from it and nothing changes.
3. On the grant the backup closes its replica (it follows nobody: the old primary is refused, even with a newer epoch),
   starts the table from the last state it stored, opens the WebSocket port and announces itself to the lobby with its
   public address. Registering replaces the entry of the failed primary, so `/assign` sends the players to the new node:
   no client needs to know that a failover happened to be redirected.
4. The promoted table has no backup (degraded mode) until one is brought up to date (F5, F8).

Choices worth knowing:

- **The players of the failed primary are treated as disconnected** (as in C5): their hands stand, the hand is settled
  without waiting, each seat is freed at the end of the hand and the balance is saved. This keeps the database
  consistent (nobody stays marked as sitting at a table nobody plays at). Resuming the hand for a client that comes back
  is F7. If the cards had not been dealt yet, the hand is cancelled instead and the bets go back to the players: they
  are not to blame for the failure.
- The promoted node does **not** release the players at start-up, unlike a fresh table.
- A primary that comes back after the promotion (restart or healed partition) is fenced by the lobby: see "A replaced
  primary comes back" in the F5 section.
- Worst-case time to the new primary: timeout + one period for the lobby to notice + one period for the retry.

## Behaviour under partition (F5)

### The backup cannot be reached: the primary goes on alone

Rows 3 and 4 of the promotion table. While the backup does not acknowledge, every move is undone and the players are
asked to try again (F2): the table is paused. When the primary suspects its backup (F3), at every period it asks the
lobby `POST /tables/exclude-backup` with `epoch = its epoch + 1` (`BackupExcluder`). The lobby grants it if the epoch
is newer than the current one for the table: the primary, which asks, and the lobby, which hears it, are two of
three. From then on the table plays without waiting for the backup (degraded mode), with the new epoch.

Promotions and exclusions share the epochs kept by the lobby (`PromotionArbiter`), and this decides every race:

- When the two replicas lose each other, the backup asks for the promotion and the primary for the exclusion, both
  with the epoch of the primary + 1: at most one of the two can win. While the lobby hears the primary only the
  exclusion can win.
- An excluded backup still has the old epoch, so it can no longer be promoted, not even if the primary fails
  afterwards: it does not have the moves played since the exclusion.
- A primary cut off from the backup **and** from the lobby (row 5) stays paused; if in the meantime the backup has been
  promoted, the epoch the primary would ask for has already been used, so it can never go on alone.

### The excluded backup comes back: resynchronisation

The primary in degraded mode keeps sending its heartbeats to the backup, with its new epoch. The backup accepts them,
because the epoch is newer than its own, and answers. When the primary hears it again it takes it back
(`BackupExcluder`): at the next event of the table it sends the whole current state, and if the backup stores it the
table waits for the backup at every move again. If it does not, the table stays alone and tries again at the next
period.

No agreement of the lobby is needed for this: the whole state replaces whatever the backup had, and from that moment the
backup holds the current epoch, which is exactly what makes it eligible again for a promotion (its request carries the
current epoch + 1). If the two replicas lose each other again, the same race of the previous section starts, one epoch
higher.

### A replaced primary comes back: epochs from the lobby

Until now the epoch of a primary was the instant it started: a primary that restarted after the promotion of its backup
had a greater epoch than the promoted node, and the lobby would have let it go on alone and take the players back. Now
the lobby is the only source of the epochs, as the decision record says:

- **At start-up** a replicated table node asks the lobby `POST /tables/start` and waits, retrying at every period,
  until the lobby grants it the next epoch. The lobby refuses while the table is available in its registry, i.e. while
  another node (the promoted backup) plays it. Only once it has the epoch does the node free the players marked at the
  table in the database, so a primary that restarts can no longer free the players of the node that replaced it.
- At the first period the new primary sends its (empty) state to the backup, so that the backup knows the epoch even if
  nobody plays before a failure, and can be promoted.
- **Registrations and heartbeats carry the epoch** of a replicated table. The lobby answers 409 to an epoch older than
  the current one: a primary that was cut off and comes back cannot register again in place of the promoted node, and
  its first heartbeat tells it that it has been replaced. A refused exclusion (409) tells the same.
- **A replaced primary steps down**: it refuses every move ("the table is played on another node: ask the lobby"),
  closes the connections of its clients, which go back to the lobby, and writes nothing to the database, not even when
  it stops. In general a primary that stops writes the balances only if the backup has acknowledged the last state or
  the table is playing alone: otherwise the backup still has the players, and settles them if it takes over.
- The epochs of the lobby are soft state, like the registry: after a restart the lobby learns them again from the
  heartbeats of the primaries, which raise the current epoch of their table.

Tables that are not replicated (no backup) send no epoch and are never refused.

## Every table has its backup (F8)

The pool tables (`table-2`, `table-3`) are replicated like `table-1`: each one has a backup container named after it
(`table-2-backup`, `table-3-backup`), in the same profile, with its own published ports (health `918n`, WebSocket
`919n` after a promotion). When the lobby starts a table of the pool it starts its backup first, so that the table can
replicate from its first move; a backup that does not exist or cannot be started does not keep the table down: the table
is paused until the lobby lets it go on alone (F5), as if the backup had failed.

