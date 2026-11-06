package server.liveness

/** The other nodes a table node or a backup node depends on, and whose failure it must notice.
 * (The lobby watches the backups by table id, see `BackupHeartbeatEndpoint`.) */
enum Peer:
  case Primary, Backup, Lobby
