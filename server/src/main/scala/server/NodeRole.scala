package server

/** What a node does. The lobby assigns the players to the tables, a table runs one table and a backup keeps a copy
 * of a table and takes it over if the primary fails. */
enum NodeRole:
  case Lobby, Table, Backup

object NodeRole:
  /** The role named by the text, ignoring case and surrounding spaces. */
  def parse(raw: String): Either[String, NodeRole] =
    raw.trim.toLowerCase match
      case "lobby"  => Right(Lobby)
      case "table"  => Right(Table)
      case "backup" => Right(Backup)
      case other    => Left(s"Unknown role '$other' (expected: lobby | table | backup)")