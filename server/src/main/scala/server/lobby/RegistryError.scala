package server.lobby

/** Why a change to the registry was refused. */
enum RegistryError:
  /** No table has this id. */
  case UnknownTable(id: String)
  /** The id, the host or the port of the table is not valid. */
  case InvalidTable(reason: String)
  /** The free seats are not between 0 and the capacity. */
  case InvalidFreeSeats(free: Int, capacity: Int)