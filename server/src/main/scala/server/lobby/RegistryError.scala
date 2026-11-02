package server.lobby

/** Why a change to the registry was refused. */
enum RegistryError:
  case UnknownTable(id: String)
  case InvalidTable(reason: String)
  case InvalidFreeSeats(free: Int, capacity: Int)