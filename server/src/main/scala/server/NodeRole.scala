package server

enum NodeRole:
  case Lobby, Table


//Either[String, NodeRole]: come Option[A] ha come cases Just(a) e None, Either ha Left(error) e Right(nodeRole), dove error è di tipo
// String (mi dice la causa dell'errore), e nodeRole è di tipo NodeRole. Solitamente Left è configurato per il caso con errore mentre Right con il caso giusto
//E' più esplicativo di un Option perchè mi permette di portarmi dietro la causa di errori
object NodeRole:
  def parse(raw: String): Either[String, NodeRole] =
    raw.trim.toLowerCase match
      case "lobby" => Right(Lobby)
      case "table" => Right(Table)
      case other   => Left(s"Unknown role '$other' (expected: lobby | table)")