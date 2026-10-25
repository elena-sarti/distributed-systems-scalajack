package protocol

import upickle.default.{ReadWriter, macroRW, readwriter}
import utils.ModelExports.{PlayerState, StandardCard, Suit, Value}

/** JSON (de)serializers for the model types that travel inside the protocol messages.
 *
 * They live here, and not in the model, so that the model stays a pure domain module
 * with no knowledge of how (and whether) it is sent over the network.
 */
object ModelCodecs:

  // Simple enums (only singleton cases) are written as the plain name of the case, e.g. "Hearts".
  // `bimap` adapts the String serializer: toString on the way out, valueOf on the way in.
  // An unknown name makes valueOf throw, which MessageCodec turns into a Left (malformed message).
  given ReadWriter[Suit]        = readwriter[String].bimap[Suit](_.toString, Suit.valueOf)
  given ReadWriter[Value]       = readwriter[String].bimap[Value](_.toString, Value.valueOf)
  given ReadWriter[PlayerState] = readwriter[String].bimap[PlayerState](_.toString, PlayerState.valueOf)

  // A card is a plain product (suit, value, isFaceUp): `macroRW` generates its serializer at
  // compile time from its fields, using the Suit and Value serializers defined above.
  given ReadWriter[StandardCard] = macroRW
