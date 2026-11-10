package model

object FicheModule:

  enum Fiche:
    case FiftyCent, Two, Five, Ten, Twenty, Fifty

  object Fiche:
    private val denominations: List[Fiche] = Fiche.values.toList.sortBy(-_.value)

    val smallestDenomination: Double = denominations.map(_.value).min

    def fromAmount(amount: Double): List[Fiche] =
      require(amount >= 0, "amount cannot be negative")
      require(amount % smallestDenomination == 0, s"amount must be a multiple of $smallestDenomination")
      denominations.foldLeft((amount, List.empty[Fiche])):
        case ((remaining, acc), fiche) =>
          val count: Int = (remaining / fiche.value).toInt
          (remaining - count * fiche.value, acc ::: List.fill(count)(fiche))
      ._2

    extension (f: Fiche)
      def value: Double = f match
        case FiftyCent => 0.5
        case Two       => 2
        case Five      => 5
        case Ten       => 10
        case Twenty    => 20
        case Fifty     => 50

    extension (fiches: List[Fiche])
      def totalValue: Double = fiches.map(_.value).sum