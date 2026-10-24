package model

object ParticipantModule:
  import utils.ModelExports.{StandardCard, Score, calculateScore}

  trait Participant:
    private var currentCards: List[StandardCard] = List.empty

    /** The name of the participant. */
    def name: String

    /** The cards in the hand of a player. */
    def cards: List[StandardCard] = currentCards

    /** Clears all the cards currently held by the participant. */
    def clearHand(): Unit = currentCards = List.empty

    /** Replaces the current cards held by the participant with a new list of cards.
     *
     * @param newCards the new list of cards to assign to the participant.
     */
    protected def setCards(newCards: List[StandardCard]): Unit =
      currentCards = newCards

    /** Adds a card to the list of cards of a participant */
    def addCard(standardCard: StandardCard): Unit =
      currentCards = cards :+ standardCard

    /** The score of a participant during a hand. */
    def score: Score = currentCards.calculateScore

    /** The textual representation of the score. Defaults to the full `Score`
     * representation (which may show both readings when there's an ace).
     * */
    protected def displayScore: String = score.toString

    /** Returns a string representation of the player. */
    override def toString: String =
      val cardsLines: List[Array[String]] = cards.map(_.toString.linesIterator.toArray)
      val topRow = cardsLines.map(lines => lines(0)).mkString("  ")
      val middleRow = cardsLines.map(lines => lines(1)).mkString("  ")
      val bottomRow = cardsLines.map(lines => lines(2)).mkString("  ")
      s"[$name]:\n$topRow\n$middleRow\n$bottomRow\nSCORE: $displayScore"