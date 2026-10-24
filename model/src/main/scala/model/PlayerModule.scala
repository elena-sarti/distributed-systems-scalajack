package model

object PlayerModule:
  import utils.ModelExports.{Fiche, StandardCard, Participant}

  enum PlayerState:
    case Active
    case Busted
    case Standing
    case Blackjack

  /** Represents the player wallet during the game. */
  trait Wallet:

    private var currentBalance: List[Fiche] = List.empty

    /** Method to set the initial balance of a user*/
    protected def initializeBalance(amount: Double): Unit =
      currentBalance = Fiche.fromAmount(amount)

    /** The list of fiches currently owned by the player. */
    def balance: List[Fiche] =
      currentBalance

    /** Deposit a specific amount in the current balance by converting it into fiches.
     *
     * @param amount The value to be deposited.
     */
    def deposit(amount: Double): Unit =
      require(amount > 0)
      currentBalance = currentBalance ::: Fiche.fromAmount(amount)

    /** Withdraws the necessary fiches to cover the requested bet amount
     *
     * The method first attempts to withdraw extract value fiches starting from the largest.
     * If an exact combination is not found it attempts to use the smallest available
     * fiche that is greater than the remaining debt, returning the change to the balance.
     *
     * @param amount The total bet amount to be subtracted.
     * @return [[true]] if the player has enough fiches and the withdrawn succeeds, [[false]] otherwise,
     */
    def withdraw(amount: Double): Boolean =
      require(amount > 0, "withdraw amount must be greater than 0")

      def selectFichesToKeep(fiches: List[Fiche], target: Double): (List[Fiche], Double) =
        fiches.foldLeft((List.empty[Fiche], target)):
          case ((keptFiches, remainingAmount), f) if remainingAmount > 0 && f.value <= remainingAmount =>
            (keptFiches, remainingAmount - f.value)
          case ((keptFiches, remainingAmount), f) =>
            (f :: keptFiches, remainingAmount)

      val sortedFiches = currentBalance.sortBy(-_.value)
      val (keptFiches, remainingAmount) = selectFichesToKeep(sortedFiches, amount)
      if remainingAmount == 0 then
        currentBalance = keptFiches
        true
      else
        keptFiches.sortBy(_.value).find(_.value >= remainingAmount).exists: changeFiche =>
          val change = changeFiche.value - remainingAmount
          currentBalance = keptFiches.diff(List(changeFiche)) ::: (if change > 0 then Fiche.fromAmount(change) else Nil)
          true

  trait InsuranceSupport:
    /**
     * Returns whether the player has chosen the insurance option.
     *
     * @return `true` if the player has insurance, `false` otherwise.
     */
    def hasInsurance: Boolean

    /**
     * Updates the player's insurance status.
     *
     * @param value `true` if the player chooses insurance, `false` otherwise.
     */
    def hasInsurance_=(value: Boolean): Unit

  /** Represents a player at the game table.
   * Manages the player's current balance and state in the game.
   */
  trait Player(initialBalance: Double) extends Participant with Wallet:

    initializeBalance(initialBalance)
    private var currentState = PlayerState.Active

    /** The state of the player */
    def state: PlayerState =
      if score.minValue > 21 then
        PlayerState.Busted
      else
        currentState

    /** Changes the player's state to `Standing`. */
    def stand(): Unit =
      currentState = PlayerState.Standing

    /** Changes the player's state to `BlackJack`. */
    def winBlackjack(): Unit =
      currentState = PlayerState.Blackjack

    /** Changes the player's state to `Busted`(when exceeding 21). */
    def bust(): Unit =
      currentState = PlayerState.Busted

    /** Resets the player's state to `Active` to start a new round. */
    def prepareForNewHand(): Unit =
      currentState = PlayerState.Active
      clearHand()

    /** Prints a player in a format: [NAME] CARDS - SCORE - STATE */
    override def toString: String = super.toString + s"\nSTATE: $state\n"

  class NormalPlayer(override val name: String,
                     val initialBalance: Double) extends Player(initialBalance) with InsuranceSupport:

    private var insurance = false

    override def hasInsurance: Boolean =
      insurance

    override def hasInsurance_=(value: Boolean): Unit =
      insurance = value


  class SplitPlayer(override val name: String,
                    val splitCard: StandardCard,
                    val initialBalance: Double = 0) extends Player(initialBalance):

    addCard(splitCard)

  class BotPlayer(override val name: String,
                  val initialBalance: Double = BotPlayer.randomBalance,
                  var bet: Int = BotPlayer.randomBet) extends Player(initialBalance):

    private val StandingThreshold = 17

    override def toString: String = super.toString + s"BET: $bet\n"

    def computeSafeBet: Int =
      if bet > balance.totalValue then bet = balance.totalValue.toInt
      bet

    def hasFinishedTurn: Boolean = score.playableValue >= StandingThreshold

  object BotPlayer:
    private val MinBalance = 100
    private val MaxBalance = 500
    private val BalanceStep = 100
    private val MinBet = 10
    private val MaxBet = 50
    private val BetStep = 10

    private def getRandomNumber(min: Int, max: Int, step: Int): Int =
      min + scala.util.Random.nextInt((max - min) / step + 1) * step

    private def randomBalance: Double = getRandomNumber(MinBalance, MaxBalance, BalanceStep)

    private def randomBet: Int = getRandomNumber(MinBet, MaxBet, BetStep)


