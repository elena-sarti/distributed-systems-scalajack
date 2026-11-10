package model

import utils.ModelExports.*
import utils.TestExports.*

class FicheTest extends AnyFunSuite:

  private val exactFiftyAmount = 50
  private val multiDenominationAmount = 85
  private val amountRequiringMultipleFifties = 280
  private val belowMinimumAmount = 0.25
  private val nonMultipleOfFiftyCentAmount = 6.25
  private val negativeAmount = -5
  private val nullAmount = 0

  test("fromAmount should convert an exact amount into a single fiche"):
    Fiche.fromAmount(exactFiftyAmount) shouldBe List(Fiche.Fifty)

  test("fromAmount should use the algorithm for amounts requiring multiple fiches"):
    Fiche.fromAmount(multiDenominationAmount) shouldBe List(Fiche.Fifty, Fiche.Twenty, Fiche.Ten, Fiche.Five)

  test("fromAmount should use multiple fiches of the same denomination when needed"):
    Fiche.fromAmount(amountRequiringMultipleFifties) shouldBe
      List(Fiche.Fifty, Fiche.Fifty, Fiche.Fifty, Fiche.Fifty, Fiche.Fifty, Fiche.Twenty, Fiche.Ten)

  test("the sum of a list of fiches should match the total value"):
    val totalValue = 75
    List(Fiche.Fifty, Fiche.Twenty, Fiche.Five).totalValue shouldBe totalValue

  test("fromAmount of zero should return an empty list"):
    Fiche.fromAmount(nullAmount) shouldBe List.empty

  test("fromAmount should correctly include FiftyCent fiches when the amount requires fractional denomination"):
    val notIntegerAmount = 10.5
    Fiche.fromAmount(notIntegerAmount) shouldBe List(Fiche.Ten, Fiche.FiftyCent)

  test("fromAmount with an amount below 50 cents should throw an exception"):
    an[IllegalArgumentException] should be thrownBy Fiche.fromAmount(belowMinimumAmount)

  test("fromAmount with an amount not a multiple of 50 cents should throw an exception"):
    an[IllegalArgumentException] should be thrownBy Fiche.fromAmount(nonMultipleOfFiftyCentAmount)

  test("fromAmount with a negative amount should throw an exception"):
    an[IllegalArgumentException] should be thrownBy Fiche.fromAmount(negativeAmount)
