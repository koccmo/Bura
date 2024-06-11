package com.bura.services

import com.bura.domain.{CardDesk, Human}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DealingCardsTest extends AnyFunSuite with Matchers {

  test("Dealing check for Two Players") {

    val desk = CardDesk()
    val humans = List(Human("Alex"),Human("John"))

    val dealingCards = DealingCards
    val dealtCard = dealingCards(humans, desk)

    val result = dealtCard.cardDesk.cards.size

    result shouldBe 30
  }
}
