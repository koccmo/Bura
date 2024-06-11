package com.bura.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CardDeskTest extends AnyFunSuite with Matchers {
  test("Check how work method Upgrade") {

    val desk: CardDesk = CardDesk()
    val listCard = desk.get(35)
    val upgradedDesk = desk.upgrade(listCard)

    listCard.size shouldBe 35
    upgradedDesk.cards.size shouldBe 1
  }


  test("Check if i need return Zero cards") {

    val desk: CardDesk = CardDesk()
    val listCard = desk.get(0)
    val upgradedDesk = desk.upgrade(listCard)

    listCard.size shouldBe 0
    upgradedDesk.cards.size shouldBe 36
  }

  test("Check if i need return more card then it have desc") {

    val desk: CardDesk = CardDesk()
    val listCard = desk.get(37)
    val upgradedDesk = desk.upgrade(listCard)

    listCard.size shouldBe 36
    upgradedDesk.cards.size shouldBe 0
  }
}
