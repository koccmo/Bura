package com.bura.services

import com.bura.domain.{Card, CardDesk, Human}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PlayerAttackTest extends AnyFunSuite with Matchers {

  test("firstCheck") {
    val cardDesk = CardDesk().doTrump
    val cards: List[Card] = cardDesk.get(3)
    val upgratedDesk = cardDesk.upgrade(cards)
    val human = Human("Alex").setHand(cards)
    val result = PlayerAttack.attack(human, upgratedDesk)

    println(result)
  }
}
