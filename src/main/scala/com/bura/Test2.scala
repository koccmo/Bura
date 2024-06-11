package com.bura

object Test2 extends App {

//  val cardDesk = CardDesk().doTrump
//  val cards: List[Card] = cardDesk.get(3)
//  val upgratedDesk = cardDesk.upgrade(cards)
//  val human = Human("Alex").setHand(cards)
//  val result = PlayerAttack.attack(human, upgratedDesk)
//
//  println(result)
  println("----")
  case class Pop(name: String, point: Int)
  val list = List(Pop("a", 2), Pop("g", 1), Pop("p", 1))
  println(list.sortBy(_.point).take(2))
}
