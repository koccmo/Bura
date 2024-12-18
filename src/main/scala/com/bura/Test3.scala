package com.bura

import com.bura.domain.Rank._
import com.bura.domain.Suit.{Clubs, Diamonds, Hearts}
import com.bura.domain._
import com.bura.services.{DealingCards, RobDefender}

object Test3 extends App {

  val cards = List(Card(Ace, Hearts), Card(Ace, Hearts), Card(Six, Hearts))

//  println(cards.map(_.suit).headOption)

  val defe: List[Int] = List(7, 11).sorted
  val att: List[Int]  = List.empty[Int].sorted

  val higher: Boolean = defe.zip(att).forall { case (defeNum, attNum) =>
    defeNum > attNum
  }

//  println(higher)

//  val okk: Boolean = {
//    val kkk = defe.map{x => att.forall(_ < x)}.count(_ == true)
//
//    kkk >= att.size
//  }
//
//  println(okk)

  val cardDesk    = CardDesk(CardDesk.cardsList, Some(Card(Ace, Hearts)))
  val attackCards = List(Card(Ten, Hearts), Card(Seven, Hearts))
  val defend      = List(Card(King, Clubs), Card(Ten, Diamonds), Card(Nine, Hearts))
  val robot       = Robot().setHand(defend)

  val rDefender = RobDefender
  println(s"Trump = ${cardDesk.trump}")
  println(rDefender.defend(robot, attackCards, cardDesk))

  println("Check defend")

  val robN                  = Robot()
  val human                 = Human("Rob")
  val defendRobot: Robot    = Robot().setAttack(false)
  val cardDeskN             = CardDesk()
//  val players = List(robN, human)
  val players: List[Player] = List(robN, defendRobot)
  val dealtCards            = DealingCards(players, cardDeskN)

  val attackPlayer = dealtCards.players.collect {
    case robot @ Robot(_, _, _, _, _, true) => robot
    case human @ Human(_, _, _, _, _, true) => human
  }

//  val attackedCard = attackPlayer.flatMap {
//    case human: Human => Attack.attack(human, cardDesk)
//    case robot: Robot => Attack.attack(robot, cardDesk)
//  }
//  val attackedCard = attackPlayer.flatMap(x => Attack.attack(x, cardDesk))

//  val answer = attackPlayer.foreach {Attack.attack(_, dealtCards.cardDesk)}

  val defender = dealtCards.players.find(!_.attack) match {
    case Some(person) => person
  }

  //  val attackedPlayersCards: List[Card] = Attack.attack(attackPlayer, dealtCards.cardDesk)
//  val defenderCards: BeatDiscard = RobDefend.defend(attackedCard, defender, dealtCards.cardDesk)
//
//  println(s"Trump -> ${dealtCards.cardDesk.trump}")
//  println(s" Attacker Hands Cards -> $attackPlayer")
//  println(s" Defender Hands Cards -> ${defender.hand}")
//  println(s"Attacker cards to Attack -> $attackedCard")
//  println(s"Defender cards to Defend -> $defenderCards.")
//  println(Attack.attack(robN, dealtCards.cardDesk))

}
